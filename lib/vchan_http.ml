
let (>>=) = Lwt.bind

module type VCHAN =
  sig
    type t
    val read_into : t -> string -> int -> int -> int Lwt.t
    val write_from : t -> string -> int -> int -> int Lwt.t
  end

module IO = functor (V : VCHAN) -> 
struct
  type 'a t = 'a Lwt.t
    
  let (>>=) = Lwt.bind
  let (>>) m n = m >>= fun _ -> n
  let return = Lwt.return

  type ic = V.t
  type oc = V.t
    
  let iter fn x = Lwt_list.iter_s fn x
    
  let read_line ic =
    let rec inner s =
      let buf = String.create 1 in
      V.read_into ic buf 0 1 >>= fun _ ->
      if buf.[0] = '\n' 
      then begin 
        if s.[String.length s - 1] = '\r'
        then return (Some (String.sub s 0 (String.length s - 1)))
        else return (Some s) end
      else inner (s ^ buf) 
    in inner ""

  let read ic n =
    let buf = String.create n in
    let rec inner left =
      if left = 0 then return buf else begin
	V.read_into ic buf (n-left) left >>= fun x ->
	Printf.fprintf stderr "read %d '%s'\n%!" x (String.sub buf (n-left) x);
	inner (left - x)
      end
    in inner n

  let read_exactly ic len =
    read ic len >>= fun x -> Lwt.return (Some x)

  let write oc s =
    lwt _ = V.write_from oc s 0 (String.length s) in Lwt.return ()

  let flush _ = Lwt.return ()
end

module Make ( V : VCHAN ) = struct
  module IO = IO(V)
  module Request = Cohttp.Request.Make(IO)
  module Response = Cohttp.Response.Make(IO)

  let rpc string_of_call response_of_string vch call =
    let uri = Uri.of_string "vchan://" in
    let req = string_of_call call in
    let headers = Cohttp.Header.of_list [
        "User-agent", "vchan_client";
        "content-length", string_of_int (String.length req);
      ] in

    let http_req = Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers uri in
    lwt _ = Request.write (fun t vch -> Request.write_body t vch req) http_req vch in
    lwt response = Response.read vch in
    match response with
    | `Eof -> Lwt.fail (Failure (Printf.sprintf "Failed to read HTTP response"))
    | `Invalid s -> Lwt.fail (Failure (Printf.sprintf "Failed to read HTTP response: %s" s))
    | `Ok t ->
      begin match Cohttp.Response.status t with
        | `OK ->
          lwt body = 
          lwt chunk = Response.read_body_chunk t vch in
          match chunk with 
          | Cohttp.Transfer.Chunk body
          | Cohttp.Transfer.Final_chunk body -> Lwt.return body
          | _ -> Lwt.return "" in
          Lwt.return (response_of_string body)
        | bad -> Lwt.fail (Failure (Printf.sprintf "Unexpected HTTP response code: %s" (Cohttp.Code.string_of_status bad)))
      end 

  module RpcM = struct
    let vch = ref None 
    type 'a t = 'a Lwt.t
    let bind = Lwt.bind
    let return = Lwt.return
    let handle_failure = Lwt.catch
    let fail = Lwt.fail
    let rpc call =
      let Some v = !vch in
      lwt result = rpc Jsonrpc.string_of_call Jsonrpc.response_of_string v call in
    Lwt.return result
  end

  let http_handler call_of_string string_of_response process vch ctx =
    match_lwt Request.read vch with
    | `Eof ->
      Printf.printf "Failed to read HTTP request"; Lwt.return ()
    | `Invalid s ->
      Printf.printf "Invalid: %s" s; Lwt.return ()
    | `Ok req ->
      begin match Cohttp.Request.meth req, Uri.path (Cohttp.Request.uri req) with
      | `POST, _ ->
        let headers = Cohttp.Request.headers req in
        begin match Cohttp.Header.get headers "content-length" with
        | None ->
	  Printf.printf "Failed to read content-length"; Lwt.return ()
        | Some content_length ->
          Printf.printf "Read request headers: content_length=%s" content_length;
	  let content_length = int_of_string content_length in
	  let request_txt = String.make content_length '\000' in
	  lwt _ = 
            let rec inner n =
              lwt m = V.read_into vch request_txt n (content_length - n) in
              if m = (content_length - n) then Lwt.return () else inner (n+m)
            in inner 0
          in
	  let rpc_call = call_of_string request_txt in
	  Printf.printf "%s" (Rpc.string_of_call rpc_call);
	  lwt rpc_response = process ctx rpc_call in
	  Printf.printf "   %s" (Rpc.string_of_response rpc_response);
	  let response_txt = string_of_response rpc_response in
	  let content_length = String.length response_txt in
	  let headers = Cohttp.Header.of_list [
	    "user-agent", "vchan";
	    "content-length", string_of_int content_length;
	  ] in
	  let response = Cohttp.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers ~encoding:(Cohttp.Transfer.Fixed content_length) () in
	  Response.write (fun t vch -> Response.write_body t vch response_txt) response vch
        end
      | _, _ ->
        let content_length = 0 in
        let headers = Cohttp.Header.of_list [
	  "user-agent", "vchan";
  	  "content-length", string_of_int content_length;
        ] in
        let response = Cohttp.Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers ~encoding:(Cohttp.Transfer.Fixed content_length) () in
        Response.write (fun t vch -> Lwt.return ()) response vch
      end

end



