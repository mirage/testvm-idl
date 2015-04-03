(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let (>>=) = Lwt.bind

module IO = functor (F : V1_LWT.FLOW) -> 
struct
  type 'a t = 'a Lwt.t

  let (>>=) = Lwt.bind
  let (>>) m n = m >>= fun _ -> n
  let return = Lwt.return
  let fail = Lwt.fail

  type channel = {
    flow: F.flow;
    mutable segment: Cstruct.t option;
  }

  let rec read_into channel buf off len =
    if len = 0
    then return ()
    else
      let read_more () =
        F.read channel.flow >>= function
        | `Ok segment ->
          channel.segment <- Some segment;
          read_into channel buf off len
        | `Eof -> fail End_of_file
        | `Error _ -> fail (Failure "Unhandled error reading from FLOW") in
      match channel.segment with
      | None -> read_more ()
      | Some segment ->
        if Cstruct.len segment = 0
        then read_more ()
        else begin
          let available = min (Cstruct.len segment) len in
          Cstruct.blit_to_string segment 0 buf off available;
          channel.segment <- Some (Cstruct.shift segment available);
          read_into channel buf (off + available) (len - available)
        end

  let write_from channel buf off len =
    let segment = Cstruct.create len in
    Cstruct.blit_from_string buf off segment 0 len;
    F.write channel.flow segment >>= function
    | `Ok () -> return ()
    | `Eof -> fail End_of_file
    | `Error _ -> fail (Failure "Unhandled error writing to FLOW")

  type ic = channel
  type oc = channel
  type conn = unit

  let iter fn x = Lwt_list.iter_s fn x

  let read_line ic =
    let rec inner s =
      let buf = String.create 1 in
      read_into ic buf 0 1 >>= fun _ ->
      if buf.[0] = '\n' 
      then begin 
        if s.[String.length s - 1] = '\r'
        then return (Some (String.sub s 0 (String.length s - 1)))
        else return (Some s) end
      else inner (s ^ buf) 
    in inner ""

  let read ic n =
    let buf = String.create n in
    read_into ic buf 0 n >>= fun () ->
    return buf

  let read_exactly ic len =
    read ic len >>= fun x -> Lwt.return (Some x)

  let write oc s =
    lwt _ = write_from oc s 0 (String.length s) in Lwt.return ()

  let flush _ = Lwt.return ()
end

module Make ( V : V1_LWT.FLOW ) = struct
  module IO = IO(V)
  module Request = Cohttp.Request.Make(IO)
  module Response = Cohttp.Response.Make(IO)

  let openflow flow = { IO.flow; segment = None }

  let close channel =
    Lwt.return () (* unimplemented in VCHAN and FLOW *)

  let rpc string_of_call response_of_string vch call =
    let uri = Uri.of_string "vchan://" in
    let req = string_of_call call in
    let headers = Cohttp.Header.of_list [
        "User-agent", "vchan_client";
        "content-length", string_of_int (String.length req);
      ] in

    let http_req = Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers uri in
    lwt _ = Request.write (fun writer -> Request.write_body writer req) http_req vch in
    lwt response = Response.read vch in
    match response with
    | `Eof -> Lwt.fail (Failure (Printf.sprintf "Failed to read HTTP response"))
    | `Invalid s -> Lwt.fail (Failure (Printf.sprintf "Failed to read HTTP response: %s" s))
    | `Ok t ->
      begin match Cohttp.Response.status t with
        | `OK ->
          let reader = Response.make_body_reader t vch in
          lwt body = 
            lwt chunk = Response.read_body_chunk reader in
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
              IO.read_into vch request_txt 0 content_length >>= fun () ->
              let rpc_call = call_of_string request_txt in
              Printf.printf "%s" (Rpc.string_of_call rpc_call);
              lwt rpc_response = process ctx rpc_call in
              Printf.printf "   %s" (Rpc.string_of_response rpc_response);
              let response_txt = string_of_response rpc_response in
              let content_length = Int64.of_int (String.length response_txt) in
              let headers = Cohttp.Header.of_list [
                  "user-agent", "vchan";
                  "content-length", Int64.to_string content_length;
                ] in
              let response = Cohttp.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers ~encoding:(Cohttp.Transfer.Fixed content_length) () in
              Response.write (fun writer -> Response.write_body writer response_txt) response vch
          end
        | _, _ ->
          let content_length = 0L in
          let headers = Cohttp.Header.of_list [
              "user-agent", "vchan";
              "content-length", Int64.to_string content_length;
            ] in
          let response = Cohttp.Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers ~encoding:(Cohttp.Transfer.Fixed content_length) () in
          Response.write (fun writer -> Lwt.return ()) response vch
      end

end



