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

external shutdown : unit -> unit = ""
external reboot : unit -> unit = ""
external crash : unit -> unit = ""

type vbdid = string

module Vbd = struct
  external write_sector : vbdid -> int64 -> string -> unit = ""
  external read_sector : vbdid -> int64 -> string = ""
  external list : unit -> vbdid list = ""

  external start_hammer : vbdid -> unit = ""
  external stop_hammer : vbdid -> unit = ""

  external start_tickle : vbdid -> unit = ""
  external stop_tickle : vbdid -> unit = ""

  external start_junk_writer : vbdid -> int -> unit = ""
  external stop_junk_writer : vbdid -> bool = ""
end

type vifid = string

module Vif = struct
  external list : unit -> vifid list = ""
  external get_ipv4 : vifid -> string = ""
  external inject_packet : vifid -> string -> unit = ""
end
