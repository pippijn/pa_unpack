let rec list l p s =
  if Bitstream.is_empty s then
    l
  else
    list (p s :: l) p s

let list p s =
  List.rev (list [] p s)


let rec repeat c l p s =
  if c == 0 then
    l
  else
    repeat (c - 1) (p s :: l) p s

let repeat c p s =
  repeat c [] p s


let uint8 stream =
  Char.code (Bitstream.next stream)


let sint8 stream =
  let result =
    match uint8 stream with
    | s when s > 0x7f ->
        (lnot s) land 0xff - 1
    | u ->
        u
  in

  result


let net16 stream =
  let open Bitstream in
  let value = 0
    lor (Char.code stream.data.[stream.pos + 0] lsl (1 * 8))
    lor (Char.code stream.data.[stream.pos + 1] lsl (0 * 8))
  in

  stream.pos <- stream.pos + 2;
  value


(* XXX: actually this reads int, which may be 31 or 63 bits. *)
let net32 stream =
  let open Bitstream in
  let value = 0
    lor (Char.code stream.data.[stream.pos + 0] lsl (3 * 8))
    lor (Char.code stream.data.[stream.pos + 1] lsl (2 * 8))
    lor (Char.code stream.data.[stream.pos + 2] lsl (1 * 8))
    lor (Char.code stream.data.[stream.pos + 3] lsl (0 * 8))
  in

  stream.pos <- stream.pos + 4;
  value


let rec ber value stream =
  let digit = Char.code (Bitstream.next stream) in
  let value = value lor (digit land 0x7f) in

  if digit land 0x80 == 0 then
    value
  else
    ber (value lsl 7) stream


let ber stream =
  ber 0 stream


let char stream =
  Bitstream.next stream

let string stream =
  Bitstream.take (Bitstream.remaining stream) stream

let cstring count stream =
  Bitstream.take count stream
