module CharStream = struct
  type t = {
    mutable pos : int;
    data : string;
  }

  let of_string data =
    { data; pos = 0 }

  let is_empty { pos; data } =
    pos == String.length data

  let next stream =
    stream.pos <- stream.pos + 1;
    stream.data.[stream.pos - 1]

  let take n stream =
    let s = String.sub stream.data stream.pos n in
    stream.pos <- stream.pos + n;
    s

  let size stream =
    String.length stream.data - stream.pos

end


module Unpack = struct
  let rec list l p s =
    if CharStream.is_empty s then
      l
    else
      list (p s :: l) p s

  let list p s =
    list [] p s


  let rec repeat c l p s =
    if c == 0 then
      l
    else
      repeat (c - 1) (p s :: l) p s

  let repeat c p s =
    repeat c [] p s


  let uint8 stream =
    int_of_char (CharStream.next stream)


  let net16 stream =
    let open CharStream in
    let value = 0
      lor (int_of_char stream.data.[stream.pos + 0] lsl (1 * 8))
      lor (int_of_char stream.data.[stream.pos + 1] lsl (0 * 8))
    in

    stream.pos <- stream.pos + 2;
    value


  (* XXX: actually this reads int, which may be 31 or 63 bits. *)
  let net32 stream =
    let open CharStream in
    let value = 0
      lor (int_of_char stream.data.[stream.pos + 0] lsl (3 * 8))
      lor (int_of_char stream.data.[stream.pos + 1] lsl (2 * 8))
      lor (int_of_char stream.data.[stream.pos + 2] lsl (1 * 8))
      lor (int_of_char stream.data.[stream.pos + 3] lsl (0 * 8))
    in

    stream.pos <- stream.pos + 4;
    value


  let rec ber value stream =
    let digit = int_of_char (CharStream.next stream) in
    let value = value lor (digit land 0x7f) in

    if digit land 0x80 == 0 then
      value
    else
      ber (value lsl 7) stream


  let ber stream =
    ber 0 stream


  let char stream =
    CharStream.next stream

  let string stream =
    CharStream.take (CharStream.size stream) stream

  let cstring count stream =
    CharStream.take count stream

end

Printexc.record_backtrace true;;

let () =
  let s = CharStream.of_string "\006abcdefgh" in
  unpack "w/a (aa)" (fun data (c1, c2) ->
    (* prints "abcdef g h" *)
    Printf.printf "%s %c %c\n"
      data c1 c2
  ) s


let () =
  let s = CharStream.of_string "\006abcdefgh" in
  unpack "w/a (aa)*" (fun data [(c1, c2)] ->
    (* prints "abcdef g h" *)
    Printf.printf "%s %c %c\n"
      data c1 c2
  ) s


let () =
  let s = CharStream.of_string "\006abcdefgh" in
  unpack "C/a a2" (fun data s ->
    (* prints "abcdef gh" *)
    Printf.printf "%s %s\n"
      data s
  ) s


let () =
  let s = CharStream.of_string "\006abcdefgh" in
  unpack "C/a a*" (fun data s ->
    (* prints "abcdef gh" *)
    Printf.printf "%s %s\n"
      data s
  ) s


let () =
  let s = CharStream.of_string "\006abcdefgh" in
  unpack "w/a aa" (fun data c1 c2 ->
    (* prints "abcdef g h" *)
    Printf.printf "%s %c %c\n"
      data c1 c2
  ) s
