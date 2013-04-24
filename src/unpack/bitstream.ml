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

let remaining stream =
  String.length stream.data - stream.pos
