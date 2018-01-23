module Texture :
  sig
    val create_from_ba : int -> int -> (int, Bigarray.int8_unsigned_elt) Tgl4.Gl.bigarray -> int
  end

