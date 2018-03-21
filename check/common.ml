module Id(T : sig type t end) : sig
  type t
  val wrap : T.t -> t
  val unwrap : t -> T.t
end =
struct
  type t = T.t
  let wrap x = x
  let unwrap x = x
end

(* ES types *)
module Long = struct type t = int end
module Double = struct type t = float end
module Keyword = String
module Text = String
module Date = String
module Boolean = struct type t = bool end
module Ip = String
