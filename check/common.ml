module Id_(T : sig type t end) : sig
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
module Long_ = struct type t = int end
module Int64_ = struct type t = int64 end
module Double_ = struct type t = float end
module Keyword_ = String
module Text_ = String
module Date_ = String
module Boolean_ = struct type t = bool end
module Ip_ = String
