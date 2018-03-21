module Wrap(T : sig type t end) =
struct
  type t = T.t
  let wrap x = x
  let unwrap x = x
end
