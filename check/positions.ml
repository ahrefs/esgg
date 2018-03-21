open Common

module Country = Wrap(String)
module Url_hash = struct
  module Exact_only = Wrap(Int64)
end
