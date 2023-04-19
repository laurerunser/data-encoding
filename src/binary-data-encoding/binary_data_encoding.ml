(* mostly module aliases, but also splitting the high-level (encoding-based) and
   low-level (descr-based) parts of the API *)

module Sizability = Sizability
module Descr = Descr

module Query = struct
  include Public.Query
  module Of_descr = Query
end

module Encoding = Encoding

module Reader = struct
  include Public.Reader
  module Of_descr = Reader
end

module Writer = struct
  include Public.Writer
  module Of_descr = Writer
end
