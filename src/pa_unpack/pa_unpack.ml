open Camlp4

module Id : Sig.Id = struct
  let name = "pa_unpack"
  let version = "0.1"
end

module Make (AstFilters : Sig.AstFilters) = struct
  open AstFilters
  module Expander = Expander.Make(AstFilters)

  let expander = object (o)
    inherit Ast.map as super

    method! expr = function
      | <:expr@loc<unpack $str:template$>> ->
          Expander.unpack loc template
      | <:expr@loc<pack $str:template$>> ->
          Expander.pack loc template
      | e -> super#expr e
  end;;

  AstFilters.register_str_item_filter expander#str_item
end

module M = Register.AstFilter(Id)(Make)
