open Camlp4

module Make (AstFilters : Sig.AstFilters) = struct
  open AstFilters
  open Template

  let parse template =
    let lexbuf = Lexing.from_string template in
    try
      TemplateParser.template TemplateLexer.token lexbuf
    with
    | TemplateParser.StateError (token, state) ->
        print_endline ("syntax error near " ^ TemplateLexer.to_string token);
        failwith "parse error"


  let rec generate_pack _loc = function
    | ast ->
        failwith (Sexplib.Sexp.to_string_hum (Template.sexp_of_conversion ast))


  let pack _loc template =
    let ast = parse template in

    let fcall, _ =
      List.fold_left (fun (fcall, next) _ ->
        <:expr<$fcall$ $lid:"_" ^ string_of_int next$>>, next + 1
      ) (<:expr<f>>, 0) ast
    in

    let code, _ =
      List.fold_left (fun (code, next) conv ->
        <:expr<
          let $lid:"_" ^ string_of_int next$ =
            $generate_pack _loc conv$
          in
          $code$
        >>, next + 1
      ) (fcall, 0) ast
    in

    code


  let rec generate_unpack _loc = function
    | BER	-> <:expr<Unpack.ber>>
    | Char	-> <:expr<Unpack.char>>
    | UInt8	-> <:expr<Unpack.uint8>>
    | SInt8	-> <:expr<Unpack.sint8>>
    | UInt16	-> <:expr<Unpack.uint16>>
    | SInt16	-> <:expr<Unpack.sint16>>
    | UInt32	-> <:expr<Unpack.uint32>>
    | SInt32	-> <:expr<Unpack.sint32>>
    | UInt64	-> <:expr<Unpack.uint64>>
    | SInt64	-> <:expr<Unpack.sint64>>
    | UInt	-> <:expr<Unpack.uint>>
    | SInt	-> <:expr<Unpack.sint>>
    | Net16	-> <:expr<Unpack.net16>>
    | Net32	-> <:expr<Unpack.net32>>
    | VAX16	-> <:expr<Unpack.vax16>>
    | VAX32	-> <:expr<Unpack.vax32>>

    | List Char ->
        <:expr<Unpack.string>>

    | List conv ->
        <:expr<Unpack.list $generate_unpack _loc conv$>>

    | FixedRepeat (count, Char) ->
        <:expr<Unpack.cstring $`int:count$>>
    | FixedRepeat (count, conv) ->
        <:expr<Unpack.repeat $`int:count$ $generate_unpack _loc conv$>>

    | Repeat (count, Char) ->
        <:expr<
          let count = $generate_unpack _loc count$ s in
          Unpack.cstring count
        >>

    | Repeat (count, conv) ->
        <:expr<
          let count = $generate_unpack _loc count$ s in
          Unpack.repeat count $generate_unpack _loc conv$
        >>

    | Grouping list ->
        let members, _ =
          List.fold_left (fun (members, next) _ ->
            <:expr<$lid:"_" ^ string_of_int next$>> :: members, next - 1
          ) ([], List.length list) list
        in

        let tuple =
          match members with
          | [member] -> member
          | members  -> Ast.(ExTup (_loc, exCom_of_list members))
        in

        let code = unpack_of_list _loc tuple list in

        <:expr<fun s -> $code$>>


  and unpack_of_list _loc code list =
    let code, _ =
      List.fold_right (fun conv (code, next) ->
        <:expr<
          let $lid:"_" ^ string_of_int next$ =
            $generate_unpack _loc conv$ s
          in
          $code$
        >>, next - 1
      ) list (code, List.length list)
    in

    code


  let unpack _loc template =
    let ast = parse template in

    let fcall, _ =
      List.fold_left (fun (fcall, next) _ ->
        <:expr<$fcall$ $lid:"_" ^ string_of_int next$>>, next + 1
      ) (<:expr<f>>, 1) ast
    in

    let code = unpack_of_list _loc fcall ast in

    <:expr<fun f s -> $code$>>

end
