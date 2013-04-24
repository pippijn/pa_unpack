{
  open TemplateParser
}


rule token = parse
| [' ']+		{ token lexbuf }

| ['0'-'9']+ as i	{ TOK_INTEGER (int_of_string i) }

| 'a'			{ TOK_a }

| 'c'			{ TOK_c }
| 'C'			{ TOK_C }

| 's'			{ TOK_s }
| 'S'			{ TOK_S }

| 'l'			{ TOK_l }
| 'L'			{ TOK_L }

| 'q'			{ TOK_q }
| 'Q'			{ TOK_Q }

| 'i'			{ TOK_i }
| 'I'			{ TOK_I }

| 'n'			{ TOK_n }
| 'N'			{ TOK_N }
| 'v'			{ TOK_v }
| 'V'			{ TOK_V }

| 'w'			{ TOK_w }

| '('			{ TOK_LPAREN }
| ')'			{ TOK_RPAREN }
| '['			{ TOK_LBRACK }
| ']'			{ TOK_RBRACK }

| '*'			{ TOK_STAR }
| '/'			{ TOK_SLASH }

| eof			{ EOF }


{
  let to_string = function
    | EOF -> "EOF"

    | TOK_INTEGER i -> "TOK_INTEGER " ^ string_of_int i

    | TOK_a -> "TOK_a"

    | TOK_c -> "TOK_c"
    | TOK_C -> "TOK_C"

    | TOK_s -> "TOK_s"
    | TOK_S -> "TOK_S"

    | TOK_l -> "TOK_l"
    | TOK_L -> "TOK_L"

    | TOK_q -> "TOK_q"
    | TOK_Q -> "TOK_Q"

    | TOK_i -> "TOK_i"
    | TOK_I -> "TOK_I"

    | TOK_n -> "TOK_n"
    | TOK_N -> "TOK_N"
    | TOK_v -> "TOK_v"
    | TOK_V -> "TOK_V"

    | TOK_w -> "TOK_w"

    | TOK_LPAREN -> "TOK_LPAREN"
    | TOK_RPAREN -> "TOK_RPAREN"
    | TOK_LBRACK -> "TOK_LBRACK"
    | TOK_RBRACK -> "TOK_RBRACK"

    | TOK_STAR -> "TOK_STAR"
    | TOK_SLASH -> "TOK_SLASH"
}
