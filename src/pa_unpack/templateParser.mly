%{
  open Template

  let identity x = x
%}

%token EOF

%token<int> TOK_INTEGER

%token TOK_a

%token TOK_c
%token TOK_C

%token TOK_s
%token TOK_S

%token TOK_l
%token TOK_L

%token TOK_q
%token TOK_Q

%token TOK_i
%token TOK_I

%token TOK_n
%token TOK_N
%token TOK_v
%token TOK_V

%token TOK_w

%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_LBRACK
%token TOK_RBRACK

%token TOK_STAR

%token TOK_SLASH


%start<Template.conversions> template

%%
template:
	| conversions EOF				{ List.rev $1 }


conversions:
	| conversion					{ [$1] }
	| conversions conversion			{ $2 :: $1 }


conversion:
	| conversion_expr				{ $1 }
	| conversion_atom TOK_SLASH conversion_expr	{ Repeat ($1, $3) }


conversion_expr:
	| conversion_atom				{ $1 }
	| TOK_LPAREN conversions TOK_RPAREN repeat	{ $4 (Grouping (List.rev $2)) }


conversion_atom:
	| TOK_a repeat					{ $2 Char }

	| TOK_c repeat					{ $2 SInt8 }
	| TOK_C repeat					{ $2 UInt8 }

	| TOK_s repeat					{ $2 SInt8 }
	| TOK_S repeat					{ $2 UInt8 }

	| TOK_l repeat					{ $2 SInt8 }
	| TOK_L repeat					{ $2 UInt8 }

	| TOK_q repeat					{ $2 SInt8 }
	| TOK_Q repeat					{ $2 UInt8 }

	| TOK_i repeat					{ $2 SInt }
	| TOK_I repeat					{ $2 UInt }

	| TOK_n repeat					{ $2 Net16 }
	| TOK_N repeat					{ $2 Net32 }
	| TOK_v repeat					{ $2 VAX16 }
	| TOK_V repeat					{ $2 VAX32 }

	| TOK_w repeat					{ $2 BER }


repeat:
	| /* empty */					{ identity }
	| TOK_STAR					{ fun x -> List x }
	| TOK_INTEGER					{ fun x -> FixedRepeat ($1, x) }
	| TOK_LBRACK TOK_INTEGER TOK_RBRACK		{ fun x -> FixedRepeat ($2, x) }
