install Syntax ".DEFAULT" [
  (* Target *)
  Name		"pa_unpack";
  Description	"Perl-style type-safe pack/unpack";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Expander";
    "Pa_unpack";
    "Template";
    "TemplateLexer";
    "TemplateParser";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "camlp4.extend";
    "camlp4.quotations";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "expander.ml",	"-syntax camlp4o";
    "pa_unpack.ml",	"-pp camlp4of";
    "template.ml",	"-syntax camlp4o";
  ];
]
