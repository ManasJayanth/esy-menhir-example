{
  open Parser
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let string = '"' [^'"']* '"'
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

rule line = parse
  | ([^'\n']*'\n') as line  { Some line, true }
  | eof { None, false }
  | ([^'\n']+ as line ) eof { Some (line ^ "\n"), false }


and token = parse
  | white { token lexbuf }
  | newline  { EOL }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "null"   { NULL }
  | string      { STRING (Lexing.lexeme lexbuf) }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ':'      { COLON }
  | ','      { COMMA }
  | eof      { EOL }
  | _ { raise (Failure (Printf.sprintf "At offset %d: unexpected character" (Lexing.lexeme_start lexbuf))) }
