%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID

%token TRUE FALSE NULL LBRACE RBRACE LBRACK RBRACK COLON COMMA EOL

%start <Json.value option> prog

%%

prog:
  | EOL { None }
  | v = value { Some v }

value:
  | LBRACE obj = obj_fields RBRACE { `Assoc obj }
  | LBRACK obj = list_fields RBRACK { `List obj }
  | s = STRING { `String s }
  | i = INT { `Int i }
  | f = FLOAT { `Float f }
  | TRUE { `Bool true }
  | FALSE { `Bool false }
  | NULL { `Null }

obj_fields:
    obj = separated_list(COMMA, obj_field)    { obj } 

obj_field:
    k = STRING; COLON; v = value              { (k, v) } 

list_fields:
    vl = separated_list(COMMA, value)         { vl } 
