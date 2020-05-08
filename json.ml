type value = [
  | `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string
]

let rec string_of_assoc x =
  x
  |> List.map (fun (k, v) -> Printf.sprintf "%s: %s" k (toString v))
  |> String.concat "," |> Printf.sprintf "{ %s }" 

and toString = function
  | `Assoc x -> string_of_assoc x
  | `Bool x -> string_of_bool x
  | `Float x -> string_of_float x
  | `Int x -> string_of_int x
  | `List x -> String.concat "," (List.map toString x)
  | `Null -> "null"
  | `String x -> x

