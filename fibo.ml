open Printf

type expression =
  | LambdaExpression of string * expression
  | Application of expression * expression
  | Var of string

exception LambdaParseError of string

let rec substitute expr var value =
  match expr with
  | LambdaExpression (v, body) when v = var -> expr
  | LambdaExpression (v, body) ->
      if v = var then expr
      else LambdaExpression (v, substitute body var value)
  | Application (func, arg) ->
      Application (substitute func var value, substitute arg var value)
  | Var v when v = var -> value
  | Var v -> Var v

let tokenize expr =
  let expr = Str.global_replace (Str.regexp "[\\.()=\\\\]") " \\0 " expr in
  let expr = Str.global_replace (Str.regexp "[ \t\n\r]+") " " expr in
  let tokens = Str.split (Str.regexp " ") expr in
  List.filter (fun s -> s <> "") tokens

let rec parse_expression tokens pos =
  if pos >= Array.length tokens then
    raise (LambdaParseError "Fim inesperado da expressão")
  else
    let current = tokens.(pos) in
    if current = "\\" then
      if pos + 2 >= Array.length tokens || tokens.(pos + 2) <> "." then
        raise (LambdaParseError "Sintaxe inválida para expressão lambda")
      else
        let var = tokens.(pos + 1) in
        if not (Str.string_match (Str.regexp "[a-zA-Z0-9]+") var 0) then
          raise (LambdaParseError ("Nome de variável inválido: " ^ var))
        else
          let body, new_pos = parse_expression tokens (pos + 3) in
          LambdaExpression (var, body), new_pos
    else if current = "(" then
      let func, pos = parse_expression tokens (pos + 1) in
      let rec parse_args func pos =
        if pos >= Array.length tokens then
          raise (LambdaParseError "Parêntese não fechado")
        else if tokens.(pos) = ")" then
          func, pos + 1
        else
          let arg, new_pos = parse_expression tokens pos in
          parse_args (Application (func, arg)) new_pos
      in
      parse_args func pos
    else if current = ")" || current = "." || current = "=" then
      raise (LambdaParseError ("Token inesperado: " ^ current))
    else
      Var current, pos + 1

let parse_definition tokens pos =
  if pos + 2 >= Array.length tokens || tokens.(pos + 2) <> "=" then
    raise (LambdaParseError "Sintaxe inválida para definição let")
  else
    let name = tokens.(pos + 1) in
    let expr, new_pos = parse_expression tokens (pos + 3) in
    name, expr, new_pos

let rec evaluate expr env max_steps =
  let rec eval expr steps =
    if steps >= max_steps then
      raise (Failure "Número máximo de passos de redução excedido")
    else
      match expr with
      | Var v -> (
          try List.assoc v env with Not_found -> expr)
      | LambdaExpression _ -> expr
      | Application (func, arg) -> (
          match eval func (steps + 1) with
          | LambdaExpression (var, body) ->
              eval (substitute body var (eval arg (steps + 1))) (steps + 1)
          | func' -> Application (func', eval arg (steps + 1)))
  in
  eval expr 0


let rec expression_to_string expr =
  match expr with
  | LambdaExpression (var, body) -> Printf.sprintf "λ%s.%s" var (expression_to_string body)
  | Application (func, arg) ->
      let func_str = expression_to_string func in
      let arg_str = expression_to_string arg in
      Printf.sprintf "(%s %s)" func_str arg_str
  | Var v -> v


let process_code code =
  let env = ref [] in
  let lines = List.filter (fun s -> s <> "") (Str.split (Str.regexp "\n") code) in
  let last_expr = ref None in
  List.iter
    (fun line ->
      let tokens = Array.of_list (tokenize line) in
      if Array.length tokens > 0 then
        if tokens.(0) = "let" then
          let name, expr, _ = parse_definition tokens 0 in
          env := (name, expr) :: !env
        else
          let expr, _ = parse_expression tokens 0 in
          last_expr := Some expr)
    lines;
  match !last_expr with
  | None -> raise (LambdaParseError "Nenhuma expressão para avaliar")
  | Some expr ->
      printf "Ambiente: %s\n" (String.concat ", " (List.map (fun (name, expr) -> name ^ " = " ^ (expression_to_string expr)) !env));
      printf "Expressão: %s\n" (expression_to_string expr);
      evaluate expr !env 1000

let main () =
  if Array.length Sys.argv <> 2 then (
    printf "Uso: ocaml fibo.ml <arquivo>\n";
    exit 1)
  else
    let file_path = Sys.argv.(1) in
    try
      let ic = open_in file_path in
      let code = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let result = process_code code in
      printf "Resultado da avaliação: %s\n" (expression_to_string result);
      printf "=> %s\n" (expression_to_string result)
    with
    | LambdaParseError e ->
        printf "Erro de parsing: %s\n" e;
        exit 1
    | Sys_error e ->
        printf "Erro de arquivo: %s\n" e;
        exit 1
    | Failure e ->
        printf "Erro inesperado: %s\n" e;
        exit 1

let () = main ()
