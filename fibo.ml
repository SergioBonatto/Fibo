(* Tipos básicos para representar expressões lambda *)
type expr =
  | Var of string
  | Lambda of string * expr
  | App of expr * expr

(* Ambiente para armazenar definições *)
module StrMap = Map.Make(String)
type environment = expr StrMap.t

(* Exceções personalizadas *)
exception Parse_error of string
exception Runtime_error of string

(* Função auxiliar para dividir string em tokens *)
let tokenize str =
  (* Usamos o caractere lambda unicode diretamente *)
  let lambda = "\xCE\xBB" in (* λ em UTF-8 *)
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let is_special = function '(' | ')' | '.' | '=' -> true | _ -> false in

  let rec split acc curr = function
    | [] -> List.rev (if curr = [] then acc else (String.concat "" (List.rev curr)) :: acc)
    | '\xCE' :: '\xBB' :: rest ->  (* Detecta λ em UTF-8 *)
        let new_acc = if curr = [] then lambda :: acc
                     else lambda :: (String.concat "" (List.rev curr)) :: acc in
        split new_acc [] rest
    | c :: rest when is_space c ->
        split (if curr = [] then acc else (String.concat "" (List.rev curr)) :: acc) [] rest
    | c :: rest when is_special c ->
        let new_acc = if curr = [] then String.make 1 c :: acc
                     else String.make 1 c :: (String.concat "" (List.rev curr)) :: acc in
        split new_acc [] rest
    | c :: rest ->
        split acc (String.make 1 c :: curr) rest
  in
  str
  |> String.to_seq
  |> List.of_seq
  |> split [] []
  |> List.filter (fun s -> s <> "")

(* Parser recursivo descendente *)
let parse tokens =
  let rec parse_expr = function
    | [] -> raise (Parse_error "Unexpected end of input")
    | t :: var :: "." :: rest when t = "\xCE\xBB" ->
        let body, remaining = parse_expr rest in
        Lambda(var, body), remaining
    | "(" :: rest ->
        let func, rest1 = parse_expr rest in
        let arg, rest2 = parse_expr rest1 in
        (match rest2 with
        | ")" :: remaining -> App(func, arg), remaining
        | _ -> raise (Parse_error "Missing closing parenthesis"))
    | token :: rest ->
        if token = ")" then raise (Parse_error "Unexpected closing parenthesis")
        else Var(token), rest
  in

  let expr, remaining = parse_expr tokens in
  if remaining <> [] then
    raise (Parse_error "Extra tokens after expression")
  else expr

(* Substituição de variáveis, evitando captura *)
let rec substitute expr var value =
  match expr with
  | Var x when x = var -> value
  | Var x -> Var x
  | Lambda(x, body) when x = var -> Lambda(x, body)
  | Lambda(x, body) -> Lambda(x, substitute body var value)
  | App(e1, e2) -> App(substitute e1 var value, substitute e2 var value)

(* Avaliador *)
let rec evaluate env expr =
  let rec eval_steps steps expr =
    if steps > 1000 then
      raise (Runtime_error "Maximum evaluation steps exceeded")
    else
      match expr with
      | Var x ->
          (match StrMap.find_opt x env with
          | Some e -> eval_steps (steps + 1) e
          | None -> expr)
      | Lambda _ -> expr
      | App(e1, e2) ->
          let v1 = eval_steps steps e1 in
          let v2 = eval_steps steps e2 in
          match v1 with
          | Lambda(x, body) -> eval_steps (steps + 1) (substitute body x v2)
          | _ -> App(v1, v2)
  in
  eval_steps 0 expr

(* Processamento de definições e expressões *)
let process_line env line =
  let tokens = tokenize line in
  match tokens with
  | "let" :: name :: "=" :: rest ->
      let expr = parse rest in
      (StrMap.add name expr env, None)
  | _ ->
      let expr = parse tokens in
      (env, Some(evaluate env expr))

(* Função principal para processar o arquivo *)
let process_file filename =
  let lines =
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        if line = "" then read_lines acc
        else read_lines (line :: acc)
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    read_lines []
  in

  let rec process_lines env = function
    | [] -> raise (Runtime_error "No expression to evaluate")
    | [last] ->
        let _, result = process_line env last in
        (match result with
        | Some expr -> expr
        | None -> raise (Runtime_error "Last line must be an expression"))
    | line :: rest ->
        let new_env, _ = process_line env line in
        process_lines new_env rest
  in

  process_lines StrMap.empty lines

(* Pretty printer para expressões *)
let rec string_of_expr = function
  | Var x -> x
  | Lambda(x, body) -> "λ" ^ x ^ ". " ^ string_of_expr body
  | App(e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"

(* Função main *)
let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.printf "Uso: %s <arquivo>\n" Sys.argv.(0);
    exit 1
  end;

  try
    let result = process_file Sys.argv.(1) in
    Printf.printf "=> %s\n" (string_of_expr result)
  with
  | Parse_error msg -> Printf.printf "Erro de parsing: %s\n" msg
  | Runtime_error msg -> Printf.printf "Erro de execução: %s\n" msg
  | Sys.Break -> Printf.printf "Operação cancelada\n"
  | e -> Printf.printf "Erro inesperado: %s\n" (Printexc.to_string e)
