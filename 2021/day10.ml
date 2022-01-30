open Prelude

(* solution = sum . map lookup . filterMap getError *)

type tok = Curly | Paren | Square | Angle
type err = NoError | Incomplete of tok list | Corrupted of tok

let is_incomplete = function Incomplete xs -> Some xs | _ -> None 

let to_tok = function 
  | '(' | ')' -> Paren | '{' | '}' -> Curly
  | '[' | ']' -> Square | '<' | '>' -> Angle
  | _ -> failwith "idk" 

let pair c =
  match c with
  | '(' ->  ')'  | '{' ->  '}'
  | '[' ->  ']' | '<' ->  '>'
  | _ -> failwith "idk"  

let starts = function 
  | '('   | '{' | '[' | '<' ->  true
  | _ -> false 

let expected c = Corrupted (to_tok c)

let consume ch tokens = 
  match tokens with
  | [] -> Incomplete [], [] 
  | c :: rest ->  
    if c <> ch then expected c, rest  
    else NoError, rest

let rec parse_tok xs = 
  match xs with
  | [] -> NoError, [] 
  | c :: rest when starts c -> 
    let (result, rest) = parse_tok rest in
    (match result with 
    | NoError -> 
      (match consume (pair c) rest with
      | NoError, rest -> parse_tok rest  
      | Incomplete is, rest -> Incomplete (snoc is (to_tok (pair c))), rest
      | Corrupted t, rest -> Corrupted t, rest)  
    | Incomplete is -> Incomplete (snoc is (to_tok (pair c))), rest
    | error -> error, rest)
  | rest -> NoError, rest

let lookup = function 
  | Corrupted Paren -> 3 | Corrupted Square -> 57
  | Corrupted Curly -> 1197 | Corrupted Angle -> 25137
  | _ -> 0   

let part1 = List.map (string_to_list >> parse_tok >> fst >> lookup) >> sum

let points =
  let lookup = function Paren -> 1 | Square -> 2 | Curly -> 3 | Angle -> 4 in
  List.fold_left (fun total tok -> 5*total + lookup tok) 0

let middle l = List.nth l (List.length l / 2)
let bind = Fun.flip Option.bind
let part2 = List.filter_map (string_to_list >> parse_tok >> fst >> is_incomplete >> bind (points >> Option.some)) >> List.sort (-) >> middle

let () =  
  let file = "./input/day10.txt" in
  let lines = file |> input_lines  in 
  Printf.printf "part one: %d \n" (part1 lines);
  Printf.printf "part two: %d \n" (part2 lines)