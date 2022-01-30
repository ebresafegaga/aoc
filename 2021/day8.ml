(* soltion = sum . map length . map (filter unique) . map snd  *)
open Prelude

let unique number = 
  match String.length number with 
  | 2 | 3 | 4 | 7 -> true | _ -> false

let part1 = List.map (snd >> List.filter unique >> List.length) >> sum

let derive_mapping : string list -> (string * int) list = fun numbers -> 
  (* we can easily derive the mappings for 1,4,7,8 *)
  let derive_simple =
    let e = "" in
    let f (one, four, seven, eight) str = 
      match String.length str with 
      | 2 -> (str, four, seven, eight)
      | 4 -> (one, str, seven, eight)
      | 3 -> (one, four, str, eight)
      | 7 -> (one, four, seven, str)
      | _ -> (one, four, seven, eight)
    in 
    List.fold_left f (e, e, e, e)  
  in
  let (one, four, seven, eight) = derive_simple numbers in

  (* remove 1,4,7,8 from the list *)
  let numbers = numbers |> List.filter (unique >> not) in 
  (* seperate 5 segments from 6 segments *)
  let (fives, sixs) = numbers |> List.partition (fun number -> String.length number = 5) in

  (* derive fives *)
  (* if it has all characters present in 1 that means it is 3
      else if it has all characters in (4 apart from the ones present in 1) then it is 5 
      else it is 2 *)
  let derive_fives  = 
    let one, four = string_to_list one, string_to_list four in 
    let fives = fives |> List.map string_to_list in 
    let p1 str = one |> List.for_all (fun elem -> List.exists ((=) elem) str) in 
    let p2 str = 
      let four_minus_one = List.fold_right remove one four in 
      four_minus_one |> List.for_all (fun elem -> List.exists ((=) elem) str)
    in 
    let (a, b) = fives |> List.partition p1 in
    let (b, c) = b |> List.partition p2 in 
    let three, five, two = List.hd a, List.hd b, List.hd c in
    (list_to_string three, list_to_string five, list_to_string two)  
  in
  let (three, five, two) = derive_fives in
  
  (* derive sixs *)
  (* if it doesn't have all segments in 1 it is a 6 (i.e at most one segment)
    else if it has all segments lit by 4 it is a 9 
    else it is a 0  *)
  let derive_sixs  =
    let one, four = string_to_list one, string_to_list four in 
    let sixs = sixs |> List.map string_to_list in 
    let p1 str = one |> List.filter (fun elem -> str |> List.exists ((=) elem) |> not) |> List.length = 1 in
    let p2 str = four |> List.for_all (fun elem -> List.exists ((=) elem) str) in 
    let (a, b) = sixs |> List.partition p1 in 
    let (b, c) = b |> List.partition p2 in 
    let six, nine, zero = List.hd a, List.hd b, List.hd c in
    (list_to_string six, list_to_string nine, list_to_string zero)
  in
  let (six, nine, zero) = derive_sixs  in

  [ zero, 0; one, 1; two, 2; three, 3; four, 4; 
    five, 5; six, 6; seven, 7; eight, 8; nine, 9 ]

let apply_mapping : (string * int) list -> string -> int = fun mapping elem ->
  let p (string, _number)  = 
    let string = string_to_list string in 
    let elem = string_to_list elem in 
    if List.length string <> List.length elem then false
    else elem |> List.for_all (fun x -> List.exists ((=) x) string)
  in 
  mapping |> List.find p |> snd

let aggregate = List.fold_left (fun t n -> t*10 + n) 0

let part2 = List.map (fun (input, output) -> output |> List.map (input |> derive_mapping |> apply_mapping) |> aggregate) >>  sum

let parse_numbers = String.split_on_char ' '
let parse str = 
  match Str.split (Str.regexp " | ") str with 
  | [left; right] -> parse_numbers left, parse_numbers right 
  | _ -> failwith "expected two items seperated by a bar."

let () =  
  let file = "./input/day8.txt" in
  let input = file |> input_lines |> List.map parse in
  Printf.printf "part one: %d \n" (part1 input);
  Printf.printf "part two: %d \n" (part2 input)