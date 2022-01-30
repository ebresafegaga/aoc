open Prelude

let rec visited_double = function
  | [] -> false 
  | v :: visited when is_lower v -> List.exists ((=) v) visited || visited_double visited
  | _v :: visited -> visited_double visited

let next1 graph visited node =
  let visited_small key = visited |> List.exists (fun v -> v = key && is_lower key) in
  let f (key, value) = if node = key && not (visited_small value) then Some value else None in 
  graph |> List.filter_map f 

let next2 graph visited node =
  let visited_small key = 
    if contains key visited && not (visited_double visited) then false
    else if contains key visited then visited |> List.exists (fun v -> v = key && is_lower key) 
    else  false
  in
  let f (n, value) = if node = n && not (visited_small value) then Some value else None in 
  graph |> List.filter_map f

let last node = node = "end"
let start = "start"

let rec all_path graph v next node = 
  let v = node :: v in
  let adjs = next graph v node  in
  if last node then [[node]] else
  List.concat_map (all_path graph v next >> List.map (List.cons node)) adjs

let all_path next g = all_path g [] next start
let part1 = all_path next1 >> List.length
let part2 = all_path next2  >> List.length

let edge = function 
  | [any;"start"] | ["start";any]-> [("start", any)]
  | ["end";any] | [any;"end"] -> [(any, "end")]
  | [a;b] -> [(a,b); (b,a)]
  | _ -> failwith "expeected two items"

let parse = List.concat_map (String.split_on_char '-' >> edge)

let () =  
  let file = "./input/day12.txt" in
  let lines = file |> input_lines |> parse in 
  Printf.printf "part one: %d \n" (part1 lines);
  Printf.printf "part two: %d \n" (part2 lines)