type color =
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet

type stick = int

let color_of_int i =
  match i mod 7 with
  | 0 -> Red
  | 1 -> Orange
  | 2 -> Yellow
  | 3 -> Green
  | 4 -> Blue
  | 5 -> Indigo
  | _ -> Violet


let positions = ref (Array.init 0 (fun i -> i))
let elements = ref (Array.init 0 (fun i -> i))
let colors = ref (Array.init 0 color_of_int)
let has_changed = ref true

let get_color_code : color -> string = function
  | Red -> "\027[41m"
  | Orange -> "\027[43m"
  | Yellow -> "\027[103m"
  | Green -> "\027[42m"
  | Blue -> "\027[44m"
  | Indigo -> "\027[45m"
  | Violet -> "\027[105m"


let swap (i : int) (j : int) : unit =
  if i <> j then (
    let el = !elements.(i)
    and el' = !elements.(j) in
    !positions.(el) <- j;
    !positions.(el') <- i;
    !elements.(i) <- el';
    !elements.(j) <- el;
    has_changed := true)


let shuffle (n : int) : unit =
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    swap i j
  done


let sleep_length = ref 1.0
let sleep () = Unix.sleepf !sleep_length

let prepare (n : int) : stick list =
  positions := Array.init n (fun i -> i);
  elements := Array.init n (fun i -> i);
  colors := Array.init n color_of_int;
  shuffle n;
  List.init n (Array.get !elements)


(* Find the best position for the list, considering both current positions
   and where elements should eventually be *)
let find_best_position (l : stick list) : int =
  if l = [] then
    0
  else (
    let n = Array.length !elements in
    let list_len = List.length l in
    let current_positions = List.map (Array.get !positions) l in
    let target_positions = l in
    (* Try different anchor positions and score them *)
    let score_position anchor =
      let distance = ref 0 in
      List.iteri
        (fun i el ->
           let new_pos = anchor + i in
           let current_pos = !positions.(el) in
           let target_pos = el in
           (* Element value = its target position *)
           (* Penalize distance from current position (to minimize movement) *)
           distance := !distance + abs (new_pos - current_pos);
           (* Penalize distance from target position (to guide towards final state) *)
           (* Weight this less than current distance to avoid too much disruption *)
           distance := !distance + (abs (new_pos - target_pos) / 2))
        l;
      !distance
    in
    (* Find the best anchor among several candidates *)
    let candidates =
      (* Include: min position, max position, median, and target-based position *)
      let positions_array = Array.of_list current_positions in
      Array.sort compare positions_array;
      let min_pos = positions_array.(0) in
      let max_pos = positions_array.(Array.length positions_array - 1) in
      let median_pos = positions_array.(list_len / 2) in
      (* Average target position *)
      let avg_target = List.fold_left ( + ) 0 target_positions / list_len in
      (* Generate candidates in valid range *)
      let all_candidates =
        [ min_pos; max_pos - list_len + 1; median_pos; avg_target; avg_target - (list_len / 2) ]
      in
      List.filter (fun pos -> pos >= 0 && pos + list_len <= n) all_candidates
    in
    (* If no valid candidates (shouldn't happen), use safe default *)
    if candidates = [] then
      max 0 (min (n - list_len) (List.hd current_positions))
    else (
      (* Find candidate with minimum score *)
      let best_pos = ref (List.hd candidates) in
      let best_score = ref (score_position !best_pos) in
      List.iter
        (fun pos ->
           let s = score_position pos in
           if s < !best_score then (
             best_score := s;
             best_pos := pos))
        (List.tl candidates);
      !best_pos))


let update_list : stick list -> unit = function
  | [] -> ()
  | l ->
    let n = Array.length !elements in
    let list_len = List.length l in
    let anchor_pos = find_best_position l in
    (* Double-check bounds *)
    let anchor_pos = max 0 (min anchor_pos (n - list_len)) in
    List.iteri
      (fun i el ->
         let new_pos = anchor_pos + i in
         let old_pos = !positions.(el) in
         swap old_pos new_pos)
      l


let display () =
  let altered_height = List.map (fun h -> (h / 3) + 1) (Array.to_list !elements) in
  let max_height = List.fold_left max 0 altered_height in
  let grid = Array.make_matrix max_height (Array.length !elements) " " in
  List.iteri
    (fun col height ->
       let color_code = get_color_code !colors.(!elements.(col)) in
       for row = max_height - height to max_height - 1 do
         grid.(row).(col) <- color_code ^ " " ^ "\027[0m"
       done)
    altered_height;
  let lines = List.map (fun x -> String.concat "" (Array.to_list x)) (Array.to_list grid) in
  let space = String.make 50 '\n' in
  print_endline "\027[2J\027[H";
  print_endline (space ^ String.concat "\n" lines);
  flush stdout;
  if !has_changed then (
    sleep ();
    has_changed := false)


let return (x : stick list) : stick list =
  update_list x;
  display ();
  x


let _action : (stick list -> stick list) ref = ref return
let action (x : stick list) : stick list = !_action x

let visualize (f : stick list -> stick list) : unit =
  let steps = ref 0 in
  let sticks = prepare 150 in
  let count x =
    incr steps;
    x
  in
  _action := count;
  let _ = f sticks in
  sleep_length := 2.0 /. float_of_int !steps;
  Printf.printf "Number of steps : %d\n\n" !steps;
  _action := return;
  let _ = f sticks in
  ()


module Runtime = struct
  let ( ^:: ) x y = action (x :: y)
  let visualize = visualize
end
