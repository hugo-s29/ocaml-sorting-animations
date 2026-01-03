let[@animate] insertion_sort =
  let rec insert x = function
    | [] -> x :: []
    | y :: l when x < y -> x :: y :: l
    | y :: l -> y :: insert x l
  in
  let rec aux sorted = function
    | [] -> sorted
    | x :: l -> aux (insert x sorted) l
  in
  fun l -> aux [] l


let[@animate] selection_sort =
  let rec select_min = function
    | [] -> []
    | h :: t ->
      (match select_min t with
       | h' :: t' when h < h' -> h :: h' :: t'
       | h' :: t' -> h' :: h :: t'
       | [] -> h :: [])
  in
  let rec sort l =
    match select_min l with
    | min_el :: rest -> min_el :: sort rest
    | [] -> []
  in
  sort


let[@animate] merge_sort =
  let rec merge l1 l2 =
    match (l1, l2) with
    | [], l | l, [] -> l
    | h1 :: t1, h2 :: t2 when h1 < h2 -> h1 :: merge t1 (h2 :: t2)
    | h1 :: t1, h2 :: t2 -> h2 :: merge (h1 :: t1) t2
  in
  let split (l : 'a list) : 'a list * 'a list =
    let n = List.length l in
    let left = List.take (n / 2) l in
    (* Repeated List.hd, can easily be done with recursion *)
    let right = List.drop (n / 2) l in
    (* Repeated List.tl, can easily be done with recursion *)
    (left, right)
  in
  let rec sort = function
    | [] -> []
    | [ x ] -> [ x ]
    | l ->
      let left, right = split l in
      let left = sort left in
      let right = sort right in
      merge left right
  in
  fun l -> sort l


let[@animate] quicksort =
  let rec partition pivot = function
    | [] -> ([], [])
    | x :: l when x >= pivot ->
      let a, b = partition pivot l in
      (a, x :: b)
    | x :: l ->
      let a, b = partition pivot l in
      (x :: a, b)
  in
  let rec sort = function
    | [] -> []
    | pivot :: rest ->
      let smaller, greater = partition pivot rest in
      let greater = sort greater in
      let smaller = sort smaller in
      List.append (List.append smaller [ pivot ]) greater
  in
  fun l -> sort l


let () =
  print_endline "Choose the algorithm to look at";
  print_endline "1. Insertion Sort O(n²)";
  print_endline "2. Selection Sort O(n²)";
  print_endline "3. Merge Sort O(n log n)";
  print_endline "4. Quicksort Sort O(n log n)";
  print_endline "(average complexities, not the worst-case ones)";
  print_endline "";
  print_endline "Warning, the screen can flicker a little, and the animation";
  print_endline "does not always represent the “real” state of the sorted list";
  print_endline "(it's only an approximation)";
  let choice = ref (read_int_opt ()) in
  let is_invalid = function
    | None -> true
    | Some x -> x <= 0 || x > 4
  in
  while is_invalid !choice do
    print_endline "Invalid choice, try again";
    choice := read_int_opt ()
  done;
  match !choice with
  | Some 1 -> visualize insertion_sort_animation
  | Some 2 -> visualize selection_sort_animation
  | Some 3 -> visualize merge_sort_animation
  | Some 4 -> visualize quicksort_animation
  | _ -> assert false
