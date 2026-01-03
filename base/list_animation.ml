(* Reimplementation of List functions, except with PPX running *)
(* No rev (or rev-related) as it messes with the visualizer, *)
(* that's why some of the implementations are very clearly suboptimal *)

let[@animate] hd = function
  | [] -> raise (Invalid_argument "hd")
  | x :: _ -> x


let[@animate] tl = function
  | [] -> raise (Invalid_argument "tl")
  | _ :: q -> q


let[@animate] append l l2 =
  let rec aux = function
    | [] -> l2
    | x :: q -> x :: aux q
  in
  aux l


let[@animate] rec fold_left f acc = function
  | [] -> acc
  | x :: q -> fold_left f (f acc x) q


let[@animate] length l = fold_left (fun acc _ -> acc + 1) 0 l
let[@animate] iter f = fold_left (fun _ x -> f x) ()

let[@animate] iteri f l =
  ignore
    (fold_left
       (fun i x ->
          ignore (f i x);
          i + 1)
       0
       l)


let[@animate] rec map f = function
  | [] -> []
  | x :: q ->
    let v = f x in
    v :: map f q


let[@animate] mapi f l =
  let rec aux n = function
    | [] -> []
    | x :: q ->
      let v = f n x in
      v :: aux (n + 1) q
  in
  aux 0 l


let[@animate] init n f =
  let[@animate] rec aux i =
    if i = n then
      []
    else (
      let[@animate] v = f i in
      v :: aux (i + 1))
  in
  aux 0


let[@animate] rec for_all f = function
  | [] -> true
  | x :: q -> f x && for_all f q


let[@animate] rec exists f = function
  | [] -> false
  | x :: q -> f x || exists f q


let[@animate] rec mem x = function
  | [] -> false
  | y :: q -> x = y || mem x q


let[@animate] rec take n = function
  | _ when n <= 0 -> []
  | [] -> []
  | x :: q -> x :: take (n - 1) q


let[@animate] rec drop n = function
  | l when n <= 0 -> l
  | [] -> []
  | _ :: q -> drop (n - 1) q


let[@animate] rec append l l' =
  match l with
  | [] -> l'
  | x :: l -> x :: append l l'


let[@animate] ( @ ) = append

let[@animate] rec filter f = function
  | [] -> []
  | x :: l when f x -> x :: filter f l
  | _ :: l -> filter f l
