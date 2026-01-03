# Sorting Animations

The goal of this mini project is to allow people to write simple sorting algorithms like

```ocaml
let insertion_sort =
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
```

then to simply add a `[@animate]` attribute to the code

```ocaml
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
```

and automatically setting up an animation that can be ran with

```ocaml
let () = visualize insertion_sort_animation
```

As we are dealing with a `list`-based algorithm and not an `array`-based one, we can't simply display the whole array in the console, and instead we have to "estimate" a sublist's position.

_**Warning.**_ The display can be kind of flashy, it flickers a bit.

Under the hood, a custom PPX takes the code and transforms it into animation code (function `f` becomes `f_animation`, and `f` can still be used as usual).

- The `bin/` folder contains an example implementation of some sorting algorithms (insertion sort, selection sort, merge sort, quick sort).
- The `ppx/` folder contains the PPX responsible for this madness (only used at compile time).
- The `visualizer/` is used at runtime to visualize the sorting algorithms (the `Runtime` module is especially used).
- The `base/` folder contains the reimplementation of some functions from the OCaml `List` module, except they have the PPX applied to them.
