let opt2str = function
    | None -> "None"
    | Some x -> x;;

let print_option a = print_string (opt2str a ^ "\n");;

(* 1 *)
let rec last = function
    | [] -> None
    | [a] -> Some a
    | a :: b -> last b;;

(* print_option (last ["a"; "b"; "c"; "d"]);;
print_option (last []); *)

(* 2 *)
let rec last_two l =
    match l with
    | [] | [_] -> None
    | x :: y :: []-> Some (x, y)
    | _ :: s -> last_two s;;

let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k - 1) t;;

(* tail-recursive *)
let length l =
    let rec loop n = function
        | [] -> n
        | _ :: t -> loop (n + 1) t
    in loop 0 l;;

let rev l =
    let rec loop r = function
        | [] -> r
        | h :: t -> loop (h :: r) t in
    loop [] l;;

let is_palindrome l = l = rev l;;

type 'a node =
    | One of 'a
    | Many of 'a node list;;

let flattern l =
    let rec loop acc = function
        | [] -> acc
        | One x :: t -> loop (x :: acc) t
        | Many h :: t -> loop (loop acc h) t in
    rev (loop [] l);;

let rec compress = function
    | a :: (b :: c as t) -> if a = b then compress t else a :: compress t
    | other -> other;;

let pack l = 
    let rec loop current acc = function
        | [] -> []
        | [a] -> (a :: current) :: acc
        | a :: (b :: c as t) ->
            if a = b then loop (a :: current) acc t
            else loop [] ((a :: current) :: acc) t in
    List.rev (loop [] [] l);;

let encode l =
    let rec loop count acc = function
        | [] -> []
        | [x] -> (count + 1, x) :: acc
        | a :: (b :: _ as t) ->
            if a = b then loop (count + 1) acc t
            else loop 0 ((count + 1, a) :: acc) t in
    List.rev (loop 0 [] l);;

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode2 l =
    let create_tuple cnt elm =
        if cnt = 1 then One elm
        else Many (cnt, elm) in
    let rec loop count acc = function
        | [] -> []
        | [a] -> (create_tuple (count + 1) a) :: acc
        | a :: (b :: _ as t) ->
            if a = b then loop (count + 1) acc t
            else loop 0 ((create_tuple (count + 1) a) :: acc) t in
    loop 0 [] l;;

let decode l =
    let rec many acc n x =
        if n = 0 then acc else many (x :: acc) (n - 1) x in
    let rec aux acc = function
        | [] -> acc
        | One a :: t -> aux (a :: acc) t
        | Many (cnt, a) :: t -> aux (many acc cnt a) t in
    aux [] (List.rev l);;

