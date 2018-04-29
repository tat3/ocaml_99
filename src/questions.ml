(* 1 *)
let rec last = function
    | [] -> None
    | [a] -> Some a
    | a :: b -> last b;;

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

let flatten l =
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
    List.rev (loop 0 [] l)

let decode l =
    let rec many acc n x =
        if n = 0 then acc else many (x :: acc) (n - 1) x in
    let rec aux acc = function
        | [] -> acc
        | One a :: t -> aux (a :: acc) t
        | Many (cnt, a) :: t -> aux (many acc cnt a) t in
    aux [] (List.rev l);;

let encode3 l =
    let rle cnt x = if cnt = 0 then One x else Many (cnt + 1, x) in
    let rec aux cnt acc = function
        | [] -> []
        | [x] -> rle cnt x :: acc
        | a :: (b :: _ as t) ->
            if a = b then aux (cnt + 1) acc t
        else aux 0 (rle cnt a :: acc) t in
    List.rev (aux 0 [] l);;

let duplicate li =
    let rec aux acc = function
        | [] -> acc
        | h :: t -> aux (h :: (h :: acc)) t in
    List.rev (aux [] li)

let replicate li num =
    let rec prepend x cnt acc =
        if cnt = 0 then acc
        else prepend x (cnt - 1) (x :: acc) in
    let rec aux acc = function
        | [] -> acc
        | h :: t -> aux (prepend h num acc) t in
    aux [] (List.rev li)

let drop l position =
    let rec aux pos = function
        | [] -> []
        | h :: t -> 
            if pos = position then aux 1 t
            else h :: aux (pos + 1) t in
    aux 1 l

let split li length =
    let rec aux i acc = function
        | [] -> List.rev acc, []
        | h :: t as l -> if i = 0 then List.rev acc, l
                         else aux (i - 1) (h :: acc) t in
    aux length [] li

let slice li i k =
    let rec aux st ed acc = function
        | [] -> acc
        | h :: t ->
            if st > -1 then aux (st - 1) (ed - 1) (h :: acc) t
            else if ed = 0 then h :: acc
            else aux (st - 1) (ed - 1) acc t in
    List.rev (aux i k [] li)

let slice li i k =
    let rec take n = function
        | [] -> []
        | h :: t -> if n = 0 then [] else h :: take (n - 1) t in
    let rec drop n = function
        | [] -> []
        | h :: t as l-> if n = 0 then l else drop (n - 1) t in
    take (k - i + 1) (drop i li)

let rotate li n =
    let len = List.length li in
    let n = if len = 0 then 0 else (n mod len + len) mod len in
    if n = 0 then li
    else let a, b = split li n in b @ a

let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t
