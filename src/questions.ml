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

let rec insert_at str index = function
    | [] -> [str]
    | h :: t as l ->
        if index = 0 then str :: l
        else h :: insert_at str (index - 1) t

let insert_at str index li =
    let rec aux ind acc = function
        | [] -> acc
        | h :: t as l ->
            if ind = 0 then aux (ind - 1) (h :: str :: acc) t
        else aux (ind - 1) (h :: acc) t in
    List.rev (aux index [] li)

let range a b =
    let rec aux a b acc =
        if a > b then acc else aux (a + 1) b (a :: acc) in
    if a < b then List.rev (aux a b []) else aux b a [];;

let rand_select li n =
    let rec extract acc n = function
        | [] -> raise Not_found
        | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t in
    let extract_rand li len =
        extract [] (Random.int len) li in
    let rec aux n acc li len =
        if n = 0 then acc else
            let picked, rest = extract_rand li len in
            aux (n - 1) (picked :: acc) rest (len - 1) in
    let len = List.length li in
    aux (min n len) [] li len

let lotto_select num n_max =
    rand_select (range 1 n_max) num

let permutation li =
    let rec extract acc n = function
        | [] -> raise Not_found
        | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t in
    let extract_rand li len =
        extract [] (Random.int len) li in
    let rec aux acc l len =
        if len = 0 then acc
        else
            let picked, rest = extract_rand l len in
            aux (picked :: acc) rest (len - 1)
    in
    aux [] li (List.length li)

let rec extract k li =
    if k <= 0 then [[]]
    else match li with
        | [] -> []
        | h :: t ->
            let with_h = List.map (fun l -> h :: l) (extract (k - 1) t) in
            let without_h = extract k t in
    with_h @ without_h

let group li sizes =
    let initial = List.map (fun size -> size, []) sizes in

    let prepend p li =
        let emit l acc = l :: acc in
        let rec aux emit acc = function
            | [] -> emit [] acc
            | (n, l) as h :: t ->
                let acc =
                    if n > 0 then emit ((n - 1, p :: l) :: t) acc
                    else acc in
                aux (fun l acc -> emit (h :: l) acc) acc t in
        aux emit [] li in
    let rec aux = function
        | [] -> [initial]
        | h :: t -> List.concat (List.map (prepend h) (aux t)) in
    let all = aux li in
    let complete = List.filter (List.for_all (fun (x, _) -> x = 0)) all in
    List.map (List.map snd) complete
