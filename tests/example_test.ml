open OUnit2;;


let test_eq name arg1 arg2 =
    name >:: (fun _ -> assert_equal arg1 arg2);;

type 'a node =
    | One of 'a
    | Many of 'a node list

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let tests = "all_tests" >::: [

    test_eq "echo string" (Example.say "hello") "hello";

    test_eq "q-01"
        (Questions.last ["a"; "b"; "c"; "d"])
        (Some "d");

    test_eq "q-02"
        (Questions.last_two [ "a" ; "b" ; "c" ; "d" ])
        (Some ("c", "d"));

    test_eq "q-03"
        (Questions.at 3 [ "a" ; "b"; "c"; "d"; "e" ])
        (Some "c");

    test_eq "q-04"
        (Questions.length [ "a" ; "b" ; "c"])
        3;

    test_eq "q-05"
        (Questions.rev ["a"; "b"; "c"])
        ["c"; "b"; "a"];

    test_eq "q-06"
        (Questions.is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ])
        true;

    test_eq "q-07"
        (Questions.flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ])
        ["a"; "b"; "c"; "d"; "e"];

    test_eq "q-08"
        (Questions.compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
        ["a"; "b"; "c"; "a"; "d"; "e"];

    test_eq "q-09"
        (Questions.pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"])
        [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]];

    test_eq "q-10"
        (Questions.encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
        [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")];

    test_eq "q-11"
        (Questions.encode2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
        [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];

    test_eq "q-12"
        (Questions.decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")])
        ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];

    test_eq "q-13"
        (Questions.encode3 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
        [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];

    test_eq "q-14"
        (Questions.duplicate ["a";"b";"c";"c";"d"])
        ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"];

    test_eq "q-15"
        (Questions.replicate ["a";"b";"c"] 3)
        ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"];

    test_eq "q-16" (Questions.drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3)
        ["a"; "b"; "d"; "e"; "g"; "h"; "j"];

    test_eq "q-17" (Questions.split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3)
        (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]);

    test_eq "q-17-2" (Questions.split ["a";"b";"c";"d"] 5)
        (["a"; "b"; "c"; "d"], []);


    test_eq "q-18" (Questions.slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6)
        ["c"; "d"; "e"; "f"; "g"];

    test_eq "q-19" (Questions.rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3)
        ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"];

    test_eq "q-19-2" (Questions.rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2))
        ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"];

    test_eq "q-20" (Questions.remove_at 1 ["a";"b";"c";"d"])
        ["a"; "c"; "d"];

    test_eq "q-21"
        (Questions.insert_at "alfa" 1 ["a";"b";"c";"d"])
        ["a"; "alfa"; "b"; "c"; "d"];

    test_eq "q-22"
        (Questions.range 4 9)
        [4; 5; 6; 7; 8; 9];

    test_eq "q-22-2"
        (Questions.range 9 4)
        [9; 8; 7; 6; 5; 4];

    test_eq "q-23"
        (Questions.rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3)
        ["g"; "d"; "a"];

    (* test_eq "q-24"
        (Questions.lotto_select 6 49)
        [10; 20; 44; 22; 41; 2]; *)

    (* test_eq "q-25"
        (Questions.permutation ["a"; "b"; "c"; "d"; "e"; "f"])
        ["a"; "e"; "f"; "b"; "d"; "c"]; *)

     test_eq "q-26"
        (Questions.extract 2 ["a";"b";"c";"d"])
        [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]];

    test_eq "q-27"
        (Questions.group ["a";"b";"c";"d"] [2;1])
        [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];[["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];[["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];[["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
         ];
