open OUnit2

let test_eq name arg1 arg2 =
    name >:: (fun _ -> assert_equal arg1 arg2);;

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let tests = "all_tests" >::: [

    "echo string" >:: (fun _ -> assert_equal (Example.say "hello") "hello");

    "q-01" >:: (fun _ -> assert_equal
        (Questions.last ["a"; "b"; "c"; "d"])
        (Some "d")
    );

    "q-02" >:: (fun _ -> assert_equal
        (Questions.last_two [ "a" ; "b" ; "c" ; "d" ])
        (Some ("c", "d"))
    );

    "q-03" >:: (fun _ -> assert_equal
        (Questions.at 3 [ "a" ; "b"; "c"; "d"; "e" ])
        (Some "c")
    );

    "q-04" >:: (fun _ -> assert_equal
        (Questions.length [ "a" ; "b" ; "c"])
        (3)
    );

    "q-05" >:: (fun _ -> assert_equal
        (Questions.rev ["a"; "b"; "c"])
        (["c"; "b"; "a"])
    );

    "q-06" >:: (fun _ -> assert_equal
        (Questions.is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ])
        (true)
    );

    "q-07" >:: (fun _ -> assert_equal
        (Questions.flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ])
        (["a"; "b"; "c"; "d"; "e"])
    );

    "q-08" >:: (fun _ -> assert_equal
        (Questions.compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
        (["a"; "b"; "c"; "a"; "d"; "e"])
    );

    "q-09" >:: (fun _ -> assert_equal
        (Questions.pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"])
        ([["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]])
    );

    (* "q-10" >:: (fun _ -> assert_equal
        (Questions.encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
        ([(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])
    ); *)
    test_eq "q-10" 
        (Questions.encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
        [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")];

    "q-11" >:: (fun _ -> assert_equal
        (Questions.encode2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
        ([Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")])
    );

    "q-12" >:: (fun _ -> assert_equal
        (Questions.decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")])
        (["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
    );

    "q-13" >:: (fun _ -> assert_equal
        (Questions.encode3 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
        ([Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")])
    );

    "q-14" >:: (fun _ -> assert_equal
        (Questions.duplicate ["a";"b";"c";"c";"d"])
        (["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])
    );

    "q-15" >:: (fun _ -> assert_equal
        (Questions.replicate ["a";"b";"c"] 3)
        (["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])
    );

    test_eq "q-16" (Questions.drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3)
        ["a"; "b"; "d"; "e"; "g"; "h"; "j"];

    test_eq "q-17" (Questions.split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3)
        (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]);

    test_eq "q-18" (Questions.slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6)
        ["c"; "d"; "e"; "f"; "g"];

    test_eq "q-19" (Questions.rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3)
        ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"];

    test_eq "q-19-2" (Questions.rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2))
        ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"];

    test_eq "q-20" (Questions.remove_at 1 ["a";"b";"c";"d"])
        ["a"; "c"; "d"];
]
