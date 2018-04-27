open OUnit2

(* Basic test *)
let say_test =
    "echo string" >:: (fun _ -> assert_equal (Example.say "hello") "hello")

let q00 =
    "q-00" >:: (fun _ -> assert_equal
        (Questions.last ["a"; "b"; "c"; "d"])
        (Some "d"))

let tests =
    "all_tests" >::: [
        say_test;
        q00; 
    ]
