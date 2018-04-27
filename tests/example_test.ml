open OUnit2

(* say関数のテスト関数、テストの説明と関数でテストを表現する *)
let say_test =
  "echo string" >:: (fun _ -> assert_equal (Example.say "hello") "hello")

(* このテストモジュールのすべてのテストをまとめる *)
let tests =
  "all_tests" >::: [ say_test; ]
