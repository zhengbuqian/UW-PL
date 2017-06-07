(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["a", "B", "C"] = ["B", "C"]

val test2 = longest_string1 ["A","bc","cd"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A","bc","cd"] = "cd"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_1 = ((first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5]; false) handle NoAnswer => true)

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 5 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x > 5 then SOME [x] else NONE) [] = (SOME [])

val test9a = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards (Variable "") = 0
val test9a_2 = count_wildcards (TupleP ([Wildcard, Variable "", Wildcard])) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths (TupleP ([Wildcard, Variable "abc", Wildcard])) = 5

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("x", TupleP ([Wildcard, Variable "abc", Wildcard])) = 0
val test9c_2 = count_some_var ("abc", TupleP ([Variable "x", Wildcard, Variable "abc", Wildcard])) = 1

val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat (TupleP ([Variable "x", Wildcard, Variable "abc", Wildcard])) = true
val test10_2 = check_pat (TupleP ([Variable "abc", Wildcard, Variable "abc", Wildcard])) = false


val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match (Const(1), Wildcard) = (SOME [])
val test11_2 = match (Const(1), ConstP(1)) = (SOME [])
val test11_3 = match (Const(1), ConstP(2)) = NONE
val test11_4 = match (Const(1), Variable("s")) = (SOME [("s", Const(1))])
val test11_5 = match (Tuple([Const(1),Const(2),Const(3),Const(4)]), TupleP ([Variable "x", Wildcard, Variable "abc", Wildcard])) = (SOME [("x", Const(1)),("abc", Const(3))])
val test11_6 = match (Constructor("s", Const(1)), ConstructorP("s", ConstP(1))) = (SOME [])
val test11_7 = match (Constructor("s", Const(1)), ConstructorP("b", ConstP(1))) = NONE
val test11_8 = match (Constructor("s", Const(1)), ConstructorP("s", ConstP(12))) = NONE

val test12 = first_match Unit [UnitP] = SOME []
val test12_1 = (first_match (Tuple([Const(1),Const(2),Const(3),Const(4)])) ([TupleP ([Variable "x", Wildcard, Variable "abc", Wildcard]), Wildcard])) = (SOME [("x", Const(1)),("abc", Const(3))])

