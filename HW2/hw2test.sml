(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_1 = all_except_option ("string", []) = NONE
val test1_2 = all_except_option ("string", ["nihao", "hi", "no"]) = NONE
val test1_3 = all_except_option ("string", ["nihao", "string", "hi", "no"]) = SOME ["nihao", "hi", "no"]

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_1 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test2_2 = get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffroy"],["Nice","Jeffray"]],"Jeff") = ["Jeffroy"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_1 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test3_2 = get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffroy"],["Nice","Jeffray"]],"Jeff") = ["Jeffroy"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black
val test5_1 = card_color (Spades, Queen) = Black
val test5_2 = card_color (Hearts, King) = Red
val test5_3 = card_color (Diamonds, Ace) = Red

val test6 = card_value (Clubs, Num 2) = 2
val test6_1 = card_value (Spades, Queen) = 10
val test6_2 = card_value (Hearts, King) = 10
val test6_3 = card_value (Diamonds, Ace) = 11
val test6_4 = card_value (Diamonds, Num 9) = 9

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_1 = remove_card ([(Hearts, Ace), (Spades, Num 9)], (Hearts, Ace), IllegalMove) = [(Spades, Num 9)]
val test7_2 = remove_card ([(Diamonds, Ace), (Hearts, Ace), (Hearts, King)], (Hearts, Ace), IllegalMove) = [(Diamonds, Ace), (Hearts, King)]

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_1 = all_same_color [(Spades, Ace), (Hearts, Ace), (Hearts, King)] = false
val test8_2 = all_same_color [(Diamonds, Ace), (Diamonds, Num 1), (Diamonds, King)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9_1 = sum_cards [(Spades, Ace), (Hearts, Ace), (Hearts, King)] = 32
val test9_2 = sum_cards [(Diamonds, Ace), (Diamonds, Num 1), (Diamonds, King)] = 22

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4  (*not same color, smaller than goal*)
val test10_1 = score ([(Hearts, Num 9),(Clubs, Num 4)],10) = 9  (*not same color, bigger than goal*)
val test10_2 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2  (*all same color, smaller than goal*)
val test10_3 = score ([(Hearts, Num 9),(Diamonds, Num 4)],10) = 4  (*all same color, bigger than goal*)

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test11_1 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42) = 3
val test11_2 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) handle IllegalMove => true)
val test11_3 = officiate ([(Clubs,Num 2),(Spades,Num 9),(Diamonds,Num 7)],[Draw,Draw,Draw],10) = 1
val test11_4 = officiate ([(Clubs,Num 2),(Spades,Num 8),(Diamonds,Num 7)],[Draw,Draw,Draw],10) = 21

(*test of score challenge with no Ace*)
val test12 = score_challenge ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4  
val test12_1 = score_challenge ([(Hearts, Num 9),(Clubs, Num 4)],10) = 9  
val test12_2 = score_challenge ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2 
val test12_3 = score_challenge ([(Hearts, Num 9),(Diamonds, Num 4)],10) = 4 
(*test of score challenge with 1 Ace*)
val test12_4 = score_challenge ([(Hearts, Num 2),(Clubs, Num 4),(Spades,Ace)],10) = 3
val test12_5 = score_challenge ([(Hearts, Num 9),(Clubs, Num 4),(Spades,Ace)],10) = 12  
val test12_6 = score_challenge ([(Hearts, Num 2),(Diamonds, Num 4),(Hearts,Ace)],10) = 1
val test12_7 = score_challenge ([(Hearts, Num 9),(Diamonds, Num 4),(Hearts,Ace)],10) = 6
val test12_8 = score_challenge ([(Hearts, Num 1),(Diamonds, Num 1),(Hearts,Ace)],12) = 1

val test13 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test13_1 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42) = 3
val test13_2 = ((officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) handle IllegalMove => true)
val test13_3 = officiate_challenge ([(Clubs,Num 2),(Spades,Num 9),(Diamonds,Num 7)],[Draw,Draw,Draw],10) = 1
val test13_4 = officiate_challenge ([(Clubs,Num 2),(Spades,Num 8),(Diamonds,Num 7)],[Draw,Draw,Draw],10) = 21
val test13_5 = officiate_challenge ([(Clubs,Ace),(Spades,Num 8),(Diamonds,Num 7)],[Draw,Draw,Draw],10) = 18
val test13_6 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Diamonds,Num 7)],[Draw,Draw,Draw],10) = 1
