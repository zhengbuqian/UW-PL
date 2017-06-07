(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val date1 = (2000, 1, 1)
val date2 = (2000, 2, 1)
val date3 = (2000, 2, 4)
val date4 = (2000, 4, 1)
val date5 = (2001, 1, 1)
val date6 = (2001, 4, 3)


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test1_1 = is_older (date1, date2) = true
val test1_2 = is_older (date2, date3) = true
val test1_3 = is_older (date3, date4) = true
val test1_4 = is_older (date4, date5) = true
val test1_5 = is_older (date5, date6) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test2_1 = number_in_month ([date1, date2], 1) = 1
val test2_2 = number_in_month ([date1, date2, date3], 1) = 1
val test2_3 = number_in_month ([date1, date2, date3], 2) = 2
val test2_4 = number_in_month ([date1, date2, date3, date4], 2) = 2
val test2_5 = number_in_month ([date1, date2], 12) = 0

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test3_1 = number_in_months ([date1, date2], [1]) = 1
val test3_2 = number_in_months ([date1, date2], [1, 2]) = 2
val test3_3 = number_in_months ([date1, date2, date3], [1, 2]) = 3
val test3_4 = number_in_months ([date1, date2, date3], [1, 2, 4]) = 3
val test3_5 = number_in_months ([date1, date2, date3, date4], [1, 2, 4]) = 4
val test3_6 = number_in_months ([date1, date2], [12]) = 0

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test4_1 = dates_in_month ([date1, date2], 1) = [date1]
val test4_2 = dates_in_month ([date1, date2, date3], 2) = [date2, date3]
val test4_3 = dates_in_month ([date1, date2, date3, date4], 2) = [date2, date3]
val test4_4 = dates_in_month ([date1, date2, date3, date3], 12) = []

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test5_1 = dates_in_months ([date1, date2], [1]) = [date1]
val test5_2 = dates_in_months ([date1, date2, date3], [2]) = [date2, date3]
val test5_3 = dates_in_months ([date1, date2, date3, date4], [1, 2]) = [date1, date2, date3]
val test5_4 = dates_in_months ([date1, date2, date3, date3], [12]) = []

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 4) = "are"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7_1 = date_to_string (date1) = "January 1, 2000"
val test7_2 = date_to_string (date2) = "February 1, 2000"
val test7_3 = date_to_string (date6) = "April 3, 2001"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8_1 = number_before_reaching_sum (11, [1,2,3,4,5]) = 4
val test8_2 = number_before_reaching_sum (1, [1,2,3,4,5]) = 0

val test9 = what_month 70 = 3
val test9_1 = what_month 31 = 1
val test9_2 = what_month 32 = 2
val test9_3 = what_month 59 = 2
val test9_4 = what_month 60 = 3
val test9_5 = what_month 365 = 12

val test10 = month_range (31, 34) = [1,2,2,2]
val test10_1 = month_range (30, 31) = [1, 1]
val test10_2 = month_range (30, 34) = [1, 1, 2, 2, 2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_1 = oldest([(2012,2,28),(2011,3,31),(2010,4,28)]) = SOME (2010,4,28)
val test11_2 = oldest([(2010,4,28)]) = SOME (2010,4,28)
val test11_3 = oldest([]) = NONE

val test12_1 = number_in_months_challenge ([date1, date2, date3], [1, 1, 2, 4]) = 3
val test12_2 = dates_in_months_challenge ([date1, date2, date3, date4], [1, 1, 2]) = [date1, date2, date3]

val test13_1 = reasonable_date date1
val test13_2 = reasonable_date date2
val test13_3 = reasonable_date (0, 1, 2) = false
val test13_4 = reasonable_date (~1, 1, 2) = false
val test13_5 = reasonable_date (2010, 2, 29) = false
val test13_6 = reasonable_date (2000, 2, 29) = true
val test13_7 = reasonable_date (2100, 2, 29) = false
