val test1_1 = alternate([1,2,3,4]) = ~2

val test2_1 = min_max([1]) = (1, 1)
val test2_2 = min_max([1, 2, 3]) = (1, 3)
val test2_3 = min_max([2, 3, 5, 11, 0]) = (0, 11)

val test3_1 = cumsum([1, 4, 20]) = [1, 5, 25]

val test4_1 = greeting(NONE) = "𝙷𝚎𝚕𝚕𝚘 𝚝𝚑𝚎𝚛𝚎, you!"
val test4_2 = greeting(SOME "roy") = "Hello there, roy!"

val test5_1 = repeat([1, 2, 3], [4, 0, 3]) = [1, 1, 1, 1, 3, 3, 3]

val test6_1 = addOpt([SOME 1, NONE, SOME 3]) = SOME 4
val test6_2 = addOpt([NONE, NONE, NONE]) = NONE

val test8_1 = any([false, false, false, false]) = false
val test8_2 = any([false, true, false, false]) = true
val test8_3 = any([true, true, true, true]) = true

val test9_1 = all([true, true, true, true]) = true
val test9_2 = all([false, false, false, false]) = false
val test9_3 = all([false, true, false, false]) = false

val test10_1 = zip([1,2,3],[4,6]) = [(1,4),(2,6)]
val test10_2 = zip([1,2,3,4],[4,6]) = [(1,4),(2,6)]
val test10_3 = zip([1,2],[4,6,7]) = [(1,4),(2,6)]
val test10_4 = zip([1,2],[4,6]) = [(1,4),(2,6)]

val test11_1 = zipRecycle([1,2],[4,6,7]) = [(1,4),(2,6),(1,7)]
val test11_2 = zipRecycle2([1,2],[4,6,7]) = [(1,4),(2,6),(1,7)]

val test12_1 = zipOpt([1,2],[4,6,7]) = NONE
val test12_2 = zipOpt([1,2],[4,6]) = SOME [(1,4),(2,6)]

val test13_1 = lookup([("a",2),("c",112)], "c") = SOME 112
val test13_2 = lookup([("a",2),("c",112)], "d") = NONE

val test14_1 = splitup([1,3,~3,4,~9,0]) = ([1,3,4,0],[~3,~9])

val test15_1 = splitAt([1,3,~3,4,~9,0], 0) = ([1,3,4,0],[~3,~9])
val test15_2 = splitAt([1,3,~3,4,~9,0], 1) = ([1,3,4],[~3,~9,0])
val test15_3 = splitAt([1,3,~3,4,~9,0,5], 2) = ([3,4,5],[1,~3,~9,0])

val test16_1 = isSorted([~231,~1,1,2,3,4,5,110,321]) = true
val test16_2 = isSorted([~231,0,1,2,3,~2,5,110,321]) = false

val test17_1 = isAnySorted([~231,~1,1,2,3,4,5,110,321]) = true
val test17_2 = isAnySorted([~231,0,1,2,3,~2,5,110,321]) = false
val test17_3 = isAnySorted([321,22,20,1,0,~2]) = true

val test18_1 = sortedMerge([1,4,7],[2,8,9,10]) = [1,2,4,7,8,9,10]

val test19_1 = qsort([1,3,100,29,33,12,78,45,33,25,16]) = [1,3,12,16,25,29,33,33,45,78,100]

val test20_1 = divide([1,2,3,4,5,6,7]) = ([1,3,5,7],[2,4,6])

val test21_1 = not_so_quick_sort([1,3,100,29,33,12,78,45,33,25,16]) = [1,3,12,16,25,29,33,33,45,78,100]

val test22_1 = fullDivide(2,40) = (3,5)
val test22_2 = fullDivide(3,10) = (0,10)

val test23_1 = factorize(20) = [(2,2),(5,1)]
val test23_2 = factorize(36) = [(2,2),(3,2)]
val test23_3 = factorize(270) = [(2,1),(3,3), (5,1)]

val test24_1 = multiply([(2,1),(3,3), (5,1)]) = 270
val test24_2 = multiply([(2,2),(3,2)]) = 36
val test24_3 = multiply([(2,2),(5,1)]) = 20