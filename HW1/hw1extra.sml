fun append (xs: 'a list, ys: 'a list) = 
  if null xs then ys
  else (hd xs) :: append(tl xs, ys)
fun alternate(nums: int list) = 
  if null nums then 0 else
  hd nums - alternate(tl nums)
  (* 1-2+3-4 = 1-(2-3+4) = 1-(2-(3-4)) *)
fun min_max(xs: int list) =
  if null (tl xs) then (hd xs, hd xs) else
  let
    val head = hd xs
    val tmp = min_max(tl xs)
    val min = if (#1 tmp) < head then (#1 tmp) else head
    val max = if (#2 tmp) > head then (#2 tmp) else head
  in
    (min, max)
  end
fun cumsum(xs: int list) =
  if null xs then [] else 
  let
    fun cumsum_with (ys: int list, y: int) =
      if null ys then [] else 
      (hd ys + y) :: cumsum_with(tl ys, hd ys + y)
  in
    (hd xs) :: cumsum_with(tl xs, hd xs)
  end
fun greeting(name: string option) =
  if isSome name then "Hello there, " ^ (valOf name) ^ "!"
  else "𝙷𝚎𝚕𝚕𝚘 𝚝𝚑𝚎𝚛𝚎, you!"
fun repeat(xs: int list, ys: int list) =
  let
    fun n2l(x: int, y: int) = 
      (*number to list, number x repeats y times. requires y >= 0*)
      if y = 0 then [] else x :: n2l(x, y - 1)
  in
    if null xs then [] else
    append(n2l(hd xs, hd ys), repeat(tl xs, tl ys))
  end

fun addOpt(xs: int option list) = 
  if null xs then NONE else 
  let
    fun addSome(a: int option, b: int option) = 
      if (not (isSome a)) andalso (not (isSome b)) then NONE else
      let
        val val_a = if isSome a then valOf a else 0
        val val_b = if isSome b then valOf b else 0
      in
        SOME (val_a + val_b)
      end
    val tail = addOpt(tl xs)
  in
    addSome(hd xs, addOpt(tl xs))
  end
fun any(xs: bool list) =
  if null xs then false else
  hd xs orelse any(tl xs)
fun all(xs: bool list) = 
  if null xs then true else
  hd xs andalso all(tl xs)
fun zip(xs: int list, ys: int list) = 
  if null xs orelse null ys then [] else
  (hd xs, hd ys) :: zip(tl xs, tl ys)
fun list_length(xs: 'a list) =
  if null xs then 0 else
  1 + list_length(tl xs)
fun zipRecycle(xs: int list, ys: int list) =
  if null xs orelse null ys then [] else
  let
    val lx = list_length xs
    val ly = list_length ys
    fun extend(xss: int list, l: int) =
      if list_length xss >= l then xss else
      extend(xss @ xss, l)
  in
    if lx = ly then zip(xs, ys) else
    if lx < ly then zip(extend(xs, ly), ys) else
    zip(xs, extend(ys, lx))
  end
fun zipRecycle2(xs: int list, ys: int list) = 
  let
    fun helper(x: int list, y: int list, len: int) =
      if len = 0 then [] else
      if null x then helper(xs, y, len) else 
      if null y then helper(x, ys, len) else
      (hd x, hd y) :: helper(tl x, tl y, len - 1)
    val lx = list_length xs
    val ly = list_length ys
  in
    helper(xs, ys, if lx > ly then lx else ly)
  end
fun zipOpt(xs: int list, ys: int list) =
  if list_length xs <> list_length ys then NONE
  else SOME (zip(xs, ys))
fun lookup(xs: (string * int) list, y: string) =
  if null xs then NONE else
  if #1 (hd xs) = y then SOME (#2 (hd xs)) else
  lookup(tl xs, y)
fun splitAt(xs: int list, y: int) =
  if null xs then ([],[]) else
  let
    val head = hd xs
    val tail_result = splitAt(tl xs, y)
  in
    if head < y then (#1 tail_result, head :: (#2 tail_result))
    else (head :: (#1 tail_result), #2 tail_result)
  end
fun splitup(xs: int list) =
  splitAt(xs, 0)
fun isSorted(xs: int list) =
  if null (tl xs) then true else
  ((hd xs) <= (hd (tl xs))) andalso (isSorted(tl xs))
fun isAnySorted(xs: int list) =
  if null (tl xs) then true else
  let
    fun isDecrSorted(xs: int list) =
      if null (tl xs) then true else
      ((hd xs) >= (hd (tl xs))) andalso (isDecrSorted(tl xs))
  in
    if ((hd xs) >= (hd (tl xs))) then isDecrSorted(xs)
    else isSorted(xs)
  end
fun sortedMerge(xs: int list, ys: int list) =
  if null xs then ys else if null ys then xs else
  if hd xs <= hd ys then
  hd xs :: sortedMerge(tl xs, ys)
  else
  hd ys :: sortedMerge(xs, tl ys)
fun qsort(xs: int list) =
  if null xs then [] else
  let
    val split = splitAt(tl xs, hd xs)
  in
    qsort(#2 split) @ [hd xs] @ qsort(#1 split)
  end
fun divide(xs: int list) =
  if null xs then ([],[]) else
  if null (tl xs) then (xs, []) else
  let
    val tail = divide(tl (tl xs))
    val first_half = #1 tail
    val second_half = #2 tail
  in
    (hd xs :: first_half, hd (tl xs) :: second_half)
  end
fun not_so_quick_sort(xs: int list) =
  if null xs then [] else
  if null (tl xs) then xs else
  let
    val divi = divide(xs)
    val first_half = #1 divi
    val second_half = #2 divi
  in
    sortedMerge(not_so_quick_sort(first_half), not_so_quick_sort(second_half))
  end
fun fullDivide(k: int, n: int) =
  (if n mod k <> 0 then (0, n) else
  let
    val tail_result = fullDivide(k, n div k)
  in
    (1+(#1 tail_result), #2 tail_result)
  end)
fun factorize(x: int) = 
  let
    fun sqrt(n: int) =
      let
        fun helper(num: int, l: int) =
          if l * l >= num then l else helper(num, l+1)
      in
        helper(n, 1)
      end
    val square_root = sqrt(x)
    fun helper(x: int, n: int) =
      let
        val divi = fullDivide(n, x)
      in
        if n > square_root then [] else
        if #1 divi <> 0 then
        (n, #1 divi) :: helper(#2 divi, n+1)
        else helper(x, n+1)
      end
  in
    helper(x, 2)
  end
fun multiply(xs: (int * int) list) =
  let
    fun exp(a: int, b: int) =
      if b = 0 then 1 else a * exp(a,b-1)
  in
    if null (tl xs) then exp(#1 (hd xs), #2 (hd xs)) else
    exp(#1 (hd xs), #2 (hd xs)) * multiply(tl xs)
  end
fun all_products(xs: (int * int) list) =
  let
    val l = list_length xs
    fun expand(ns: (int * int) list) =
      
  in
    
  end