fun append (xs: 'a list, ys: 'a list) = 
  if null xs then ys
  else (hd xs) :: append(tl xs, ys)

fun is_older(d1: int * int * int, d2: int * int * int) = 
  if (#1 d1) > (#1 d2) then false
    else if (#1 d1) < (#1 d2) then true
    else (* same year *)
      if (#2 d1) > (#2 d2) then false
      else if (#2 d1) < (#2 d2) then true
      else (* same month *)
        if (#3 d1) >= (#3 d2) then false
          else true

fun number_in_month (dates: (int * int * int) list, month: int) =
  if null dates then 0 
  else
    let
      val tal = number_in_month(tl dates, month)
    in
      if (month = #2 (hd dates)) then 1 + tal else tal
    end

fun number_in_months (dates: (int * int * int) list, months: int list) =
  if null dates then 0 
  else if null months then 0 
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates: (int * int * int) list, month: int) =
  if null dates then []
  else 
    if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates: (int * int * int) list, months: int list) =
  if null dates then []
  else if null months then []
  else 
    append(dates_in_month(dates, hd months), dates_in_months(dates, tl months))

fun get_nth (xs: string list, index: int) =
  if index = 1 then hd xs
  else get_nth (tl xs, index - 1)

fun date_to_string (date: (int * int * int)) = 
  let
    val names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum: int, nums: int list) =
  if sum <= hd nums then 0
  else 
    1 + number_before_reaching_sum(sum - (hd nums), tl nums)

fun what_month(day: int) =
  let
    val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    1 + number_before_reaching_sum(day, days)
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2 then []
  else 
    what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
  if null dates then NONE
  else 
    let
      fun oldest_nonnull(dates: (int * int * int) list) = 
        if null (tl dates) then hd dates
        else 
          let
            val remain_oldest = oldest_nonnull(tl dates)
          in
            if is_older(hd dates, remain_oldest)
            then hd dates else remain_oldest
          end
    in
      SOME (oldest_nonnull(dates))
    end

fun deduplicate(nums: int list) = 
  if null nums then [] else
  let
    fun sublist_without(ns: int list, n) =
      if null ns then []
      else 
        if (hd ns) = n 
        then sublist_without(tl ns, n)
        else (hd ns) :: sublist_without(tl ns, n)
  in
    (hd nums) :: sublist_without(deduplicate(tl nums), hd nums)
  end
  
fun number_in_months_challenge(dates: (int * int * int) list, months: int list) =
  number_in_months(dates, deduplicate(months))

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
  dates_in_months(dates, deduplicate(months))

fun reasonable_date(date: (int * int * int))=
  if #1 date <= 0 then false
  else if #2 date < 1 orelse #2 date > 12 then false
  else 
  let
    val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    fun get_nth(nums:int list, n: int) = 
      if n = 1 then hd nums
      else get_nth(tl nums, n - 1)
    fun is_leap() =
      if #1 date mod 100 <> 0 
      then #1 date mod 4 = 0
      else #1 date mod 400 = 0
    fun get_num_days() = 
      if #2 date <> 2 then get_nth(days, #2 date)
      else 
        if is_leap() then 29 else 28
  in
    #3 date >= 1 andalso #3 date <= get_num_days()
  end