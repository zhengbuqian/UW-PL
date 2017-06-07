(* 2017.05.11, Maria Knorps *)
(* program for course homework 1 *)

fun is_older ( date1 : int*int*int, date2 : int*int*int) =
  if #1 date1 < #1 date2
  then true
  else  if #1 date1 = #1 date2 andalso #2 date1 < #2 date2
        then true
        else if #2 date1 = #2 date2 andalso #3 date1 < #3 date2
             then true
	     else false

fun number_in_month (ldates : (int*int*int) list, month :int) =
  if null ldates
  then 0
  else
  let
      fun count_a_match (a : int) =
	if a=month
	then 1
	else 0
  in   		 
      number_in_month( tl ldates,month) + count_a_match(#2( hd ldates))
  end
      
fun number_in_months (ldates : (int*int*int) list, months :int list) =
  if null months
  then 0
  else number_in_months (ldates, tl months) + number_in_month ( ldates, hd months)

fun dates_in_month (ldates : (int*int*int) list, month :int) =
  if null ldates
  then []
  else
  	if #2 (hd ldates) = month
	then hd ldates :: dates_in_month(tl ldates, month)
	else dates_in_month(tl ldates, month)

fun dates_in_months (ldates : (int*int*int) list, months :int list)=
  if null months
  then []
  else dates_in_month ( ldates, hd months) @  dates_in_months (ldates, tl months)


fun get_nth (lstr : string list, n :int) =
      if n=1
      then hd lstr
      else get_nth ( tl lstr,n-1)	  
     
fun date_to_string ( date1 : int*int*int) =
  let
      val months = ["January", "February", "March", "April",
		"May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date1) ^" "^ Int.toString(#3 date1)^ ", " ^ Int.toString(#1 date1)
  end
      
fun number_before_reaching_sum (sum : int, plist : int list ) =
      if hd plist >= sum
      then 0
      else 1 + number_before_reaching_sum ( sum - (hd plist), tl plist )				


fun what_month (day : int) =
  let
      val monthdays = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
       1 + number_before_reaching_sum (day, monthdays)
  end
      
fun month_range (day1 : int, day2 : int) =
  if day2 < day1
  then []
  else what_month(day1)::month_range (day1+1,day2)
				     
      
fun oldest ( dates : (int*int*int) list) =
  if null dates
  then
      NONE
  else if null ( tl dates)
  then
      SOME ( hd dates)
  else
	let
            val oldest_rest = oldest ( tl dates ) 
	in
	    if is_older(hd dates, valOf (oldest_rest))
	    then SOME (hd dates)
	    else oldest_rest
	end		
	  
      
  
