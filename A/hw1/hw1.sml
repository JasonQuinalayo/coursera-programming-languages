fun is_older ( date1 : int * int * int, date2 : int * int * int ) =
  (#1 date1) < (#1 date2) orelse ((#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2)) 
                          orelse ((#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2)
                                                          andalso (#3 date1) < (#3 date2))

fun number_in_month ( datelist : (int * int * int) list, month : int ) = 
  if null datelist then 0 else 
    if (#2 (hd datelist)) = month then 1 + number_in_month(tl datelist, month)
    else number_in_month(tl datelist, month)

fun number_in_months ( datelist : (int * int * int) list, monthlist : int list ) =
  if null monthlist then 0 
  else number_in_month(datelist, hd monthlist) + number_in_months(datelist, tl monthlist)

fun dates_in_month ( datelist : (int * int * int) list, month : int ) =
  if null datelist then []
  else if (#2 (hd datelist)) = month then (hd datelist) :: dates_in_month(tl datelist, month)
  else dates_in_month(tl datelist, month)

fun dates_in_months ( datelist : (int * int * int) list, monthlist : int list ) =
  if null monthlist then []
  else dates_in_month(datelist, hd monthlist) @ dates_in_months(datelist, tl monthlist)

fun get_nth ( stringlist : string list,  n : int ) = 
  if n = 1 then hd stringlist else get_nth(tl stringlist, n - 1)

fun date_to_string ( date : int * int * int ) =
let 
  val dates = ["January", "February", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December"] 
in 
  get_nth(dates, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
end

fun number_before_reaching_sum ( sum : int, intlist : int list ) = 
let 
  fun helper ( previous_sum : int, remaining_numbers : int list, iter : int ) =
  let 
    val current_sum = previous_sum + (hd remaining_numbers)
  in 
    if current_sum >= sum then iter - 1
    else helper(current_sum, tl remaining_numbers, iter + 1)
  end
in
  helper(0, intlist, 1)
end

fun what_month ( day : int ) = 
let 
  val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
in 
  number_before_reaching_sum(day, months) + 1
end

fun month_range ( day1 : int, day2 : int )  =
  if day1 > day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest ( dates : (int * int * int) list ) =
let 
  fun helper ( helperdates : (int * int * int) list , oldest : int * int * int ) =
    if null helperdates then oldest 
    else 
      if is_older(hd helperdates, oldest) then helper(tl helperdates, hd helperdates) 
      else helper(tl helperdates, oldest)
in
  if null dates then NONE
  else SOME (helper(tl dates, hd dates))
end

fun check_for_dup ( cfdlist : int list, cfdmonth : int ) =
  if null cfdlist then false 
  else cfdmonth = hd cfdlist orelse check_for_dup(tl cfdlist, cfdmonth)
fun remove_dup ( rdmonthlist : int list ) =
  if null rdmonthlist then []
  else if not (check_for_dup(tl rdmonthlist, hd rdmonthlist))
  then (hd rdmonthlist) :: remove_dup(tl rdmonthlist)
  else remove_dup(tl rdmonthlist)

fun number_in_months_challenge ( datelist : (int * int * int) list, monthlist : int list ) = 
  number_in_months(datelist, remove_dup(monthlist))

fun dates_in_months_challenge ( datelist : (int * int * int) list, monthlist : int list ) =
  dates_in_months(datelist, remove_dup(monthlist))

fun reasonable_date ( date : int * int * int ) =
let 
  fun get_month_days ( months : int list, month : int ) = 
    if month = 1 then hd months else get_month_days(tl months, month - 1)
  fun check_if_leap( year: int ) = 
    year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
in
  if ((#1 date) > 0) andalso ((#2 date >= 1) andalso (#2 date <= 12)) 
  then 
  let
    val months = if check_if_leap(#1 date) then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                 else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 ,31]
    val month_days = get_month_days(months, #2 date)
  in
    (#3 date > 0) andalso (#3 date <= month_days)
  end
  else false
end

val test = reasonable_date(2100, 2, 29) = false

