
type t =
  int (** Year, with years before the common era being offset by one. **)
  * int (** Day of the year, counting from [0]. **)
  * int (** Minute of the day, counting from [0]. **)

let now =
  let t = Unix.localtime (Unix.time ()) in
  (t.Unix.tm_year + 1900,
   t.Unix.tm_yday,
   t.Unix.tm_min + 60 * t.Unix.tm_hour)

let max_date = (max_int / 2, 0, 0)

let leap_year y =
  if y mod 4 <> 0 then false
  else if y mod 100 <> 0 then true
  else y mod 400 = 0

let size_year y =
  if leap_year y then 366 else 365

let add_years (y, d, m) i =
  let y = y + i in
  if d >= size_year y then
    (y + 1, d - size_year y, m)
  else (y, d, m)

let rec add_days (y, d, m) i =
  if d + i < 0 then
    add_days (add_years (y, 0, m) (-1)) (size_year y + d + i)
  else if d + i < size_year y then
    (y, d + i, m)
  else add_days (add_years (y, d, m) 1) (i - size_year y)

let rec add_minutes (y, d, m) i =
  if m + i < 0 then
    let mp = Utils.positive_mod (m + i) (60 * 24) in
    let dd = (m + i - mp) / (60 * 24) in
    add_days (y, d, mp) dd
  else if m + i < 60 * 24 then
    (y, d, m + i)
  else add_days (y, d, (m + i) mod (60 * 24)) ((m + i) / (60 * 24))

(** The functions of this file always make sure to return
 * a normalised date (with days between 0 and [size_year y - 1]
 * and minutes between [0] and [24 * 60].
 * This function effectively normalises an unnormalised date so
 * that functions that might produce non-normalised intermediate
 * dates can normalise them. **)
let normalise d = add_years (add_days (add_minutes d 0) 0) 0

let compare (y1, d1, m1) (y2, d2, m2) =
  if y1 < y2 then -1
  else if y1 > y2 then 1
  else if d1 < d2 then -1
  else if d1 > d2 then 1
  else compare m1 m2

let min d1 d2 =
  if compare d1 d2 < 0 then d1 else d2

let max d1 d2 =
  if compare d1 d2 < 0 then d2 else d1

(** Return the size of the months of a given year. **)
let months y =
  [ 31 ; if leap_year y then 29 else 28 ;
    31 ; 30 ; 31 ; 30 ; 31 ; 31 ; 30 ; 31 ; 30 ; 31 ]

(** Return the month number and day of the month (both starting from [1]). **)
let month_day (y, d, _) =
  let rec aux m d = function
    | [] -> assert false
    | c :: l ->
      if d < c then (m, d + 1)
      else aux (m + 1) (d - c) l in
  aux 1 d (months y)

(** Return the year and day of the year from a year (starting from [0]),
 * a month, and a day of the month (starting from [1]). **)
let rec month_day_inv y m d =
  if m < 1 then
    month_day_inv (y - 1) (m + 12) d
  else
    let rec aux d m = function
      | [] -> month_day_inv (1 + y) m d
      | c :: l ->
        if m = 1 then (y, d - 1)
        else aux (d + c) (m - 1) l in
    aux d m (months y)

let year (y, _, _) = y

let month d =
  fst (month_day d)

let day d =
  snd (month_day d)

let hour (_, _, m) = m / 60

let minute (_, _, m) = m mod 60

let iso8601 (y, d, m) =
  let (month, day) = month_day (y, d, m) in
  let y = Utils.positive_mod y 10_000 in
  Utils.complete_string_pre "0" (string_of_int y) 4
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int month) 2
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int day) 2

(** Take a number [n] and a list [l] of strings.
 * This function tries to read a number of size up to [n]
 * in base [10] out of the first element of the list [l],
 * and returns the parsed number with the rest of the list.
 * If the first element of [l] is larger than this number,
 * this first string is splitted to only extract the expected
 * number of characters.
 * The idea behind this function is that a first string has
 * been splitted by looking for non-numerical characters.
 * In the case that these non-numerical characters are not
 * present, then the size is used as an indicator. **)
let rec get_integer n = function
  | [] -> (1, [])
  | str :: l ->
    try
      if String.length str > n then
        (int_of_string (String.sub str 0 n),
         String.sub str n (String.length str - n) :: l)
      else (int_of_string str, l)
    with _ -> (1, l)

let from_iso8601 str =
  if str = "" then now
  else
    let (year, month, day) =
      match String.split_on_char '-' str with
      | year :: month :: day :: [] ->
        (int_of_string year, int_of_string month, int_of_string day)
      | l ->
        let (year, l) = get_integer 4 l in
        let (month, l) = get_integer 2 l in
        let (day, l) = get_integer 2 l in
        (year, month, day) in
    let (year, d) = month_day_inv year month day in
    normalise (year, d, 0)

let rfc2445 (y, d, m) =
  let (month, day) = month_day (y, d, m) in
  let y = Utils.positive_mod y 10_000 in
  Utils.complete_string_pre "0" (string_of_int y) 4
  ^ Utils.complete_string_pre "0" (string_of_int month) 2
  ^ Utils.complete_string_pre "0" (string_of_int day) 2
  ^ "T"
  ^ Utils.complete_string_pre "0" (string_of_int (m / 60)) 2
  ^ Utils.complete_string_pre "0" (string_of_int (m mod 60)) 2
  ^ "00"

let from_rfc2445 str =
  if str = "" then now
  else
    let parse_time str =
      match String.split_on_char '-' str with
      | hour :: minute :: _ -> (int_of_string hour, int_of_string minute)
      | l ->
        let (hour, l) = get_integer 2 l in
        let (minute, l) = get_integer 2 l in
        (hour, minute) in
    let (date, time) =
      match String.split_on_char 'T' str with
      | [] -> assert false
      | date :: time :: [] -> (date, time)
      | str :: [] ->
        if String.length str >= 8 then
          (String.sub str 0 8, String.sub str 8 (String.length str - 8))
        else (str, "")
      | _ -> failwith ("Unable to parse date: " ^ str ^ ".") in
    let (y, d, _) = from_iso8601 date in
    let (h, m) = parse_time time in
    (y, d, 60 * h + m)

let timeline (y, d, m) =
  let (month, day) = month_day (y, d, m) in
  let y = Utils.positive_mod y 10_000 in
  Utils.complete_string_pre "0" (string_of_int y) 4
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int month) 2
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int day) 2
  ^ " "
  ^ Utils.complete_string_pre "0" (string_of_int (m / 60)) 2
  ^ ":" ^ Utils.complete_string_pre "0" (string_of_int (m mod 60)) 2
  ^ ":00"

(** Dates in ord-mode are encapsulated in either square or angle brackets
 * dependending on whether they are active.
 * These functions choose the right enclosing symbols accordingly. **)
let orgmode_start active =
  if active then "<" else "["
let orgmode_end active =
  if active then ">" else "]"

let orgmode ?(active = false) (y, d, m) =
  let (month, day) = month_day (y, d, m) in
  orgmode_start active
  ^ Utils.complete_string_pre "0" (string_of_int y) 4
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int month) 2
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int day) 2
  ^ " _ " ^ string_of_int (m / 60)
  ^ ":" ^ Utils.complete_string_pre "0" (string_of_int (m mod 60)) 2
  ^ orgmode_end active

let orgmode_range ?(active = false) (y1, d1, m1) (y2, d2, m2) =
  if (y1, d1, m1) = (y2, d2, m2) then
    orgmode ~active:false (y1, d1, m1)
  else
    let (month1, day1) = month_day (y1, d1, m1) in
    let (month2, day2) = month_day (y2, d2, m2) in
    if (y1, month1, day1) = (y2, month2, day2) then
      orgmode_start active
      ^ Utils.complete_string_pre "0" (string_of_int y1) 4
      ^ "-" ^ Utils.complete_string_pre "0" (string_of_int month1) 2
      ^ "-" ^ Utils.complete_string_pre "0" (string_of_int day1) 2
      ^ " _ " ^ string_of_int (m1 / 60)
      ^ ":" ^ Utils.complete_string_pre "0" (string_of_int (m1 mod 60)) 2
      ^ "-" ^ string_of_int (m2 / 60)
      ^ ":" ^ Utils.complete_string_pre "0" (string_of_int (m2 mod 60)) 2
      ^ orgmode_end active
    else orgmode (y1, d1, m1) ^ "--" ^ orgmode (y2, d2, m2)

