
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

let add_years (y, d, m) i =
    (y + i, d, m)

let leap_year y =
  if y mod 4 <> 0 then false
  else if y mod 100 <> 0 then true
  else y mod 400 = 0

let size_year y =
  if leap_year y then 366 else 365

let rec add_days (y, d, m) i =
  if d + i < 0 then
    add_days (add_years (y, d, m) (-1)) (i + size_year (y - 1))
  else if d + i < size_year y then
    (y, d + i, m)
  else add_days (add_years (y, d, m) 1) (i - size_year y)

let rec add_minutes (y, d, m) i =
  if m + i < 0 then
    let mp = -((-m - i) mod (60 * 24)) in
    add_days (y, d, mp) ((m + i) / 60 * 24 - 1)
  else if m + i < 60 * 24 then
    (y, d, m + i)
  else add_days (y, d, (m + i) mod (60 * 24)) ((m + i) / (60 * 24))

let compare (y1, d1, m1) (y2, d2, m2) =
  if y1 < y2 then -1
  else if y1 > y2 then 1
  else if d1 < d2 then -1
  else if d1 > d2 then 1
  else compare m1 m2

(** Returns the size of the months of a given year. **)
let months y =
  [ 31 ; if leap_year y then 29 else 28 ;
    31 ; 30 ; 31 ; 30 ; 31 ; 31 ; 30 ; 31 ; 30 ; 31 ]

(** Returns the month number and day of the month (starting from [1]). **)
let month_day (y, d, _) =
  let rec aux m d = function
    | [] -> assert false
    | c :: l ->
      if d < c then (m, d + 1)
      else aux (m + 1) (d - c) l in
  aux 1 d (months y)

let rfc2445 (y, d, m) =
  let (month, day) = month_day (y, d, m) in
  let y = Utils.positive_mod y 1000 in
  Utils.complete_string_pre "0" (string_of_int y) 4
  ^ Utils.complete_string_pre "0" (string_of_int month) 2
  ^ Utils.complete_string_pre "0" (string_of_int day) 2
  ^ "T"
  ^ Utils.complete_string_pre "0" (string_of_int (m / 60)) 2
  ^ Utils.complete_string_pre "0" (string_of_int (m mod 60)) 2
  ^ "00"

(** Dates in ord-mode are encapsulated in either square or angle brackets
 * dependending on whether they are active. **)
let orgmode_start active =
  if active then "<" else "["
let orgmode_end active =
  if active then ">" else "]"

let orgmode ?active:(active=false) (y, d, m) =
  let (month, day) = month_day (y, d, m) in
  orgmode_start active
  ^ Utils.complete_string_pre "0" (string_of_int y) 4
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int month) 2
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int day) 2
  ^ " _ " ^ string_of_int (m / 60)
  ^ ":" ^ string_of_int (m mod 60)
  ^ orgmode_end active

let orgmode_range ?active:(active=false) (y1, d1, m1) (y2, d2, m2) =
  let (month1, day1) = month_day (y1, d1, m1) in
  let (month2, day2) = month_day (y2, d2, m2) in
  if (y1, month1, day1) = (y2, month2, day2) then
    orgmode_start active
    ^ Utils.complete_string_pre "0" (string_of_int y1) 4
    ^ "-" ^ Utils.complete_string_pre "0" (string_of_int month1) 2
    ^ "-" ^ Utils.complete_string_pre "0" (string_of_int day1) 2
    ^ " _ " ^ string_of_int (m1 / 60)
    ^ ":" ^ string_of_int (m1 mod 60)
    ^ "-" ^ string_of_int (m2 / 60)
    ^ ":" ^ string_of_int (m2 mod 60)
    ^ orgmode_end active
  else orgmode (y1, d1, m1) ^ "--" ^ orgmode (y2, d2, m2)

