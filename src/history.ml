
type character = Id.t

type result =
  | Relation_event
    of character
       * Relation.t

type date =
  int
  * int
  * int

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

let compare_date (y1, d1, m1) (y2, d2, m2) =
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

let orgmode (y, d, m) =
  let (month, day) = month_day (y, d, m) in
  "[" ^ Utils.complete_string_pre "0" (string_of_int y) 4
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int month) 2
  ^ "-" ^ Utils.complete_string_pre "0" (string_of_int day) 2
  ^ " _ " ^ string_of_int (m / 60)
  ^ ":" ^ string_of_int (m mod 60) ^ "]"

let orgmode_range (y1, d1, m1) (y2, d2, m2) =
  let (month1, day1) = month_day (y1, d1, m1) in
  let (month2, day2) = month_day (y2, d2, m2) in
  if (y1, month1, day1) = (y2, month2, day2) then
    "[" ^ Utils.complete_string_pre "0" (string_of_int y1) 4
    ^ "-" ^ Utils.complete_string_pre "0" (string_of_int month1) 2
    ^ "-" ^ Utils.complete_string_pre "0" (string_of_int day1) 2
    ^ " _ " ^ string_of_int (m1 / 60)
    ^ ":" ^ string_of_int (m1 mod 60)
    ^ "-" ^ string_of_int (m2 / 60)
    ^ ":" ^ string_of_int (m2 mod 60) ^ "]"
  else orgmode (y1, d1, m1) ^ "--" ^ orgmode (y2, d2, m2)


type event_type =
    | For_life_event
    | Long_term_event
    | Medium_term_event
    | Short_term_event
    | Very_short_term_event
    | Immediate_event

type event = {
    event_begin : date ;
    event_end : date ;
    event_type : event_type ;
    event_attendees : character list
  }

let generate_event beg dur chars =
  let en =
    match dur with
    | For_life_event -> max_date
    | Long_term_event -> add_years beg (Utils.rand 1 10)
    | Medium_term_event -> add_days beg (Utils.rand 14 100)
    | Short_term_event -> add_days beg (Utils.rand 2 10)
    | Very_short_term_event -> add_minutes beg (Utils.rand 2 300)
    | Immediate_event -> beg
  in {
    event_begin = beg ;
    event_end = en ;
    event_type = dur ;
    event_attendees = chars
  }

let compatible_events e1 e2 =
  e1.event_type <> e2.event_type
  || compare e1.event_begin e2.event_end = 1
  || compare e2.event_begin e1.event_end = 1

type t = event list

let compatible el e =
  List.for_all (fun ep -> compatible_events e ep) el

type state = t array

let copy = Array.copy

let create_state n =
    Array.make n []

type final = state (* FIXME *)

let finalise = Utils.id (* TODO *)

