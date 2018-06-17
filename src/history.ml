
type character = Utils.Id.t

type result =
  | Relation_event
    of character
       * Relations.t

type date =
  int
  * int
  * int

let current_year = 0 (* TODO *)

let add_years (y, d, m) i =
    (y + i, d, m)

let leap_year y =
  let y = y + current_year in
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

type event_type =
    | For_life_event
    | Long_term_event
    | Medium_term_event
    | Short_term_event
    | Very_short_term_event
    | Instance_event

type event =
  date
  * date
  * result
  * event_type
  * character list

let max_date = (max_int / 2, 0, 0)

let generate_event beg dur res chars =
  let en =
    match dur with
    | For_life_event -> max_date
    | Long_term_event -> add_years beg (Utils.rand 1 10)
    | Medium_term_event -> add_days beg (Utils.rand 14 100)
    | Short_term_event -> add_days beg (Utils.rand 2 10)
    | Very_short_term_event -> add_minutes beg (Utils.rand 2 300)
    | Instance_event -> beg
  in (beg, en, res, dur, chars)

let order_event (y1, d1, m1) (y2, d2, m2) =
  if y1 < y2 then true
  else if y1 > y2 then false
  else if d1 < d2 then true
  else if d1 > d2 then false
  else m1 <= m2

let compatible_events (b1, e1, _, t1, _) (b2, e2, _, t2, _) =
  t1 <> t2 || order_event e1 b2 || order_event e2 b1

type t = event list

let compatible el e =
  List.for_all (fun ep -> compatible_events e ep) el

type state = t array

let create_state n =
    Array.make n []

