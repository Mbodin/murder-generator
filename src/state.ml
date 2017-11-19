
type character = Utils.idt

type relation_state =
  Relations.t array array

exception SelfRelation

let create_relation_state n =
  Array.init (n - 1) (fun i ->
    Array.make (n - 1 - i) (Relations.Basic Relations.Neutral, false))

let rec get_relation_state a c1 c2 =
  let c1 = Utils.idt_to_array c1 in
  let c2 = Utils.idt_to_array c2 in
  if c1 = c2 then
    raise SelfRelation
  else if c1 > c2 then
    Relations.reverse a.(c2).(c1)
  else a.(c1).(c2)

let write_relation_state a c1 c2 r =
  let c1 = Utils.idt_to_array c1 in
  let c2 = Utils.idt_to_array c2 in
  if c1 = c2 then
    raise SelfRelation
  else if c1 > c2 then
    a.(c2).(c1) <- Relations.reverse r
  else a.(c2).(c1) <- r

type t =
  relation_state * History.state

let get_relation (a, _) = get_relation_state a

let write_relation (a, _) = write_relation_state a

let create_state n =
  (create_relation_state n, History.create_state n)

