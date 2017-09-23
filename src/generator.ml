
type character = Utils.idt

type relation_state =
  Relation.t array array

exception SelfRelation

let create_relation_state n =
  Array.init (n - 1) (fun i ->
    Array.make (n - 1 - i) (Relation.Basic Relation.Neutral))

let rec get_relation a c1 c2 =
  let c1 = Utils.idt_to_array c1 in
  let c2 = Utils.idt_to_array c2 in
  if c1 = c2 then
    raise SelfRelation
  else if c1 > c2 then
    Relation.reverse a.(c2).(c1)
  else a.(c1).(c2)

let write_relation a c1 c2 r =
  let c1 = Utils.idt_to_array c1 in
  let c2 = Utils.idt_to_array c2 in
  if c1 = c2 then
    raise SelfRelation
  else if c1 > c2 then
    a.(c2).(c1) <- Relation.reverse r
  else a.(c2).(c1) <- r

type state =
    relation_state * History.state

let generator = TODO

