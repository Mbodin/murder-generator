
type t = int

let new_id_function _ =
  let current = ref (-1) in
  fun _ ->
    incr current ;
    !current

let new_id = new_id_function ()

let to_array = Utils.id
let from_array = Utils.id

type 'a map = {
    forwards : ('a, t) PMap.t (** Forwards map **) ;
    reverse : (t, 'a) PMap.t (** Reverse map **) ;
    fresh : int (** Fresh identifier **)
  }

let map_create ?(compare = compare) _ = {
    forwards = PMap.create compare ;
    reverse = PMap.empty ;
    fresh = 0
  }

let map_insert_t m o =
  try
    let i = PMap.find o m.forwards in
    (i, m)
  with Not_found ->
    let i = m.fresh in
    let m = {
        forwards = PMap.add o i m.forwards ;
        reverse = PMap.add i o m.reverse ;
        fresh = 1 + m.fresh
      } in
    (i, m)

let map_insert m e =
  snd (map_insert_t m e)

let get_id m o =
  try Some (PMap.find o m.forwards)
  with Not_found -> None

let map_inverse m i =
  try Some (PMap.find i m.reverse)
  with Not_found -> None

let map_fold f i m =
  PMap.foldi f m.forwards i

