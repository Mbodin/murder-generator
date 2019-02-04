
type language = string

type 'a t = ('a * language, string) PMap.t

let empty = PMap.empty

let add m l o str = PMap.add (o, l) str m

let translate m o l =
  try Some (PMap.find (o, l) m)
  with Not_found -> None

