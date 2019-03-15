
type language = string

let generic = ""

let iso639 = Utils.id
let from_iso639 = Utils.id

type 'a t = ('a * language, string) PMap.t

type element = {
    category : Utils.Id.t t ;
    attribute : State.attribute t ;
    constructor : State.constructor t
  }


let empty = PMap.empty

let empty_element = {
    category = empty ;
    attribute = empty ;
    constructor = empty
  }


let add m l o str = PMap.add (o, l) str m

let translate m o l =
  try Some (PMap.find (o, l) m)
  with Not_found -> None


let from_json fileName fileContent =
  match Yojson.Safe.from_string ~fname:fileName fileContent with
  | `List l ->
    Utils.list_fold_lefti (fun i (t, lgs) ->
        let current =
          "The " ^ string_of_int (1 + i) ^ "th element"
          ^ " of the file “" ^ fileName ^ "”" in function
        | `Assoc l ->
          let errorKey key =
            failwith (current ^ " associates the field “" ^ key
                      ^ "” to something else than a string.") in
          let lg =
            try match List.assoc "iso639" l with
                | `String lg -> lg
                | _ -> errorKey "iso639"
            with Not_found ->
              failwith (current ^ " has no key “iso639”.") in
          (List.fold_left (fun t -> function
            | key, `String str -> add t lg key str
            | (key, _) -> errorKey key) t l, lg :: lgs)
        | _ ->
          failwith (current ^ " is not an object.")) (empty, []) l
  | _ ->
    failwith ("The file “" ^ fileName ^ "” is not a list.")

