
(** This type works like an automaton. **)
type 'a t = {
    init : unit -> 'a (** The initial state. **) ;
    transition : 'a -> string * 'a option
      (** A transition.
       * Returning [None] means that the transition halted. **)
  }

(** A more furnished automaton. **)
type 'a alternative = {
    alternative_size : int ; (** Expected size of the resulting words. **)
    alternative_init : (int * ('a * string)) list
      (** Wheighed list of initial states. **) ;
    alternative_transition : 'a -> (int * ('a * string)) list
      (** Wheighed list of transitions. **) ;
    alternative_final : 'a -> (int * string) list
      (** Wheighed list of final states. **)
  }

let convertAlternative spec =
  (** Decides whether it is time to halt the generation. **)
  let halt size =
    (** The basic decide function is simple, but we may want to alter
     * its distribution.**)
    let decide () =
      size <= 0 || Random.int size = 0 in
    if size > spec.alternative_size * 3 / 2 then
      decide () && decide ()
    else if size < spec.alternative_size / 2 then (
      if size <= 0 then
        Random.int 4 <> 0
      else if size < spec.alternative_size / 4 then
        decide () || decide ()
      else decide () || Random.int 5 = 0
    ) else decide () in {
    init = (fun _ ->
      let (s, str) = Utils.select spec.alternative_init in
      (spec.alternative_size, Some s, str)) ;
    transition = fun (size, s, str) ->
      match s with
      | None -> (str, None)
      | Some s ->
        (str,
          if halt size then
            Some (0, None, Utils.select (spec.alternative_final s))
          else
            let (s, str) = Utils.select (spec.alternative_transition s) in
            Some (size - 1, Some s, str))
  }


type vowelConsonant = int * bool option * string

let createVowelConsonant size initV initC middleV middleC endV endC =
  let get_spec f strspec =
    let rec aux = function
      | [] -> (1, [])
      | l :: ls ->
        let (weight, spec) = aux ls in
        let spec = List.map (fun str -> (weight, (f str))) l @ spec in
        (3 * weight, spec) in
    snd (aux (List.map (String.split_on_char ',')
      (String.split_on_char ';' strspec))) in
  let add b e = (b, e) in
  convertAlternative {
      alternative_size = size ;
      alternative_init = get_spec (add false) initV @ get_spec (add true) initC ;
      alternative_transition = 
        (let middleV = get_spec (add false) middleV in
         let middleC = get_spec (add true) middleC in
         fun b -> if b then middleV else middleC) ;
      alternative_final = 
        let endV = get_spec Utils.id endV in
        let endC = get_spec Utils.id endC in
        fun b -> if b then endV else endC
    }


let generate data =
  let rec aux str s =
    let (suffix, next) = data.transition s in
    let str = str ^ suffix in
    match next with
    | None -> str
    | Some s -> aux str s in
  aux "" (data.init ())

