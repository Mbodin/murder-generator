
type basic =
  | Neutral
  | Hate
  | Trust
  | Chaotic
  | Undetermined
  | Avoidance

type relation =
  | Basic of basic
  | Asymmetrical of basic * basic
  | Explosive of relation * relation

type t = relation * bool

let is_explosive = function
  | (Explosive (_, _), _) -> true
  | _ -> false

let is_strong = snd

let basic_to_string = function
  | Neutral -> "neutral"
  | Hate -> "hate"
  | Trust -> "trust"
  | Chaotic -> "chaotic"
  | Undetermined -> "undetermined"
  | Avoidance -> "avoidance"

let rec relation_to_string = function
  | Basic r -> basic_to_string r
  | Asymmetrical (r1, r2) ->
    "asymmetrical " ^ basic_to_string r1 ^ " " ^ basic_to_string r2
  | Explosive (r1, r2) ->
    "explosive " ^ relation_to_string r1 ^ " " ^ relation_to_string r2

let to_string (r, s) =
  (if s then "strong " else "") ^ relation_to_string r


let basic_complexity = function
  | Neutral -> 0
  | Hate -> 1
  | Trust -> 1
  | Chaotic -> 1
  | Undetermined -> 2
  | Avoidance -> 1

let rec relation_complexity = function
  | Basic r -> basic_complexity r
  | Asymmetrical (r1, r2) -> max (basic_complexity r1) (basic_complexity r2)
  | Explosive (r1, r2) -> 1 + relation_complexity r1 + relation_complexity r2

let complexity (r, s) =
  let c = relation_complexity r in
  if s then c + c / 2 else c

let basic_difficulty = function
  | Neutral -> 0
  | Hate -> 2
  | Trust -> -1
  | Chaotic -> 1
  | Undetermined -> 1
  | Avoidance -> 1

let rec relation_difficulty = function
  | Basic r -> basic_difficulty r
  | Asymmetrical (Neutral, r) | Asymmetrical (r, Neutral) -> basic_difficulty r - 1
  | Asymmetrical (r1, r2) -> max (basic_difficulty r1) (basic_difficulty r2)
  | Explosive (r1, r2) -> 1 + relation_difficulty r1 + relation_difficulty r2

let difficulty (r, s) =
  let c = relation_difficulty r in
  if s then 2 * c else c

let normalise r =
  let rec aux l = function
    | Explosive (r1, r2) ->
      aux (aux l r1) r2
    | Basic b -> Utils.Left b :: l
    | Asymmetrical (b1, b2) -> Utils.Right (b1, b2) :: l in
  match List.sort compare (aux [] r) with
  | x :: l ->
    let f = function
      | Utils.Left b -> Basic b
      | Utils.Right (b1, b2) -> Asymmetrical (b1, b2) in
    List.fold_left (fun r x -> Explosive (f x, r)) (f x) l
  | [] -> assert false

let simplify r =
  let rec aux l = function
    | Explosive (r1, r2) ->
      aux (aux l r1) r2
    | Basic b -> Utils.Left b :: Utils.Right b :: l
    | Asymmetrical (b1, b2) -> Utils.Left b1 :: Utils.Right b2 :: l
  in let extract = function
    | Utils.Left r | Utils.Right r -> r
  in let l =
    List.sort_uniq compare (List.filter (fun r -> extract r <> Neutral) (aux [] r)) in
  (* LATER: Maybe it would be better to first consider all relations that pairs
   * (for which next_exact returns something), and in a second pass, consider the rest. *)
  match l with
  | x :: l ->
    let different_type x y =
      match x, y with
      | Utils.Left _, Utils.Right _ | Utils.Right _, Utils.Left _ -> true
      | Utils.Left _, Utils.Left _ | Utils.Right _, Utils.Right _ -> false
    in let rec next_different x = function
      | y :: l when different_type x y -> (extract y, l)
      | y :: l ->
        let (z, l) = next_different x l in (z, y :: l)
      | [] ->
        (* The corresponding pair has been removed because of duplication. *)
        (Neutral, [])
    in let rec next_exact x = function
      | [] -> None
      | y :: l when x = y -> Some (extract y, l)
      | y :: l ->
        Option.map (fun (z, l) -> (z, y :: l)) (next_exact x l)
    in let next x l =
      match next_exact x l with
      | Some v -> v
      | None -> next_different x l
    in let rec aux r = function
      | [] -> r
      | x :: l ->
        let (y, l) = next x l in
        let (x, y) =
          match x with
          | Utils.Left x -> (x, y)
          | Utils.Right x -> (y, x)
        in let r =
          if x = y then
            if x = Neutral then r
            else Explosive (r, Basic x)
          else Explosive (r, Asymmetrical (x, y)) in
        aux r l
    in let (y, l) =
      next x l
    in let (x, y) =
      match x with
      | Utils.Left x -> (x, y)
      | Utils.Right x -> (y, x)
    in let r =
          if x = y then
            Basic x
          else Asymmetrical (x, y) in
    aux r l
  | [] -> Basic Neutral

let rec left_relation_projection = function
  | Basic r -> Basic r
  | Asymmetrical (r, _) -> Basic r
  | Explosive (r1, r2) -> Explosive (left_relation_projection r1, left_relation_projection r2)

let left_projection (r, s) =
  (left_relation_projection r, s)

let rec right_relation_projection = function
  | Basic r -> Basic r
  | Asymmetrical (_, r) -> Basic r
  | Explosive (r1, r2) -> Explosive (right_relation_projection r1, right_relation_projection r2)

let right_projection (r, s) =
  (right_relation_projection r, s)

let compose_basic r1 r2 =
  match r1, r2 with
  | Neutral, _ -> (Some r2, false)
  | _, Neutral -> (Some r1, false)
  | _, _ when r1 = r2 -> (Some r1, true)
  | Chaotic, _ -> (Some r2, false)
  | _, Chaotic -> (Some r1, false)
  | Undetermined, Hate | Hate, Undetermined -> (Some Hate, false)
  | Undetermined, Trust | Trust, Undetermined -> (Some Trust, false)
  | Avoidance, Hate | Hate, Avoidance -> (Some Hate, true)
  | Trust, Hate | Hate, Trust -> (None, true)
  | _, _ -> (None, false)

let not_to_be_forgotten = function
  | Hate | Trust -> true
  | _ -> false

let relation_compare r1 r2 =
  normalise (left_relation_projection r1) = normalise (left_relation_projection r2)
  && normalise (right_relation_projection r1) = normalise (right_relation_projection r2)

let rec compose_relation r1 r2 =
  match r1, r2 with
  | Basic r1, Basic r2 ->
    (match compose_basic r1 r2 with
     | Some r, s -> (Basic r, s)
     | None, s -> (Explosive (Basic r1, Basic r2), s))
  | Basic _, Asymmetrical (_, _) ->
    compose_relation r2 r1
  | Asymmetrical (r1a, r1b), Basic r2 ->
    let one_explosive r s s' ri rr =
      if s || not_to_be_forgotten r then
        (Explosive (Asymmetrical (fst rr, snd rr), Basic r2), s || s')
      else (* We forget about r, which was probably not that important here. *)
        (Explosive (Basic ri, Basic r2), s')
    in
    (match compose_basic r1a r2, compose_basic r1b r2 with
     | (Some r1, s1), (Some r2, s2) ->
       if r1 = r2 then (Basic r1, s1 || s2)
       else (Asymmetrical (r1, r2), s1 || s2)
     | (Some r, s1), (None, s2) -> one_explosive r s1 s2 r1b (r, r1b)
     | (None, s1), (Some r, s2) -> one_explosive r s2 s1 r1a (r1a, r)
     | (None, s1), (None, s2) ->
       (Explosive (Asymmetrical (r1a, r1b), Basic r2), s1 || s2))
  | Basic _, Explosive (_, _) ->
    compose_relation r2 r1
  | Explosive (r1a, r1b), Basic r2 ->
    let (ra, sa) = compose_relation r1a (Basic r2) in
    let (rb, sb) = compose_relation r1b (Basic r2) in
    let ra = simplify ra in
    let rb = simplify rb in
    let r =
      if relation_compare ra rb then ra
      else simplify (Explosive (ra, rb)) in
    (r, sa || sb)
  | Asymmetrical (r1a, r1b), Asymmetrical (r2a, r2b) ->
    (match compose_basic r1a r2a, compose_basic r1b r2b with
     | (Some r1, s1), (Some r2, s2) ->
       if r1 = r2 then (Basic r1, s1 || s2)
       else (Asymmetrical (r1, r2), s1 || s2)
     | (Some r, s1), (None, s2) ->
       (simplify (Explosive (Asymmetrical (r, r1b), Asymmetrical (r, r2b))), s1 || s2)
     | (None, s1), (Some r, s2) ->
       (simplify (Explosive (Asymmetrical (r1a, r), Asymmetrical (r2a, r))), s1 || s2)
     | (None, s1), (None, s2) ->
       (simplify (Explosive (Asymmetrical (r1a, r1b), Asymmetrical (r2a, r2b))), s1 || s2))
  | Asymmetrical (_, _), Explosive (_, _) ->
    compose_relation r2 r1
  | Explosive (r1a, r1b), Asymmetrical (_, _) ->
    let (ra, sa) = compose_relation r1a r2 in
    let (rb, sb) = compose_relation r1b r2 in
    (simplify (Explosive (ra, rb)), sa || sb)
  | Explosive (r1a, r1b), Explosive (r2a, r2b) ->
    let (ra, sa) = compose_relation r1a r2a in
    let (rb, sb) = compose_relation r1b r2b in
    let r1 = simplify (Explosive (ra, rb)) in
    let (rc, sc) = compose_relation r1a r2b in
    let (rd, sd) = compose_relation r1b r2a in
    let r2 = simplify (Explosive (ra, rb)) in
    let r =
      if relation_complexity r1 > relation_complexity r2
      then r2 else r1 in
    (r, sa || sb || sc || sd)

let compose (r1, s1) (r2, s2) =
  let (r, s) = compose_relation (normalise r1) (normalise r2) in
  (r, s || s1 || s2)

let neutral = (Basic Neutral, false)

let rec reverse_relation = function
  | Basic r -> Basic r
  | Asymmetrical (r1, r2) -> Asymmetrical (r2, r1)
  | Explosive (r1, r2) ->
    Explosive (reverse_relation r1, reverse_relation r2)

let reverse (r, s) = (reverse_relation r, s)

let rec asymmetrical_relation r1 r2 =
  match r1, r2 with
  | Explosive (r1a, r1b), _ ->
    let ra = asymmetrical_relation r1a r2 in
    let rb = asymmetrical_relation r1b r2 in
    if ra = Basic Neutral then rb
    else if rb = Basic Neutral then ra
    else Explosive (ra, rb)
  | _, Explosive (r2a, r2b) ->
    let ra = asymmetrical_relation r1 r2a in
    let rb = asymmetrical_relation r1 r2b in
    if ra = Basic Neutral then rb
    else if rb = Basic Neutral then ra
    else Explosive (ra, rb)
  | Basic b1, Basic b2 ->
    if b1 = Neutral && b2 = Neutral then Basic Neutral
    else Asymmetrical (b1, b2)
  | Asymmetrical (b1, b2), Basic b3 ->
    if b3 = Neutral then r1
    else Explosive (Asymmetrical (b1, b3), Basic b3)
  | Basic b1, Asymmetrical (b2, b3) ->
    (** We have to reverse [b2] and [b3] here. **)
    let r2 = Asymmetrical (b3, b2) in
    if b1 = Neutral then r2
    else Explosive (Basic b3, r2)
  | Asymmetrical (b1, b2), Asymmetrical (b3, b4) ->
    (** Tricky case, there are two inversions here. **)
    Explosive (Asymmetrical (b1, b4), Asymmetrical (b2, b3))

let asymmetrical (r1, s1) (r2, s2) =
  (asymmetrical_relation r1 r2, s1 || s2)

