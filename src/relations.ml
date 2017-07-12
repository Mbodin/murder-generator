
type basic =
  | Neutral
  | Hate
  | Trust
  | Chaotic
  | Undetermined
  | Avoidance

type relation =
  | Basic of basic
  | Asymetrical of basic * basic
  | Explosive of t * t

type t = relation * bool

let basic_complexity = function
  | Neutral -> 0
  | Hate -> 1
  | Trust -> 1
  | Chaotic -> 1
  | Undetermined -> 2
  | Avoidance -> 1

let rec relation_complexity = function
  | Basic r -> basic_complexity r
  | Asymetrical (r1, r2) -> max (relation_complexity r1) (relation_complexity r2)
  | Explosive (r1, r2) -> 1 + relation_complexity r1 + relation_complexity r2

let complexity (r, s) =
  let c = relation_complexity r in
  if s then c else 2 * c

let basic_difficulty = function
  | Neutral -> 0
  | Hate -> 2
  | Trust -> -1
  | Chaotic -> 1
  | Undetermined -> 1
  | Avoidance -> 1

let rec relation_difficulty = function
  | Basic r -> basic_difficulty r
  | Asymetrical (Basic Neutral, r) | Asymetrical (r, Basic Neutral) -> relation_difficulty r - 1
  | Asymetrical (r1, r2) -> max (relation_difficulty r1) (relation_difficulty r2)
  | Explosive (r1, r2) -> 1 + relation_difficulty r1 + relation_difficulty r2

let difficulty (r, s) =
  let c = relation_difficulty r in
  if s then c else 2 * c

let is_explosive = function
  | (Explosive (_, _), _) -> true
  | _ -> false

let is_strong = snd

let normalise r =
  let rec aux l = function
    | Explosive (r1, r2) ->
      aux (aux l r1) r2
    | Basic b -> Utils.left b :: l
    | Asymetrical (b1, b2) -> Utils.right (b1, b2) :: l
  in
  match List.sort compare (aux [] r) with
  | x :: l ->
    let f = function
    | Utils.left b -> Basic b
    | Utils.right (b1, b2) -> Asymetrical (b1, b2)
    in
    List.fold_left (fun r x -> Explosive (f x, r)) (f x) l
  | [] ->
    Errors.should_not_happen "Empty list computed during relation normalisation" ;
    Basic Neutral

let simplify r =
  let rec aux l = function
    | Explosive (r1, r2) ->
      aux (aux l r1) r2
    | Basic b -> Utils.left b :: Utils.right b :: l
    | Asymetrical (b1, b2) -> Utils.left b1 :: Utils.right b2 :: l
  in
  match List.sort_uniq compare (aux [] r) with
  | x :: l ->
    let different_type x y =
      match x, y with
      | Utils.left _, Utils.right _ | Utils.right _, Utils.left _ -> true
      | Utils.left _, Utils.left _ | Utils.right _, Utils.right _ -> false
    in let extract = function
      | Utils.left r | Utils.right r -> r
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
        option_map (fun (z, l) -> (z, y :: l)) (next_exact x l)
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
          | Utils.left x -> (x, y)
          | Utils.right x -> (y, x)
        in let r =
          if x = y then
            if x = Neutral then r
            else Explosive (r, Basic x)
          else Explosive (r, Asymetrical (x, y)) in
        aux r l
    in let (y, l) =
      next x l
    in let (x, y) =
      match x with
      | Utils.left x -> (x, y)
      | Utils.right x -> (y, x)
    in let r =
          if x = y then
            Basic x
          else Asymetrical (x, y) in
    aux r l
  | [] ->
    Errors.should_not_happen "Empty list computed during relation simplification" ;
    Basic Neutral

let rec left_projection = function
  | Basic r -> Basic r
  | Asymetrical (r, _) -> Basic r
  | Explosive (r1, r2) -> Explosive (left_projection r1, left_projection r2)

let rec right_projection = function
  | Basic r -> Basic r
  | Asymetrical (_, r) -> Basic r
  | Explosive (r1, r2) -> Explosive (right_projection r1, right_projection r2)

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

let not_to_forgotten = function
  | Hate | Trust -> true
  | _ -> false

let relation_compare r1 r2 =
  normalise (left_projection r1) = normalise (left_projection r2)
  and
  normalise (right_projection r1) = normalise (right_projection r2)

let rec compose_relation r1 r2 =
  match r1, r2 with
  | Basic r1, Basic r2 ->
    (match compose_basic r1 r2 with
     | Some r, s -> (r, s)
     | None, s -> (Explosive (Basic r1, Basic r2), s))
  | Basic _, Asymetrical (_, _) ->
    compose_relation r2 r1
  | Asymetrical (r1a, r1b), Basic r2 ->
    let one_explosive r s ri rr =
      if s or not_to_forgotten r then
        (Explosive (Asymetrical (fst rr, snd rr), Basic r2), s)
      else (* We forget about r, which was probably not that important here. *)
        (Explosive (Basic ri, Basic r2), false)
    in
    (match compose_basic r1a r2, compose_basic r1b r2 with
     | (Some r1, s1), (Some r2, s2) ->
       if r1 = r2 then (Basic r1, s1 or s2)
       else (Asymetrical (r1, r2), s1 or s2)
     | (Some r, s), None -> one_explosive r s r1b (r, r1b)
     | None, Some (r, s) -> one_explosive r s r1a (r1a, r)
     | None, None ->
       (Explosive (Asymetrical (r1a, r1b), Basic r2), false))
  | Basic _, Explosive (_, _) ->
    compose_relation r2 r1
  | Explosive (r1a, r1b), Basic r2 ->
    let (ra, sa) = compose_relation r1a (Basic r2) in
    let (rb, sb) = compose_relation r1b (Basic r2) in
    let ra = simplify ra in
    let rb = simplify rb in
    (if relation_compare ra rb then ra
     else Explosive (ra, rb),
     sa or sb)
  | Asymetrical (r1a, r1b), Asymetrical (r2a, r2b) ->
    (match compose_basic r1a r2a, compose_basic r1b r2b with
     | (Some r1, s1), (Some r2, s2) ->
       if r1 = r2 then (Basic r1, s1 or s2)
       else (Asymetrical (r1, r2), s1 or s2)
     | (Some r, s), None ->
       (simplify (Explosive (Asymetrical (r, r1b), Asymetrical (r, r2b))), false)
     | None, Some (r, s) ->
       (simplify (Explosive (Asymetrical (r1a, r), Asymetrical (r2a, r))), false)
     | None, None ->
       (simplify (Explosive (Asymetrical (r1a, r1b), Asymetrical (r2a, r2b))), false))
  | Asymetrical (_, _), Explosive (_, _) ->
    compose_relation r2 r1
  | Explosive (r1a, r1b), Asymetrical (_, _) ->
    let (ra, sa) = compose_relation r1a r2 in
    let (rb, sb) = compose_relation r1b r2 in
    (simplify (Explosive (ra, rb)), sa or sb)
  | Explosive (r1a, r1b), Explosive (r2a, r2b) ->
    let (ra, sa) = compose_relation r1a r2a in
    let (rb, sb) = compose_relation r1b r2b in
    let (rc, sc) = compose_relation r1a r2b in
    let (rd, sd) = compose_relation r1b r2a in
    (simplify (Explosive (Explosive (ra, rb), Explosive (rc, rd)), sa or sb or sc or sd)

let compose (r1, s1) (r2, s2) =
    let (r, s) = compose_relation (normalise r1) (normalise r2) in
    (r, s or s1 or s2)

