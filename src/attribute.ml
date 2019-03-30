
module type Attribute = sig
    type attribute
    type constructor
    type constructor_map

    val empty_constructor_map : constructor_map
    val attribute_name : constructor_map -> attribute -> string option
    val constructor_name : constructor_map -> constructor -> string option
    val constructor_attribute : constructor_map -> constructor -> attribute option
    val constructors : constructor_map -> attribute -> constructor list option
    val declare_attribute : constructor_map -> string -> attribute * constructor_map
    val declare_constructor : constructor_map -> attribute -> string -> constructor * constructor_map
    val get_attribute : constructor_map -> string -> attribute option
    val get_constructor : constructor_map -> attribute -> string -> constructor option
    val remove_constructor : constructor_map -> constructor -> constructor_map
    val declare_compatibility : constructor_map -> attribute -> constructor -> constructor -> constructor_map
    val is_compatible : constructor_map -> attribute -> constructor -> constructor -> bool
  end

module AttributeInst () =
  struct

    type attribute = Id.t
    type constructor = Id.t

    type constructor_map =
      string Id.map (** Attribute names **)
      * (attribute * string) Id.map
          (** The map storing each constructor.
           * The attribute is part of the constructor,
           * with the constructor name. **)
      * (attribute, constructor list) PMap.t
          (** Which constructors is associated to which attribute. **)
      * (attribute, (constructor, constructor list) PMap.t) PMap.t
          (** For each constructor, what are its compatibility list. **)

    let empty_constructor_map =
      (Id.map_create (),
       Id.map_create (),
       PMap.empty,
       PMap.empty)

    let attribute_name (m, _, _, _) a =
      Id.map_inverse m a

    let constructor_name (_, m, _, _) c =
      Option.map snd (Id.map_inverse m c)

    let constructor_attribute (_, m, _, _) c =
      Option.map fst (Id.map_inverse m c)

    let constructors (_, _, m, _) a =
      try Some (PMap.find a m)
      with Not_found -> None

    let declare_attribute (mn, mc, al, comp) a =
      let (a, mn) = Id.map_insert_t mn a in
      (a, (mn, mc, (if PMap.mem a al then al else PMap.add a [] al), comp))

    let declare_constructor (mn, mc, al, comp) a c =
      let (c, mc) = Id.map_insert_t mc (a, c) in
      let l =
        try PMap.find a al
        with Not_found -> assert false in
      (c, (mn, mc, PMap.add a (c :: l) al, comp))

    let get_attribute (mn, _, _, _) a =
      Id.get_id mn a

    let get_constructor (_, mc, _, _) a c =
      Id.get_id mc (a, c)

    let remove_constructor m c =
      let a = Utils.assert_option __LOC__ (constructor_attribute m c) in
      let (mn, mc, al, comp) = m in
      let filter = List.filter ((<>) c) in
      let l =
        try PMap.find a al
        with Not_found -> assert false in
      let comp =
        let m =
          try PMap.find a comp
          with Not_found -> PMap.empty in
        let m =
          List.fold_left (fun m c ->
            let l =
              try PMap.find c m
              with Not_found -> [] in
            PMap.add c (filter l) m) m l in
        PMap.add a (PMap.remove c m) comp in
      (mn, mc, PMap.add a (filter l) al, comp)

    let declare_compatibility (mn, mc, al, comp) a c c' =
      let m =
        try PMap.find a comp
        with Not_found -> PMap.empty in
      let l =
        try PMap.find c m
        with Not_found -> [] in
      (mn, mc, al, PMap.add a (PMap.add c (c' :: l) m) comp)

    let is_compatible (_, _, _, comp) a c c' =
      let m =
        try PMap.find a comp
        with Not_found -> PMap.empty in
      let l =
        try PMap.find c m
        with Not_found -> [] in
      List.mem c' l

  end

module PlayerAttribute = AttributeInst ()

module ContactAttribute = AttributeInst ()

type constructor_maps = {
    player : PlayerAttribute.constructor_map ;
    contact : ContactAttribute.constructor_map
  }

let empty_constructor_maps = {
    player = PlayerAttribute.empty_constructor_map ;
    contact = ContactAttribute.empty_constructor_map
  }

type attribute =
  | PlayerAttribute of PlayerAttribute.attribute
  | ContactAttribute of ContactAttribute.attribute

type constructor =
  | PlayerConstructor of PlayerAttribute.constructor
  | ContactConstructor of ContactAttribute.constructor

