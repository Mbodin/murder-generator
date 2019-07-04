
module type Attribute = sig
    type attribute
    type constructor
    type constructor_map

    val empty_constructor_map : constructor_map
    val attribute_name : constructor_map -> attribute -> string option
    val constructor_name : constructor_map -> constructor -> string option
    val constructor_attribute : constructor_map -> constructor -> attribute option
    val constructors : constructor_map -> attribute -> constructor list option
    val all_constructors : constructor_map -> constructor list
    val declare_attribute : constructor_map -> string -> bool -> attribute * constructor_map
    val declare_constructor : constructor_map -> attribute -> string -> bool -> constructor * constructor_map
    val get_attribute : constructor_map -> string -> attribute option
    val get_constructor : constructor_map -> attribute -> string -> constructor option
    val remove_constructor : constructor_map -> constructor -> constructor_map
    val declare_compatibility : constructor_map -> attribute -> constructor -> constructor -> constructor_map
    val is_compatible : constructor_map -> attribute -> constructor -> constructor -> bool
    val is_internal : constructor_map -> attribute -> constructor -> bool
  end

module AttributeInst () =
  struct

    type attribute = Id.t
    type constructor = Id.t

    type constructor_map = {
        name : string Id.map (** Attribute names **) ;
        map : (attribute * string) Id.map
          (** The map storing each constructor.
           * The attribute is part of the constructor, with the constructor
           * name. **) ;
        association : (attribute, constructor list) PMap.t
          (** Which constructors is associated to which attribute. **) ;
        compatibility : (attribute, (constructor, constructor list) PMap.t) PMap.t
          (** For each constructor, what is its compatibility list. **) ;
        internal : (attribute, (constructor PSet.t) option) PMap.t
            (** For each attribute, what is its internal status.
             * This status is expressed as an option type:
             * - [None] means that every constructor of this attribute
             *   are internal,
             * - [Some] means that only the ones in the associated set
             *   are internal. **)
      }

    let empty_constructor_map = {
        name = Id.map_create () ;
        map = Id.map_create () ;
        association = PMap.empty ;
        compatibility = PMap.empty ;
        internal = PMap.empty
      }

    let attribute_name m a =
      Id.map_inverse m.name a

    let constructor_name m c =
      Option.map snd (Id.map_inverse m.map c)

    let constructor_attribute m c =
      Option.map fst (Id.map_inverse m.map c)

    let constructors m a =
      try Some (PMap.find a m.association)
      with Not_found -> None

    let is_internal m a c =
      try match PMap.find a m.internal with
          | None -> true
          | Some s -> PSet.mem c s
      with Not_found -> false

    let declare_attribute m a internal =
      let (a, mn) = Id.map_insert_t m.name a in
      (a, { m with name = mn ;
                   association =
                     if PMap.mem a m.association then
                       m.association
                     else PMap.add a [] m.association ;
                   internal =
                     if internal then
                       PMap.add a None m.internal
                     else m.internal })

    let declare_constructor m a c internal =
      let (c, mc) = Id.map_insert_t m.map (a, c) in
      let l =
        try PMap.find a m.association
        with Not_found -> assert false in
      let internal =
        let s =
          try PMap.find a m.internal
          with Not_found -> Some PSet.empty in
        match s with
        | None -> m.internal
        | Some s ->
          if PSet.mem c s = internal then
            m.internal
          else
            let s = (if internal then PSet.add else PSet.remove) c s in
            PMap.add a (Some s) m.internal in
      (c, { m with map = mc ;
                   association = PMap.add a (c :: l) m.association ;
                   internal = internal })

    let get_attribute m a =
      Id.get_id m.name a

    let get_constructor m a c =
      Id.get_id m.map (a, c)

    let remove_constructor m c =
      let a = Utils.assert_option __LOC__ (constructor_attribute m c) in
      let filter = List.filter ((<>) c) in
      let l =
        try PMap.find a m.association
        with Not_found -> assert false in
      let comp =
        let mc =
          try PMap.find a m.compatibility
          with Not_found -> PMap.empty in
        let mc =
          List.fold_left (fun mc c ->
            let l =
              try PMap.find c mc
              with Not_found -> [] in
            PMap.add c (filter l) mc) mc l in
        PMap.add a (PMap.remove c mc) m.compatibility in
      { m with association = PMap.add a (filter l) m.association ;
               compatibility = comp }

    let declare_compatibility m a c c' =
      let mc =
        try PMap.find a m.compatibility
        with Not_found -> PMap.empty in
      let l =
        try PMap.find c mc
        with Not_found -> [] in
      { m with compatibility =
                 PMap.add a (PMap.add c (c' :: l) mc) m.compatibility }

    let is_compatible m a c c' =
      let mc =
        try PMap.find a m.compatibility
        with Not_found -> PMap.empty in
      let l =
        try PMap.find c mc
        with Not_found -> [] in
      List.mem c' l

    let all_constructors m =
      List.concat (PMap.fold (fun cs l -> cs :: l) m.association [])

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

