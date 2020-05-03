
open Libutils

type player_attribute = Id.t
type contact_attribute = Id.t

type player_constructor = Id.t
type contact_constructor = Id.t

type object_constructor = player_constructor

type attribute_kind =
  | Player
  | AnyObject
  | Object of object_constructor

type contact_kind = {
    kind_from : attribute_kind ;
    kind_to : attribute_kind
  }

type player_kinds = attribute_kind list
type contact_kinds = contact_kind list

type ('attribute, 'constructor, 'kind) constructor_map_type = {
    name : string Id.map (** Attribute names *) ;
    map : ('attribute * string) Id.map
      (** The map storing each constructor.
         The attribute is part of the constructor, with the constructor
         name. *) ;
    kinds : ('attribute, 'kind) PMap.t
      (** The kind of each attributes *) ;
    association : ('attribute, 'constructor list) PMap.t
      (** Which constructors is associated to which attribute. *) ;
    compatibility : ('attribute, ('constructor, 'constructor list) PMap.t) PMap.t
      (** For each constructor, what is its compatibility list. *) ;
    internal : ('attribute, ('constructor PSet.t) option) PMap.t
        (** For each attribute, what is its internal status.
           This status is expressed as an option type:
           - [None] means that every constructor of this attribute
             are internal,
           - [Some] means that only the ones in the associated set
             are internal. *)
  }

type player_constructor_map =
  (player_attribute, player_constructor, player_kinds) constructor_map_type
type contact_constructor_map =
  (contact_attribute, contact_constructor, contact_kinds) constructor_map_type

type constructor_maps = {
    player : player_constructor_map ;
    contact : contact_constructor_map
  }

type attributes =
  | PlayerAttribute of player_attribute
  | ContactAttribute of contact_attribute

type constructors =
  | PlayerConstructor of player_constructor
  | ContactConstructor of contact_constructor

let attribute_any = [Player; AnyObject]

let contact_any =
  let build f t = {
      kind_from = f ;
      kind_to = t
    } in
  List.concat (List.map (fun f -> List.map (build f) attribute_any) attribute_any)

let attribute_sub k1 k2 =
  match k1, k2 with
  | Object _, AnyObject -> true
  | _, _ -> k1 = k2

let contact_sub k1 k2 =
  attribute_sub k1.kind_from k2.kind_from
  && attribute_sub k1.kind_to k2.kind_to

let lift f l1 l2 =
  List.for_all (fun k -> List.exists (f k) l2) l1

let attributes_sub = lift attribute_sub

let contacts_sub = lift contact_sub

module type Attribute = sig
    type attribute
    type constructor
    type kind
    type constructor_map

    val any : kind
    val empty_constructor_map : constructor_map
    val attribute_name : constructor_map -> attribute -> string option
    val attribute_kind : constructor_map -> attribute -> kind option
    val constructor_name : constructor_map -> constructor -> string option
    val constructor_attribute : constructor_map -> constructor -> attribute option
    val constructors : constructor_map -> attribute -> constructor list option
    val all_constructors : constructor_map -> constructor list
    val declare_attribute : constructor_map -> string -> bool -> kind -> attribute * constructor_map
    val name_attribute : constructor_map -> string -> attribute * constructor_map
    val declare_constructor : constructor_map -> attribute -> string -> bool -> constructor * constructor_map
    val name_constructor : constructor_map -> attribute -> string -> constructor * constructor_map
    val get_attribute : constructor_map -> string -> attribute option
    val get_constructor : constructor_map -> attribute -> string -> constructor option
    val remove_constructor : constructor_map -> constructor -> constructor_map
    val declare_compatibility : constructor_map -> attribute -> constructor -> constructor -> constructor_map
    val is_compatible : constructor_map -> attribute -> constructor -> constructor -> bool
    val is_internal : constructor_map -> attribute -> constructor -> bool
    val get_constructor_maps : constructor_maps -> constructor_map
    val set_constructor_maps : constructor_maps -> constructor_map -> constructor_maps
    val to_attributes : attribute -> attributes
    val to_constructors : constructor -> constructors
    val name : string
  end

module AttributeInst (K : sig
      type kind
      val any : kind
      val name : string
      val get_constructor_maps : constructor_maps -> (Id.t, Id.t, kind) constructor_map_type
      val set_constructor_maps : constructor_maps -> (Id.t, Id.t, kind) constructor_map_type -> constructor_maps
      val kind_sub : kind -> kind -> bool
      val to_attributes : Id.t -> attributes
      val to_constructors : Id.t -> constructors
      (** Specific for tests. *)
      val test_kind1 : kind
      val test_kind2 : kind
    end) = struct

    include K

    type attribute = Id.t
    type constructor = Id.t

    type constructor_map =
      (attribute, constructor, kind) constructor_map_type

    let empty_constructor_map : constructor_map = {
        name = Id.map_create () ;
        map = Id.map_create () ;
        kinds = PMap.empty ;
        association = PMap.empty ;
        compatibility = PMap.empty ;
        internal = PMap.empty
      }

    let attribute_name m a =
      Id.map_inverse m.name a

    let attribute_kind m a =
      try Some (PMap.find a m.kinds)
      with Not_found -> None

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

    let declare_attribute m a internal k =
      let (a, mn) = Id.map_insert_t m.name a in
      let association =
        if PMap.mem a m.association then
          m.association
        else PMap.add a [] m.association in
      let kinds =
        (** Only the most precise kind is conserved. *)
        try
          let k' = PMap.find a m.kinds in
          if kind_sub k' k then m.kinds
          else PMap.add a k m.kinds
        with Not_found -> PMap.add a k m.kinds in
      let internal =
        (** Any command stating that it is internal has priority on the ones that
           state that it is public. *)
        if internal then
          PMap.add a None m.internal
        else m.internal in
      (a, { m with name = mn ;
                   association = association ;
                   kinds = kinds ;
                   internal = internal })

    let declare_constructor m a c internal =
      let (c, mc) = Id.map_insert_t m.map (a, c) in
      let l =
        try PMap.find a m.association
        with Not_found -> assert false in
      let l = if List.mem c l then l else c :: l in
      let internal =
        let s =
          try PMap.find a m.internal
          with Not_found -> Some PSet.empty in
        match s with
        | None -> m.internal
        | Some s ->
          if PSet.mem c s || not internal then
            m.internal
          else
            PMap.add a (Some (PSet.add c s)) m.internal in
      (c, { m with map = mc ;
                   association = PMap.add a l m.association ;
                   internal = internal })

    let name_attribute m a = declare_attribute m a false any
    let name_constructor m a c = declare_constructor m a c false

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

    (* Testing the behaviour of [declare_attribute] and [name_attribute]. *)
    let%test_unit _ =
      let attribute_should_be_unknown m a =
        assert (attribute_name m a = None) ;
        assert (attribute_kind m a = None) ;
        assert (constructors m a = None) in
      let attribute_should_be_known m a =
        assert (attribute_name m a <> None) ;
        assert (attribute_kind m a <> None) ;
        assert (constructors m a <> None) in
      let a1 = "attribute1" in
      let a2 = "attribute2" in
      let i1 = true in
      let i2 = false in
      let k1 = test_kind1 in
      let k2 = test_kind2 in
      let m = empty_constructor_map in
      assert (all_constructors m = []) ;
      let (a1_id, m) = name_attribute m a1 in
      attribute_should_be_known m a1_id ;
      let (a2_id, m) = declare_attribute m a2 i2 k2 in
      attribute_should_be_known m a1_id ;
      attribute_should_be_known m a2_id ;
      let (a1_id', m) = name_attribute m a1 in
      assert (a1_id = a1_id') ;
      let (a2_id', m) = name_attribute m a2 in
      assert (a2_id = a2_id') ;
      let (a1_id'', m) = declare_attribute m a1 i1 k1 in
      assert (a1_id = a1_id'') ;
      let (a2_id'', m) = name_attribute m a2 in
      assert (a2_id = a2_id'') ;
      attribute_should_be_known m a1_id ;
      attribute_should_be_known m a2_id ;
      assert (attribute_name m a1_id = Some a1) ;
      assert (attribute_name m a2_id = Some a2) ;
      assert (attribute_kind m a1_id = Some k1) ;
      assert (attribute_kind m a2_id = Some k2) ;
      let m = empty_constructor_map in
      attribute_should_be_unknown m a1_id ;
      attribute_should_be_unknown m a2_id

  end

module PlayerAttribute =
  AttributeInst (struct
      type kind = player_kinds
      let any = attribute_any
      let name = "attribute"
      let get_constructor_maps m = m.player
      let set_constructor_maps i m = { i with player = m }
      let kind_sub = attributes_sub
      let to_attributes a = PlayerAttribute a
      let to_constructors c = PlayerConstructor c
      let test_kind1 = [Player]
      let test_kind2 = [AnyObject]
    end)

module ContactAttribute =
  AttributeInst (struct
      type kind = contact_kinds
      let any = contact_any
      let name = "contact"
      let get_constructor_maps m = m.contact
      let set_constructor_maps i m = { i with contact = m }
      let kind_sub = contacts_sub
      let to_attributes a = ContactAttribute a
      let to_constructors c = ContactConstructor c
      let test_kind1 = [{ kind_from = Player ; kind_to = AnyObject }]
      let test_kind2 = [{ kind_from = Player ; kind_to = Player }]
    end)

let (empty_constructor_maps, object_type) =
  let m = PlayerAttribute.empty_constructor_map in
  let (object_type, m) =
    PlayerAttribute.declare_attribute m "<object type>" true [AnyObject] in
  let m = {
      player = m ;
      contact = ContactAttribute.empty_constructor_map
    } in
  (m, object_type)

