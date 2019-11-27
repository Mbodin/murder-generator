
This file presents various features that may be useful when writing scenario elements.
The file [elements.md](./elements.md) is a prerequisite to this file.
There is no particular order in its subsections: they can be read in any order.

# About Comments

Comments can be nested: `(* (* *) *)`.
This especially useful when commenting out large portions of code: there is no need to “break” comments inside.

If a comment starts with a star, like `(* * My Section *)`, it means that it starts a thematic section.
If it has two stars, like `(* ** My Subsection *)`, it defines a subsection.
These are of course still ignored by the parser, but it is a nice way to indicate that the elements in a section are related in some ways.

# Unique and Duplicable Elements

By default, an element will not be applied more than once per character: if a character is already involved in an element, it can’t be involved again (even with a different instantiation).
For instance, let us consider the following snippet from [notary.murder](../../data/elements/notary.murder):
```murder
element CausalDebt
begin
  let A be player with attribute Richness as Neutral
                  with contact Know to B as True
  let B be player with attribute Richness as Neutral
                  with contact Know to A as True
```
If a given player is involved in this element, for example associated to the player variable `A`, it can’t be involved in another instance of this same element, even as a different player variable (hence, not even as `B` with another `A`).
However, the element itself might be reused with a completely different instantiation: as soon as the first players involved are not involved again.

Sometimes, one wants a different behaviour.
A common behaviour that is sometimes useful is that an element should be applied at most once in any given scenario.
For instance, if a scenario element invokes a “chosen one”, or a particular god, or anything like that, one doesn’t want this scenario element to be instantiated more than once for different characters!
This can be done by stating that a scenario element is `unique`.
Here is a snippet from [religion.murder](../../data/elements/religion.murder):
```murder
unique element HarmonyDescription
begin
  let G be player with attribute Specy as God
                  with attribute DivineAllegiance as Harmony
```
This element describes a particular god.
Only one player can play this god in a particular scenario: the `unique` keyword enforces that.

As the scenario element writer, you might sometimes know that a scenario element can’t possibly be applied more than once.
This is for instance the case of most of the (technical) scenario elements defined in [language.murder](../../data/elements/language.murder).
Here is an example:
```murder
unique element NormalGrammarEN
begin
  provide strict attribute GrammarEN to any other player as Normal
end
```
The `provide strict attribute` command is applied to all players, and its strictness (see below) enforces it to only be applicable at most once per player.
To help the solver, one can thus mark it as unique: it won’t change the final result, and may help the solver not trying it several times if there is no point to do it.

In the opposite direction than the `unique` keyword, one may want no constraint to be on an element.
For instance, here is a snippet from [religion.murder](../../data/elements/religion.murder):
```murder
duplicable element GodYieldStrongFeeling
begin
  let P be player
  let G be player with attribute Specy as God

  provide relation from P to G as strong neutral
end
```
We want this element to apply as many times as possible for each player `P`: this element is meant to be applied to every pairs of player and god.
The `duplicable` keyword does exactly that: the solver will not be limited in how this element will be instantiated, and it may be instantiated several times with the same players.

# Advanced Constraints

Some elements do just not work if some conditions are met.
Typically, if someone is part of the same family, one can hardly assume that they only met for the first time during their activity.
It is thus important to be able to restrict the application of scenario elements.
This can be done with the `not as` keyword.

For instance, here is a snippet from [dreams.murder](../../data/elements/dreams.murder):
```murder
unique element DreamAboutThief
begin
  let P be player with attribute Job not as Thief
                  with contact Know to R as True
                  with contact FamilyRelation to R not as Spouse or Parent or Child
```
This scenario element is checking that the player `P` is not a thief.
Also note the usage of the `or` keyword to ensure that `P` is not a direct family member with `R`.
This `or` keyword can also be used without the `not`: to ensure that two characters `A` and `B` are direct family members, one can add a restriction `with contact FamilyRelation to B as Spouse or Parent or Child` to the declaration of `A` (and symmetrically to `B`).

One can also restrict the players that are not in referenced in the scenario element using the `any other player` expression.
This expression is usually referenced at the very beginning of scenario elements to restrict the other players.
It follows the same syntax than for any player declaration.
For instance, here is a snippet from [family.murder](../../data/elements/family.murder):
```murder
element Twins
begin
  let A be player with attribute Specy as Human
  let B be player with attribute Specy as Human
  let any other player be with contact FamilyRelation to A as None
                          with contact FamilyRelation to B as None
```
The `let any other player be` command enforces that no other players share any family relation between `A` and `B`.
If it weren’t there, other scenario elements might be applied, creating cycles in the family trees.

The `any other player` expression can also be used in the `provide attribute` and `provide contact` commands.
It will be applied to all players not referenced in the current scenario element.
For instance, here is a snippet from [family.murder](../../data/elements/family.murder):
```murder
element FromNobodysFamily
begin
  let P be player

  provide compatible contact FamilyRelation between P and any other player as None
end
```
This element will provide the contact between `P` and all players which are not `P`.
If there were to be other players declared in the scenario element, they would also be removed from the `any other player` pool.
If one also wants to apply a `provide attribute` or `provide contact` to all players, including the ones declared in the current scenario element, there exists the `any player` expression.
Its usage his however rare in practise: in most cases, one only needs the `any other player` expression.

# Event Kinds

The file [elements.md](./elements.md) said in the Event Assumptions section that events can be marked with event kinds, such as `Personal` or `Work`.
Such event kinds can be declared using the `declare event` command.
Here is for instance for the event `Personal` is declared in [identity.murder](../../data/elements/identity.murder):
```murder
declare event Personal
```
That’s it.
Thanks to this declaration, one can declare events in scenario elements as `Personal`, as shown in [elements.md](./elements.md) with the following example:
```murder
  provide event lasting weeks to A and B
  begin
    event Personal
  end
```
Note that a given event may very well have more than one event kind: it just needs to include more than one `event` command.

It is possible to include dependencies between events.
For that, just add `event` commands in a block after the `declare event` command.
For instance, here is a snippet from [job.murder](../../data/elements/job.murder):
```murder
declare event Work
begin
  event Personal
end
```
This declares the event kind `Work` and declares it as being `Personal`: every work event personally implies the character.
This means that every time an `event Work` command is issued in an event (in a scenario element), then there will be an implicit `event Personal` command.
This is by the way why most events have only one `event` command: there are of several kinds, but most of these kinds are implicit.

Inside an event kind declaration, one can also place a category inside an event declaration.
For instance, here is a snippet from [notary.murder](../../data/elements/notary.murder):
```murder
declare event MoneyExchange
begin
  event Personal
  category Money
end
```
This declaration means that the event kind `MoneyExchange` depends on the event kind `Personal` (that is, any exchange of money personally involves a character), but also that it depends on the category `Money`: any scenario element providing an event of kind `MoneyExchange` will be dependent on the `Money` category.

# Phantom Events

Some events are just there to express some constraints.
Such events are usually not associated to any translations, and it would be silly to display them in character sheets.
The `phantom` keyword tells the program that this event is indeed just there for expressing constraints and should not be exported.

For instance, to express in a scenario element that a character `B` is older than a character `A`, one can provide an event as follows (this is a snippet from [identity.murder](../../data/elements/identity.murder)):
```murder
  provide immediate phantom event to A and B
  begin
    assume no event providing attribute Born to A before
    assume no event providing attribute Born to B after
  end
```
This event is declared `phantom` and is thus associated no translations (any translation will be rejected by the program in such an event).
This event assumes some constraints about the birth events of `A` and `B`, enforcing `B` to be older.
This event is `immediate`, so it won’t conflict with another event: if it were to last years, this would make an artificial hole in `A` and `B`’s life which would be difficult to explain.
This event is thus purely here to ensure that indeed, `A`’s birth happened before than `B`.

Another usage of phantom events is to space two events.
To illustrate, here is a snippet from [romance.murder](../../data/elements/romance.murder):
```murder
element FallInLoveAfterAParty
begin
  let A be player
  let B be player

  (* Some constraints and other events. *)

  provide event lasting minutes to A and B
  begin
    event Personal
    sentence
    begin
      translation en A:+sbeg " and " B " met during a party organised by " C "."
      (* Other translations *)
    end
    sentence
    begin
      translation en "They greatly appreciated chatting one with the other and exchanged their contacts."
      (* Other translations *)
    end
  end

  provide phantom event lasting days to A and B

  provide event lasting days to A and B
  begin
    event Personal
    sentence
    begin
      translation en "Meeting once again each other, " A " and " B " fell in love."
      (* Other translations *)
    end
    provide contact Relation between A and B as Love
  end
end
```
This scenario element defines two crucial events: first the two characters `A` and `B` met, then, after some time, they met again and fall in love.
Without the phantom event, these two important events could be just placed one after the other… which would just be silly.
To space the two events, we create a `phantom` event with the right duration in between: this will force the program to place at least this amount of time between the two events.
Note that the phantom event is neither associated any translation (because it is a phantom event), but also no event kind (it could, but this one doesn’t need it) or constraints: its block is thus empty (`begin` would be immediately followed by `end`).
Empty blocks can always be safely removed, as it is the case in this example.

# Blocking Events

By default, an event “books” the space for a player according to its duration: two events lasting years can’t intersect themselves if they involve the same characters, but an event lasting years and an event lasting minutes can.
Sometimes, one may want to overwrite this default behaviour by preventing any other event from happening at the same time if they involve a common character.
For instance, in [wounds.murder](../../data/elements/wounds.murder), a scenario element describes a terrible event for a victim who ends up in an hospital:
```murder
  provide blocking event lasting days to V
  begin
    (* Player V is at the hospital for days and this will change their life. *)
  end
```
The wounds are very high and are likely to completely change the victim’s life.
It would be silly to place an event lasting minutes when the victim is at the hospital, as if nothing were to happen.
As this event also changes the life of the victim, it would be silly to place an event lasting years (like the victim’s education cursus or a year-long job) intersecting with this event: we want this event to be a rupture in the victim’s life, yielding very strong emotions.
We thus defines the event as `blocking`, enforcing that, for this player, nothing else happens at the same time.

It is also possible to combine this with phantom events, to create the sensation of a large hole in a character’s life.
For instance, here is a snippet from [identity.murder](../../data/elements/identity.murder):
```murder
element BirthOld
begin
  let P be player

  provide immediate event to P
  begin
    provide strict attribute Born to P as True

    translation en P:+sbeg " is born."
  end

  provide blocking phantom event lasting decades to P
  begin
    assume no event Personal to P before
    provide attribute Age to P as Old
  end
end
```
To create the illusion that a character is old, an birth event is provided, then a large hole of two dozens of years with nothing, then the other events of the character start: it feels as if the life of this character started long after their birth, as if the time erased the rest.
To create this sensation of a hole with no event, one use a `blocking` `phantom` event: as it is `phantom`, it is not displayed in character sheets, and as it is `blocking`, no other event will happen simultaneously.
To avoid that the program fits some events to this player before the phantom event, a constraint is placed ensure that no personal event be placed before the phantom event.
This is by the way because of this trick that the first event is not declared to be dependent on the event kind `Personal`: if it were `Personal` (although it morally is), no event can assume `assume no event Personal to P before` afterwards, thus preventing such a blocking phantom event.

# Compatibilities

TODO: Rewrite that.

The file [memory.md](./memory.md) says the following:
> However, they can only have one instance of an attribute: no character can have two different attributes `Gender` for instance.

Sometimes one would still like to combine two different values for a given attribute.
For that, there is a possibility to declare compatible values.
One would for instance create a value for the combination, then declare this “combined” value as being compatible with the other base values.
See the example of the contact `FamilyRelation` in the next section for more details: all these mechanisms are the same for attributes.
If you are familiar with [OpenStreetMap](https://www.openstreetmap.org), you can think of it as the different values for the [key `sidewalk`](https://wiki.openstreetmap.org/wiki/Key:sidewalk): `no`, `left`, `right`, and `both`.
The only difference is that we tell here the program that `both` can also be interpreted as a `left` or a `right`, whereas OpenStreetMap requires the renderers to know what each value exactly means.

As for attributes (because, really, contacts and attributes are the same thing, just that contacts are from a character to another whereas attributes air “sticked” on a particular character), one might want to combine values.
This is done through the notion of compatibility: the “combination”-value will be compatible with both “base”-values.
For instance, [family.murder](../../data/elements/family.murder) declares a value `True` for `FamilyRelation` in addition to all the specific values for family relations.
This value `True` is meant to describe the relation between two players of the same family that are not any of the most precise values (like `Sibling`, `Spouse`, etc.).
It is declared as follows:
```murder
contact FamilyRelation True
begin
  compatible with Sibling
  compatible with Parent
end
```
Stating that a value is compatible with the other means that the value can be freely coerced from the base value to the compatible one.
Hence, in the example, if the contact `FamilyRelation` from a player to another is `Sibling`, but that an element expects it to be `True`, the element will be applicable: `Sibling` can be coerced into `True` because `True` is compatible with `Sibling`.
The converse is however not true: if an element expects a contact `FamilyRelation` to be `Sibling` but that it only is `True`, then the element won’t be applicable.
In the rare case where one wants two values to be coercable in both direction, one can just declare both values to be compatible one with the other.
The actual use case of such a situation is dubious as both values won’t be differentiable by any element: they thus would be equivalent for the program.

As explained in [memory.md](./memory.md), both `Male` and `Female` are marked as being compatible with `NonBinary` such that a character with attribute `Gender` as `NonBinary` may be chosen if an elements is looking for a character with attribute `Gender` as `Male` or `Female`.
The converse is however not true: if an element is looking for a character with attribute `Gender` as `NonBinary`, only the `NonBinary`-characters will be chosen, as there is no `compatible with` command in `NonBinary`’s block.

As for attributes, this snippet uses the `compatible with` keyword to make `True` compatible with `Intimate`: if a character intimately knows another, then it also knows this other character.
```murder
declare contact Know
begin
  translation en "Knows"
  (* Other translations *)
end

contact Know True
begin
  translation en "yes"
  (* Other translations *)
  compatible with Intimate
end

contact Know Intimate
begin
  translation en "intimately"
  (* Other translations *)
end

internal contact Know False
begin
  translation en "no"
  (* Other translations *)
end
```

TODO: describe how to “break” compatible attributes and contacts by asking both “Female” and “not NonBinary”.

# Strictness

TODO: describe the strictness flag on attribute and contacts.

