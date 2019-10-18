
This program combines scenario elements defined in the [data](../data) folder.
These elements are defined in a particular format (explained in [elements.md](./elements.md)) and describe how the model of the current scenario should be updated.
This document describes the memory model of scenarios in this program.

There are three parts in the memory model: the history of events, how characters are related with each others, and the difficulty/complexity of each characters.

# Event History

The event history is what is outputed in the “Graphviz (events)” export.
It comprises all the events (although not yet translated at this stage) of any characters in the scenario.

Events are divided into six categories of length:
* immediate events, that last a few seconds;
* very short events, that last a few minutes;
* short events, that last a few days;
* medium events, that last a few weeks;
* long events, that last a few years;
* life events, or very long events, that last decades.

At any given point in history, a player can’t have more than one event of a given length-category.

Events are crucial for this program, as they represent most of the character sheets.
Each event is associated a translation to each language, as explain in [elements.md](./elements.md).

Some special events might not be associated to any translation.
Such events are only used to space two events.
They are called phantom events.

# Character State

The character state describes all the role-playing characteristics of players and theirs contacts.
It is composed of two notions: attributes and contacts.

## Attributes

Attributes are labels attached to each players.
Each attribute is named, and is associated a value.
This is very similar to what is done in the memory model of e.g., [OpenStreetMap](https://www.openstreetmap.org) or [WikiData](https://www.wikidata.org).

For instance, [identity.murder](../data/identity.murder) defines the following attribute `Gender`:
```murder
declare attribute Gender
```
Later on, three values for this attribute are declared: `Male`, `Female`, and `NonBinary`.
```murder
attribute Gender Male
attribute Gender Female
attribute Gender NonBinary
```
This means that any character in the scenario can be supposed to have an attribute `Gender` with associated value being either `Male`, `Female`, or `NonBinary`.
There will often be a value like `None` or `False` associated to most attributes to indicate that the character doesn’t have this attribute.
Such a “negative” value is however no different for the program than any other value.

Characters can have as many attributes as they want.
However, they can only have one instance of an attribute: no character can have two different attributes `Gender` for instance.

Sometimes one would still like to combine two different values for a given attribute.
For that, there is a possibility to declare compatible values.
One would for instance create a value for the combination, then declare this “combined” value as being compatible with the other base values.
See the example of the contact `FamilyRelation` for more details: all these mechanisms are the same for attributes.

The values of each attribute live in a different namespace: there is no issue with naming two values of two different attributes the same way.
For instance, both attributes `SecretSociety` and `Disability` have a value `None`.

Some attributes or attribute values are defined as internal.
This has actually very little to do with the memory model: this is just a way to tell the program that these attributes should not be displayed to the user, to avoid printing useless information.

## Contacts

Contacts are very similar to attributes, but are labels on arrays between two characters.
This means that contacts involve two characters: one “from”, one “to”, and the usual contact name and its value.
For instance, let us consider the following lines from [relations.murder](../data/relations.murder):
```murder
declare contact Know
contact Know True
internal contact Know False
```
This declares a contact `Know` which can be either `True` or `False`: a given person can know the other person, or it doesn’t know this other person.
Note how the contact value `False` is marked as internal: there is no need to explicitly tell a player that this character doesn’t know another character (but it is important to tell the player that another character is known).
A given character can have the same contact declared for any other character: knowing one person doesn’t prevent a character from knowing other person.
However, it is not possible that a given player both knows and doesn’t know another player: given an origin character and a destination player, a contact can only have one value.

Contacts are asymmetrical: it is possible that a first character A knows a second B without B knowing A.
It is the responsibility of the element writers to always generate the reversed contact when a contact is meant to be symmetrical.

As for attributes (because, really, contacts and attributes are the same thing, just that contacts are from a character to another and attributes are “sticked” on a particular character), one might want to combine values.
This is done through the notion of compatibility: the “combination”-value will be compatible with both “base”-values.
For instance, [family.murder](../data/family.murder) declares a value `True` for `FamilyRelation` in addition to all the specific values for family relations.
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
The actual use of such a situation is dubious as both values won’t be differentiable by any element: they thus would be equivalent.

# Difficulty and Complexity

Attributes and contacts are used to describe the scenario, and are thus crucial to state whether a particular scenario element applies or not.
Difficulty and complexity are used for the program to know whether a given scenario element should be applied.
In particular, target difficulty and complexity for each players are asked to the user before the generation.
The solver will do its best to match these target difficulties and complexities in a reasonable amount of time.

Difficulties and complexities come into two forms.
The first form is in direct control to the writer of scenario elements.
For instance, in [job.murder](../data/job.murder), a scenario element suggest to a thief that a character is wealthy, and thus a good target.
By doing so, the scenario element is clearly make the life of the target more complex.
This element thus contains the following line (`P` is the name of the target in this element):
```murder
add difficulty to P
```
One can symmetrically remove difficulty or complexity.
The following line would for instance remove a point of complexity to the player `P`:
```murder
remove complexity to P
```
If one wants to add or remove more than one point of difficulty or complexity, one can repeat the line as much time as necessary.
For instance, the following lines adds three points of complexity to `P`:
```murder
add complexity to P
add complexity to P
add complexity to P
```
It is to the element writer’s judge to decide how many point of difficulty or complexity should be added in a given scenario element.
Note that a scenario element doesn’t have to add or remove difficulty or complexity.
Actually, most elements do not manipulate these notions.
They are just guides for the generator to know what would be the game effect of a particular element.

These difficulties and complexities are direct: they are directly placed onto characters.
One might want some kind of semantic way to express difficulty and complexity instead.
This is done through relations.
Relations express some basic emotions that a player have with respect to a character.
They are voluntarily small: the goal is not to express scenario relations (use contacts for this), but to guide the program into what kind of gaming behaviours each players will have with respect to the others.
There are six basic relations:
* `neutral`, for when the scenario doesn’t influence the relation of these two players into a particular behaviour.
* `hate`, for when the players will try to hinder the actions of the other.
* `trust`, when the players will have the tendency to trust or help each other.
* `chaotic`, for when the behaviour of both players strongly depends on the current situation, which will vary during the game.  This can happen when both players will have to compete in some ways because of a contract, but have no particular advantage into helping or preventing the other’s actions.  The help or hindering of the players will however never last long: “It’s just business.”
* `undetermined`, for a relation that can easily go into hate or trust depending on what the players do or decide.  This happens for instance when two long-separated acquaintances meet again.
* `avoidance`, for when both players are incited not to speak with the other.

These are the base types of relations.
However, in contrary to contacts, relations never prevent an element to be applied.
Hence, if an element provides a relation `hate` but that it currently is set to `trust`, these two relations will be somehow merged.
There are thus combinations of relations, with possibly very complex schemes.
Heuristics are used to merge relations into a relation that makes sense.
As for contacts, relations do not have to be symmetrical, although they frequently are.

The point of relation is to provide complexities and difficulties to each characters.
For instance, `hate` adds two point of difficulty to both players, whilst `trust` removes one point of difficulty to both players: difficulty is meant to track how a player is helped or hindered.
Most relations adds one point of complexity, except `neutral` who doesn’t add any, and `undetermined` who adds two: complexity measures how many information a player has to keep in mind during the game.
Combinations of basic relations can lead to higher values.
For instance, the combination of `trust` and `hate` provides two points of difficulty and three points of complexity.

Finally, some relations are strong.
A strong relation doubles its effect on the player difficulty and complexity.
Hence, a `strong` `hate` relation adds four points of difficulty and two of complexity.

To sum up, each player is associated an inner difficulty and complexity, as well as a relation each other players.
The total difficulty and complexity of a player is the sum of its inner difficulty/complexity with each of the complexity/difficulty yielded by its relations.

Note that [relations.murder](../data/relations.murder) defined a contact named `Relation`, with similar values than the relations, like `Neutral` or `Trust`.
These are not to be mixed: the contact named `Relation` is a contact, used to describe the scenario and to make some elements incompatible with each others, whilst relations are meant to describe how this scenario will affect each players.
In particular, relations will never make elements incompatible with the current state, whilst contacts are designed for this.

