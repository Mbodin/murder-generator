
This program combines scenario elements defined in the [data](../../data) folder.
These elements are defined in a particular format (explained in [elements.md](./elements.md)) and describe how the model of the current scenario should be updated.
This document describes the memory model of scenarios in this program.

There are three parts in the memory model: the history of events, how characters are related with each others, and the difficulty/complexity of each characters.

# Event History

The event history is what is outputed in the “Graphviz (events)” export.
It comprises all the events of any characters in the scenario.

Events are divided into six categories of length:
- immediate events, that last a few seconds;
- very short events, that last a few minutes;
- short events, that last a few days;
- medium events, that last a few weeks;
- long events, that last a few years;
- life events, or very long events, that last decades.

At any given point in history, a player can’t have more than one event of a given length-category.
For instance, if a character has two very long events, these two events will be one after the other, but they will not intersect.

Events are crucial for this program, as they represent most of the character sheets given to users.
Each event is associated a translation to each language, as explain in [elements.md](./elements.md).

Some special events are not associated to any translation.
Such events are only used to space two events.
They are called phantom events.
For instance, if a player has three very long events, but that the middle one if a phantom event, the player will only see the first and the third, with a decade space between the two: the phantom event is just there to state that some time passed in between.

# Character State

The character state describes all the role-playing characteristics of players and theirs contacts.
It is composed of two notions: attributes and contacts.

## Attributes

Attributes are labels attached to each players.
Each attribute is named, and is associated a value.
This is very similar to what is done in the memory model of e.g., [OpenStreetMap](https://www.openstreetmap.org) or [WikiData](https://www.wikidata.org).

For instance, [identity.murder](../../data/elements/identity.murder) defines the following attribute `Gender`:
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

The values of each attribute live in a different namespace: there is no issue with naming two values of two different attributes the same way.
For instance, both attributes `SecretSociety` and `Disability` have a value `None`.

Some attributes or attribute values are defined as internal.
This has actually very little to do with the memory model: this is just a way to tell the program that these attributes should not be displayed to the user, to avoid printing useless or cumbersome information.

## Contacts

Contacts are very similar to attributes, but instead of being labels on characters, they are labels on arrows between two characters.
This means that contacts involve two characters: one “from”, and one “to” (as well as the usual contact name and its value).
For instance, let us consider the following lines from [relations.murder](../../data/elements/relations.murder):
```murder
declare contact Know
contact Know True
internal contact Know False
```
This declares a contact `Know` which can be either `True` or `False`: a given person can know the other person, or it doesn’t know this other person.
Note how the contact value `False` is marked as internal: there is no need to explicitly tell a player that this character doesn’t know another character (but it is important to tell the player that another character is known).
A given character can have the same contact declared for any other character: knowing one person doesn’t prevent a character from knowing other person.
However, it is not possible that a given player both knows and doesn’t know a given player: given an origin and a destination characters, a contact can only have one value.

Contacts are asymmetrical: it is possible that a first character A knows a second B without B knowing A.
It is the responsibility of the element writers to always generate the reversed contact when a contact is meant to be symmetrical.

# Difficulty and Complexity

Attributes and contacts are used to describe the scenario, and are thus crucial to state whether a particular scenario element *applies*.
Difficulty and complexity are used by the program to know whether a given scenario element *should be applied* (and not whether it applies).
Target difficulty and complexity for each players are asked to the user before the generation.
The solver will do its best to match these target difficulties and complexities in a reasonable amount of time.

Difficulties and complexities come into two forms.
The first form is in direct control of the scenario-element writer.
For instance, in [job.murder](../../data/elements/job.murder), a scenario element suggest to a thief that a character is wealthy, and thus a good target.
By doing so, the scenario element is clearly making the life of the target more complex.
To indicate this to the program, this element contains the following line (`P` is the name of the target in this element):
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
It is the element writer who has to judge how many points of difficulty or complexity should be added or removed in a given scenario element.
Note that a scenario element doesn’t have to add or remove difficulty or complexity.
Actually, most elements do not manipulate these notions.
They are just guides for the generator to know what would be the game effect of a particular element.

These difficulties and complexities are direct: they are directly placed onto characters.
One might want some kind of semantic way to express difficulty and complexity instead.
This is done through relations.
Relations express some basic emotions that a player have with respect to a character.
They are few of them, by design: the goal is not to express scenario relations (use contacts for this), but to guide the program into what kind of gaming behaviours each players will experience with respect to the other players.
There are six basic relations:
- `neutral`, when the scenario doesn’t influence the relation of these two players into a particular behaviour.
- `hate`, when the players will try to hinder the actions of the other.
- `trust`, when the players will have the tendency to trust or help each other.
- `chaotic`, when the behaviour of both players strongly depends on the current situation, which will vary during the game.  This can happen when both players will have to compete in some ways because of a contract, but have no particular advantage into helping or preventing the other’s actions.  The help or hindering of the players will however never last long: “It’s just business.”
- `undetermined`, for a relation that can easily go into hate or trust depending on what the players do or decide.  This happens for instance when two long-separated acquaintances meet again.
- `avoidance`, when both players are incited not to speak with the other.

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
For instance, the combination of `trust` and `hate` provides two points of difficulty and three points of complexity (instead of just one point of difficulty and two points of complexity if the program were to just add both values).

Finally, some relations are strong.
A strong relation doubles its effect on the player difficulty and complexity.
Hence, a `strong hate` relation adds four points of difficulty and two of complexity (instead of two points of difficulty and one of complexity for `hate`).

To sum up, each player is associated an inner difficulty and complexity, as well as a relation to the other players.
The total difficulty and complexity of a player is the sum of its inner difficulty/complexity and of each of the complexity/difficulty yielded by its relations.

Note that [relations.murder](../../data/elements/relations.murder) defines a contact named `Relation`, with similar values than the relations, like `Neutral` or `Trust`.
These are not to be mixed: the contact named `Relation` is a contact, used to describe the scenario and to make some elements incompatible with each others, whilst relations are meant to describe how this scenario will affect each players.
In particular, relations will never make elements incompatible with the current state, whilst contacts are designed for this.

You now know how this program stores its states.
A good read after this file is [elements.md](./elements.md) that explains how to write scenario elements.

