
This file to give a tutorial to write scenario elements.
It is strongly advise to have read [memory.md](./memory.md) before reading this tutorial, as the notions of the memory model will be used extensively here.
The tutorial itself is divided into two parts: a first “basic” part that explain how to write simple elements, then a more advanced one.

# Declaring Categories, Attributes, and Contacts

Before writing elements per-se, one has to understand how attributes, contacts, as well as categories are declared.
Attributes and contacts are described in [memory.md](./memory.md).
Category is a way to filter elements so that no offensive scenario is generated.

## Categories

Categories is a mean of protecting your users from unwanted themes.
For instance, violence is not a theme that everyone wants to have to deal with.
Users are given the choice of which categories that are confortable with when generating the scenario.
When writing elements that may be unwanted by some users, it is important to make sure that it falls into at least one category.

A category can be declared as follows.
This is a snippet from [religion.murder](../data/religion.murder), defining the category named `Religion`.
```murder
category Religion
begin
  translation en "Religion"
  (* Other translations *)

  description en "God(s), priests, workships, etc."
  (* Other translations *)
end
```
The words `category`, `begin`, `end`, `translation`, `description`, and more generally any word starting with a lower-case letter, are keywords.
User-defined names, like `Religion` in this example, start with an upper-case letter.
Comments are between `(*` and `*)`: everything inside a comment will be ignored by the program.
Comments are meant to help you and other element writer organise the files, and clarify some subtleties about user definitions.

There is no particular place where each command can be: despite the category `Religion` is defined in [religion.murder](../data/religion.murder), it can be referred to in any `.murder` file in the [data](../data) folder.
Organising the files is thus just a matter of dividing each notions into meaningful files.

The two keywords `begin` and `end` start a block that describes the defined category.
In its block, a category defines how its name should be translated into the available languages, and include a description of it.
The `translation` and `description` keywords are very similar in syntax: they are followed by a language-code, then a string of characters between `"`.
The language code is a two-letter or three-letter code associated to the language in [translations.json](../web/translations.json) (see [translations.md](./translations.md)).
It is not necessary to have a translation in all available languages, but having translations in as many languages as possible is better.

In addition to the `translation` and `description` declarations, a category can also be marked as dependent from another category.
This is done through the `category` keyword.
For instance, here follows a snippet from [secrets.murder](../data/secrets.murder).
```murder
category DarkSecrets
begin
  category NonKids

  translation en "Dark secrets"
  description en "Deep secrets which, if publicly revealed, may damage the live of some."
  (* Other languages *)
end
```
In this example, the category `DarkSecrets` is marked as being dependent from the category `NonKids`.
This means that any element dependent on the category `DarkSecrets` will also be dependent on the category `NonKids`.

## Attributes and Contacts

An attribute can be declared very similarly to a category, but with the `declare attribute` keyword.
Here is for instance a snippet from [identity.murder](../data/identity.murder) declaring an attribute `Gender`.
```murder
declare attribute Gender
begin
  translation en "Gender"
  (* Other translations *)
end
```
As for categories, an attribute can to be translated with the `translation` keyword.
It can’t be described through the `description` keyword as its description would never be shown to the user.

Once the attribute has been defined, one can defined attribute values (see [memory.md](./memory.md) for more details).
In this case `Male`, `Female`, and `NonBinary`.
```murder
attribute Gender Male
begin
  translation en "male"
  (* Other translations *)
  compatible with NonBinary
end

attribute Gender Female
begin
  translation en "female"
  (* Other translations *)
  compatible with NonBinary
end

attribute Gender NonBinary
begin
  translation en "genderqueer"
  (* Other translations *)
end
```
As explained in [memory.md](./memory.md), both `Male` and `Female` are marked as being compatible with `NonBinary` such that a character with attribute `Gender` as `NonBinary` may be chosen if an elements is looking for a character with attribute `Gender` as `Male` or `Female`.
The converse is however not true: if an element is looking for a character with attribute `Gender` as `NonBinary`, only the `NonBinary`-characters will be chosen, as there is no `compatible with` command in `NonBinary`’s block.

In [identity.murder](../data/identity.murder), the attribute values `Male` and `Female` have other `add` commands.
These commands are explained in [language.md](./language.md).
In most attribute declarations, you won’t need to use neither the `compatible with` nor the `add` commands.

As for categories, an attribute can be marked dependent on a category.
For instance, here is a snippet from [religion.murder](../data/religion.murder) where the attribute `DivineAllegiance` is marked as being dependent on the category `Religion`.
Any element referring to the attribute `DivineAllegiance` will then be implicitly marked as also dependent to the category `Religion`.
```murder
declare attribute DivineAllegiance
begin
  category Religion
  translation en "Divine allegiance"
  (* Other translations *)
end
```

Contacts work in a very similar way than attributes.
Here is for instance a snippet from [relations.murder](../data/relations.murder).
The contact `Know` is defined, with possible values `True`, `Intimate`, and `False`.
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

The contact value `False` is declared as being `internal`.
This does not change anything, except that this particular value will not be shown to the user.
It is advised to mark “default” values as internal.
In this case, if no contact is shown between two characters, one will probably assume that the `Know` contact is `False` between them: there is no need to add this cumbersome information to the user.

Attribute values can naturally also be set as internal.
One can also directly set an attribute or a contact as internal: this will mark all its values as being internal, effectively hiding this attribute or this contact to the user.
An example can be found in [objectives.murder](../data/objectives.murder), where the contact `Goal` is defined.
This contact is meant to make explicit the goals of each characters with respect to each other characters.
These goals are usually quite clear in the character sheets, and there is no need to burden the user with such an information.
```murder
declare internal contact Goal
begin
  translation en "Goal"
  (* Other translations *)
end
```

Now that we have seen how to declare attributes and contacts, we can focus on how to define elements.

# Writing Elements

An element is the part of a story that will be built by the generator.
It is thus important to consider that any element you write might appear in any context.
Here are some items to consider when writing elements:
- it would be preferable if your element could be easily played.  For instance, avoid magical objects that couldn’t be simulated with role-playing rules or whose associated rule would spoil the secrecy of the object.
- the best elements are elements that can yield interesting role-play: character background, emotional traits, stories between players, internal thoughts about another player, etc.
- an element is best if it can change over time.  In particular, it may be interesting to cut some elements into several parts.  For instance, if you are describing how two people got to know each other, marry each other, then divorce, consider splitting this element into three parts: how they met each other, how they married, and why they divorced.  This will enable you or other writers to write variants of each events (several possible way to get to know each other, several ways to organise a wedding, etc.), leading to much more diversity in the generated rules.
- on the contrary, elements should not be too small of a story.  Indeed, the generator can choose not to apply a particular element: you can’t assume that a particular element will be present in the final scenario—or will be present the way you expected it to be.  This means that each element has to work in isolation: an element shouldn’t only be interesting if another element is present in the scenario.  This means that elements sometimes can’t be split.
- a good way to get the right length of an element is to ask whether there is a single action in the element.  Think of it as the unity of action in classical dramatic tragedy.
- thanks to the category mechanism, users can protect themselves from harmful themes: do not restrict yourself when writing elements.  Just double-check that it is adequately marked in a particular category if needed.

Once you have a good idea for your element, find a place in a `.murder` file in the [data](../data) folder where it would fit (it can be placed anywhere as soon as it is not itself inside a block).
Find a name that describes well your element and write the following lines:
```murder
element MyElement
begin
end
```
This is already a valid element!
It is named `MyElement` (it has to start with an uppercase letter, please use more suggestive element name: `MyElement` is just an example).
This element is however quite useless as it does nothing.
As for attributes and contacts one can add `category` commands in its block to restrict it to some categories.
It is however rare to need to so as categories are automatically inferred from the attributes and contacts the element uses: if for instance an element makes use of the attribute `DivineAllegiance` (which is marked as depending from category `Religion`), then there is no need to add a `category Religion` to the element as it would be automatically inferred.

Inside an element there are two parts: what the element needs, and what the element provides.
There is no particular order for these, but I strongly advise to start by the needs and ends by what is provided.

An element is about playable characters.
Let us declare for instance three characters:
```murder
element MyElement
begin
  let A be player
  let B be player
  let C be player
end
```
This states that the element is about three characters `A`, `B`, and `C`.
Characters are usually represented by a single uppercase letter.
Note that `A`, `B`, and `C` are not the names of the players: each of them will be replaced by an actual character from the scenario.

One may want to restrict the element to only apply when some conditions over `A`, `B`, and `C` are satisfied.
For instance, your element may need `C` to be a priest and `A` to know `B` to make sense.
For that, add restrictions about your characters with the `with` and `as` commands:
```murder
element MyElement
begin
  let A be player with contact Know to B as True
  let B be player
  let C be player with attribute Job as Priest
end
```
Of course, the value after the `as` keyword has to correspond to a possible value of the attribute or contact after the `with` keyword.
Look for common values of attributes and contacts among the other `.murder` files if you are unsure, or just create your own (see the first section of this file for how to declare such values).

Constraints on characters can be chained.
For instance, here is a snippet from [job.murder](../data/job.murder) where the character `P` has two constraints: not only he or she can’t be a seller, but its family relation to the character `S` has to be `None`.
This snippet also shows that one can state that a value should *not* be a particular one, using the `not as` keyword.
```murder
element SellerIsEasilyKnown
begin
  let S be player with attribute Job as Seller
  let P be player with attribute Job not as Seller
                  with contact FamilyRelation to S as None
```

This first part of the element describes when the element applies.
When applying an element, the program will try to look for other elements to satisfy what it requires.
If the program can’t find such an element but that the requirements do not contradict with the current scenario, the element may still be applied (although the generator tries to avoid such cases).

Let us now focus on what the element provides.
An element can provide all the notions described in [memory.murder](./memory.murder): events, attributes, contacts, relations, difficulty, and complexity.

Let us start with the difficulty and complexity, as they are quite simple.
If you consider that your element makes a character more complex to understand or more difficult to play, you can use the `add` command.
Conversely, one can use the `remove` command if you consider that an element ease the understanding (that is, decreases the complexity) or the difficulty to play.
In this example, the element adds two point of difficulty to `A` and remove one point of complexity to `C`.
```murder
element MyElement
begin
  let A be player with contact Know to B as True
  let B be player
  let C be player with attribute Job as Priest

  add difficulty to A
  add difficulty to A
  remove complexity to C
end
```
Adding individual difficulty or complexity points can be useful, but it is in general to express these as a relation.
For instance, if the large increase of difficulty to `A` is because `C` hates `A`, just say so with the `provide relation` command:
```murder
element MyElement
begin
  let A be player with contact Know to B as True
  let B be player
  let C be player with attribute Job as Priest

  provide relation from C to A as hate
end
```
Such relations are much more informative to the program than individual addition or removal of difficulty or complexity.
The possible values for relations are explained in [memory.murder](./memory.murder).
The possible values are: `neutral`, `hate`, `trust`, `chaotic`, `undetermined`, and `avoidance`, as well as their strong variants (`strong hate`, `strong trust`, etc.).
The value `strong neutral` is possible, but it does nothing by itself.
It however (as any other `strong` relation) makes any further relation between the two player strong.
Thus, if an element provides a `strong neutral` relation between two players, and that another element adds a `trust`, then it becomes a `strong trust` relation automatically.

If the relation is symmetrical, one can of course double the commands as in:
```murder
  provide relation from A to B as trust
  provide relation from B to A as trust
```
There is however a useful shortcut: the following line is equivalent to the two lines above.
```murder
  provide relation between A and B as trust
```
As stated in [memory.murder](./memory.murder), relations are not meant to tell the program whether an element apply.
This means that even if the current relation between `A` and `B` is `hate`, an element with the command above will still apply (leading to an exploding mixture of love and hate).

To prevent an element for applying, one has to use attributes and contacts.
We have seen above how to assume that some combinations of attributes and contacts are present in the scenario.
Let us now see how to write attributes and contacts in the current state.

The syntax is quite close to relation.
Here are two commands adding the attribute `Job` with value `Seller` to `B`, and adds the contact `FamilyRelation` from `A` to `B` with value `Sibling`.
```murder
  provide attribute Job to B as Seller
  provide contact FamilyRelation from A to B as Sibling
```
Such commands are however very rarely done directly in the block of an element.
Instead, most of them are done directly inside an event: this helps provide a timestamp to when particular attribute or contact are defined (when `B` became a seller, when two people got to know each other, etc.).
Let us thus see how events can be declared.

One can be the usual `provide` command to add an event.
However this command is itself a block: events have to be translated!
It would thus look like this:
```murder
element MyElement
begin
  let A be player with contact Know to B as True
  let B be player
  let C be player with attribute Job as Priest

  provide event lasting weeks to A and B
  begin
    (* event block *)
  end
end
```
There might be more than one event declared in an element.
When more than one event is declared, the order of the event is preserved in the final scenario: the first event is guaranteed to happen before the second, and so on.

Before explaining the event block, let me explain the particularities of the `provide event` command.
First, it is applied to one character or more, each separated by `and`.
Second, as explained in [memory.murder](./memory.murder), events have lengths.
We can’t directly express their length, but we can express in which kind of length they fall thanks to the `lasting` command.
The possible values of time after the `lasting` command are: `seconds`, `minutes`, `days`, `weeks`, `years`, and `decades`.
Instead of using the `lasting` keyword, it is possible to prefix the adjectives `immediate`, (`very`) `short`, `medium`, (`very`) `long`, or `life`, as in the following example.
Its meaning it exactly the same than with the `lasting` keyword (it is however rarely used except for the `immediate` adjective).
```murder
element MyElement
begin
  let A be player with contact Know to B as True
  let B be player
  let C be player with attribute Job as Priest

  provide medium event to A and B
  begin
    (* event block *)
  end
end
```

We now focus on the event block itself.
Most event block starts with the `event Personal` command:
```murder
  provide event lasting weeks to A and B
  begin
    event Personal
  end
```
The keyword `event` is similar to `category` in that it marks the event as being `Personal`.
Other kinds of events exists (`Work`, `Dream`, etc.) and they can simply be declared in the same way than category—outside an element—with the `declare event` command.
We won’t detail it here: it is rarely useful and should be self-explaining at this stage.

Events themselves are also made of several parts: one stating where this event should be placed, what it provides, and what are its sentences.
We have already seen the command providing attributes or contact: it is exactly the same than the one for the element (`provide attribute Job to B as Seller`, `provide contact FamilyRelation from A to B as Sibling`, etc.), just that it is provided in the event.
Do not provide the same attribute or contact twice: if an event provides it, do not repeat it in the element.
Here is an example of a valid element.
```murder
element MyElement
begin
  let A be player with contact Know to B as True
  let B be player
  let C be player with attribute Job as Priest

  provide event lasting weeks to A and B
  begin
    event Personal
    provide attribute Job to B as Seller
    provide contact FamilyRelation from A to B as Sibling
  end
end
```

Commands stating where the event should be place start with the `assume` keyword.
It enables to ensure that, for particular character, at least one particular event already happened before, will happen after, or that no particular event happened before or will happen after.
One can describe an event by its event marks (`Personal`, `Work`, `Dream`, etc.).
For instance, to state that no event about work happened to `A` before, one can write:
```murder
  provide event lasting weeks to A and B
  begin
    event Personal
    assume no event Work to A before
  end
```
Commands of the form `assume event Work to A before` assuming the presence of an event before or after may be difficult to ensure for the solver: it will definitely try to look for elements that would provide such an event, but may fail in doing so.

In addition to refer to event kinds, one can assume that an event providing a particular attribute or contact.
This is typically used to state that an event should happen after that the assumptions of the element are requested.
For instance in our running example, one might want to add the following two `assume` commands (note how we do not repeat the values `True` and `Priest`):
```murder
element MyElement
begin
  let A be player with contact Know to B as True
  let B be player
  let C be player with attribute Job as Priest

  provide event lasting weeks to A and B
  begin
    event Personal
    assume event providing contact Know from A to B before
    assume event providing attribute Job to A before
  end
end
```

Finally, events should have translations.
This is crucial as these translations will be the ones shown to the players.
As for categories, attributes, and contacts, translations are defined by the `translation` keyword.
However, the language is slightly more powerful as it enables to use the names and words associated to the characters defined in the element.
In other words, it will be possible to use the names of `A`, `B`, and `C` in the example.
The syntax for such sentences is explained in [language.md](./language.md).
Here is a simple example:
```murder
  provide event lasting weeks to A and B
  begin
    event Personal
    assume event providing contact Know from A to B before
    assume event providing attribute Job to A before

    translation en "Something happens to " A " and " B "."
    (* Other translations *)
  end
```
In the final scenario, the variables `A` and `B` will be replaced by the character names.
For instance, if `A` is called Alice and `B` Bob, the translation will be `Something happens to Alice and Bob.`.

If there is more than one sentence, the `sentence` keyword enables to start a block to separate each translations.

Here follows a full example of a valid element.
```murder
element MyElement
begin
  let A be player with contact Know to B as True
  let B be player
  let C be player with attribute Job as Priest

  provide event lasting weeks to A and B
  begin
    event Personal
    assume event providing contact Know from A to B before
    assume event providing attribute Job to A before

    provide attribute Job to B as Seller
    provide contact FamilyRelation from A to B as Sibling

    sentence
    begin
      translation en "Something happens to " A " and " B "."
      (* Other translations *)
    end

    sentence
    begin
      translation en "It somehow made " B " a seller, and a sibling of " A "."
      (* Other translations *)
    end
  end

  provide event lasting years to B and C
  begin
    event Personal

    sentence
    begin
      translation en "Other things later happen."
      (* Other translations *)
    end
  end

  provide relation from C to A as hate
  remove complexity to C
end
```

You have now learned how to write elements.
The next section presents advanced features: you will probably only rarely use them.
Now you can read the already written elements in the files of the [data](../data) folder, and write your own!

# Advanced Features

This section presents various features that may be useful when writing scenario elements.
There is no particular order in its subsections: they can be read in any order.

TODO: describe the strictness flag on attribute and contacts.

TODO: describe how to “break” compatible attributes and contacts by asking both “Female” and “not NonBinary”.

TODO: `unique` and `duplicable`.

TODO: (not) as Value1 or Value2

TODO: `declare event`

TODO: phantom events

## About Comments

Comments can be nested: `(* (* *) *)`.
This especially useful when commenting out large portions of code: there is no need to “break” comments inside.

If a comment starts with a star, like `(* * My Section *)`, it means that it starts a thematic section.
If it has two stars, like `(* ** My Subsection *)`, it defines a subsection.
These are of course still ignored by the parser, but it is a nice way to indicate that the elements in a section are related in some ways.

