
This file aims to provide a detailed tutorial on how to write scenario elements.
I strongly advise reading [memory.md](./memory.md) before reading this tutorial, as the notions of the memory model will be extensively used here.
The tutorial itself is divided into two parts: a first “basic” part that explain how to write simple elements, then a more advanced one.

# Declaring Categories, Attributes, and Contacts

Before writing elements per-se, one has to understand how attributes, contacts, as well as categories are declared.
Attributes and contacts are described in [memory.md](./memory.md).
Category is a way to filter elements such that no offensive scenario is generated.

## Categories

Categories is a mean of protecting your users from unwanted themes.
For instance, violence is not a theme that everyone want to have to deal with (but some users might).
Users are given the choice of which categories to include when generating the scenario.
When writing elements that may be unwanted by some users, it is important to make sure that it falls into at least one category.

A category can be declared as follows.
This is a snippet from [religion.murder](../data/elements/religion.murder), defining the category named `Religion`.
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
These rules about case are general and hold all along the commands presented in this document.
Comments are between `(*` and `*)`: everything inside a comment will be ignored by the program.
Comments are meant to help you and other element writers organise the files, and clarify some subtleties about user definitions.

There is no particular place where each command needs to be: despite the category `Religion` is defined in [religion.murder](../data/elements/religion.murder), it can be referred to in any `.murder` file in the [data](../data) folder.
Organising the files is thus just a matter of dividing each notions into files that are meaningful to you.

The two keywords `begin` and `end` define a block that describes the defined category.
In its block, a category block defines how its name should be translated into the available languages, and includes a description of it.
The `translation` and `description` keywords are very similar in syntax: they are followed by a language-code, then a string of characters between `"`.
The language code is a two-letter or three-letter code associated to the language in [translations.json](../web/translations.json) (see [translations.md](./translations.md)).
It is not necessary to have a translation in all available languages, but having translations in as many languages as possible is better.

In addition to the `translation` and `description` declarations, a category can also be marked as being dependent of another category.
This is done through the `category` keyword.
For instance, here follows a snippet from [secrets.murder](../data/elements/secrets.murder).
```murder
category DarkSecrets
begin
  category NonKids

  translation en "Dark secrets"
  description en "Deep secrets which, if publicly revealed, may damage the live of some."
  (* Other languages *)
end
```
In this example, the category `DarkSecrets` is marked as being dependent of the category `NonKids`.
This means that any element dependent on the category `DarkSecrets` will also be dependent on the category `NonKids`.

## Attributes and Contacts

An attribute can be declared very similarly to a category, but with the `declare attribute` keyword.
Here is for instance a snippet from [identity.murder](../data/elements/identity.murder) declaring the `Gender` attribute.
```murder
declare attribute Gender
begin
  translation en "Gender"
  (* Other translations *)
end
```
As for categories, an attribute can to be translated with the `translation` keyword.
It can’t be described through the `description` keyword as its description would never be shown to the user.

Once the attribute has been defined, one can define attribute values (see [memory.md](./memory.md) for more details).
In this case `Male` and `Female` (more genders are actually defined in [identity.murder](../data/elements/identity.murder)).
```murder
attribute Gender Male
begin
  translation en "male"
  (* Other translations *)
end

attribute Gender Female
begin
  translation en "female"
  (* Other translations *)
end
```

In [identity.murder](../data/elements/identity.murder), the attribute values `Male` and `Female` have other `add` and `compatible` commands.
The `add` commands are explained in [language.md](./language.md), whilst the `compatible` commands are explained in the advanced feature section below.
In most attribute declarations, you won’t need to use neither the `compatible with` nor the `add` commands.

As for categories, an attribute can be marked to depend on a category.
For instance, here is a snippet from [religion.murder](../data/elements/religion.murder) where the attribute `DivineAllegiance` is marked as being dependent on the category `Religion`.
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
Here is for instance a snippet from [relations.murder](../data/elements/relations.murder).
The contact `Know` is defined, with possible values `True`, `Intimate`, and `False`.
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
I advise to mark “default” values as internal.
In this case, if no contact is shown between two characters, one will probably assume that the `Know` contact is `False` between them: there is no need to add this cumbersome information to the user.

Attribute values can naturally also be set as internal.
One can also directly set an attribute or a contact as internal: this will mark all its values as being internal, effectively hiding this attribute or this contact to the user.
An example can be found in [objectives.murder](../data/elements/objectives.murder), where the contact `Goal` is defined.
This contact is meant to explicit the goals of each characters with respect to each other characters.
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
- it would be preferable if your element could be easily played.  For instance, avoid magical objects that couldn’t be simulated with role-playing rules or whose associated rule would spoil the secrecy of the object.  For instance, time-manipulating artefacts are extremely difficult to simulate in an actual play, unless they are very restricted on how they work.
- the best elements are elements that can yield interesting role-play: character background, emotional traits, stories between players, internal thoughts about another player, etc.
- an element is best if it can change between two scenario generations.  In particular, it may be interesting to cut some elements into several parts.  For instance, if you are describing how two people got to know each other, marry each other, then divorce, consider splitting this element into three parts: how they met each other, how they married, and why they divorced.  This will enable you and other writers to write variants of each of these three events (several possible ways to get to know each other, several ways to organise a wedding, etc.), leading to much more diversity in the generated scenarios.
- on the contrary, elements should not be too small of a story.  Indeed, the generator can choose not to apply a particular element: you can’t assume that a particular element will be present in the final scenario—or will be present in the way you expected it to be.  This means that each element has to work in isolation: an element shouldn’t only be interesting if another element is present in the scenario.  This means that elements sometimes can’t be split into several elements.
- a good way to get the right element length is to ask whether there is a single action in the element.  Think of it as the unity of action in classical dramatic tragedy.  If the unity of action is satisfied, it’s probably a good element.
- thanks to the category mechanism, users can protect themselves from harmful themes: do not restrict yourself when writing elements.  Just double-check that it is adequately marked in the right categories.

## Basics

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
It is however rare to need to do so as categories are automatically inferred from the attributes and contacts that the element uses: if for instance an element makes use of the attribute `DivineAllegiance` (which is marked as depending from category `Religion`), then there is no need to add a `category Religion` to the element as it will be automatically inferred.

Inside an element there are two parts: what the element needs, and what the element provides.
There is no particular order for these, but I strongly advise to start by the needs and ends by what is provided.

## Declaring What an Element Needs

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
Think of `A`, `B`, and `C` as variable names.

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
For instance, here is a snippet from [job.murder](../data/elements/job.murder) where the character `P` has two constraints: not only he or she can’t be a seller, but its family relation to the character `S` has to be `None`.
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
This means that in extreme cases the generated scenario may not explain why the requirements are present (in the example above, there might be no explanation on why `S` is a seller, for instance).
This is the reason why an element has to work in isolation and has to be associated to some kind of unity of action.

Let us now focus on what the element provides.
An element can provide all the notions described in [memory.md](./memory.md): events, attributes, contacts, relations, difficulty, and complexity.

## Adding Difficulty or Complexity

Let us start with the difficulty and complexity, as they are quite simple.
If you consider that your element makes a character more complex to understand or more difficult to play, you can use the `add` command.
Conversely, one can use the `remove` command if you consider that an element eases the understanding (that is, decreases the complexity) or the difficulty to play.
In this example, the element adds two points of difficulty to `A` and removes one point of complexity to `C`.
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
Adding individual difficulty or complexity points can be useful, but it is in general much better to express these as a relation.
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
The possible values for relations are explained in [memory.md](./memory.md).
The possible values are: `neutral`, `hate`, `trust`, `chaotic`, `undetermined`, and `avoidance`, as well as their strong variants (`strong hate`, `strong trust`, etc.).
The value `strong neutral` is possible, but it does nothing by itself.
It however (as any other `strong` relation) makes any further relation between the two player strong.
Thus, if an element provides a `strong neutral` relation between two players, and that another element adds a `trust` relation, then it becomes a `strong trust` relation automatically.

If the relation is symmetrical, one can of course double the commands as in:
```murder
  provide relation from A to B as trust
  provide relation from B to A as trust
```
There is however a useful shortcut: the following line is equivalent to the two lines above.
```murder
  provide relation between A and B as trust
```
As stated in [memory.md](./memory.md), relations are not meant to tell the program whether an element applies.
This means that even if the current relation between `A` and `B` is `hate`, an element with the command above will still apply (leading to an exploding mixture of love and hate).

To prevent an element for applying, one has to use attributes and contacts.
We have seen above how to assume that some combinations of attributes and contacts are present in the scenario.
Let us now see how to write attributes and contacts to the current state.

## Writing Attributes and Contacts

The syntax is quite close to relation.
Here are two commands adding the attribute `Job` with value `Seller` to `B`, and adds the contact `FamilyRelation` from `A` to `B` with value `Sibling`.
```murder
  provide attribute Job to B as Seller
  provide contact FamilyRelation from A to B as Sibling
```
Such commands are however very rarely done directly in the block of an element.
Instead, most of them are done directly inside an event: this helps provide a timestamp to when particular attribute or contact are defined (when `B` became a seller, when two people got to know each other, etc.).
Let us thus see how events can be declared.

## Events

### Basics

One can use the `provide` command to add an event.
In contrary to the other instances of `provide` above, `provide event` is followed by a block: events have to be translated, and thus needs lots of information!
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
Second, as explained in [memory.md](./memory.md), events have lengths.
We can’t directly express their length, but we can express in which kind of length they fall thanks to the `lasting` command.
The possible values of time after the `lasting` command are: `seconds`, `minutes`, `days`, `weeks`, `years`, and `decades`.
Instead of using the `lasting` keyword, it is possible to prefix the adjectives `immediate`, (`very`) `short`, `medium`, (`very`) `long`, or `life`, as in the following example.
Its meaning it exactly the same than with the corresponding `lasting` keyword (it is however rarely used except for the `immediate` adjective): the element below is exactly the same that than the one above.
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

### Event Assumptions

We now focus on the event block itself.
Most event blocks start with the `event Personal` command:
```murder
  provide event lasting weeks to A and B
  begin
    event Personal
  end
```
The keyword `event` is similar to `category` in that it marks the event as being `Personal`.
Personal events are events personally involving characters (instead of just describing things to them).
Other kinds of events exist (`Work`, `Dream`, etc.) and they can simply be declared in the same way than categories—outside an element—with the `declare event` command.
This command is detailed in the advanced features.

Events themselves are also made of several parts: one stating where this event should be placed, what it provides, and what are its sentences.
We have already seen the command providing attributes or contacts: it is exactly the same than the one for the element (`provide attribute Job to B as Seller`, `provide contact FamilyRelation from A to B as Sibling`, etc.), just that it is provided in the event.
Do not provide the same attribute or contact twice: if an event provides it, do not repeat it in the element.
Here is thus an example of a valid element.
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

Commands stating where the event should be placed in time start with the `assume` keyword.
It enables to ensure that, for a particular character, at least one particular event already happened before, will happen after, or that no particular event happened before or will happen after.
One can describe an event by its event kinds (`Personal`, `Work`, `Dream`, etc.).
For instance, to state that no event about work happened to `A` before, one can write:
```murder
  provide event lasting weeks to A and B
  begin
    event Personal
    assume no event Work to A before
  end
```
If one would prefer to assume that there will be an event about work after, one should write:
```murder
  provide event lasting weeks to A and B
  begin
    event Personal
    assume event Work to A after
  end
```
Commands of the form `assume event` may be difficult to ensure for the solver: it will definitely try to look for elements that would provide such an event, but may fail in doing so.
However, commands of the form `assume no event` are guaranteed to be satisfied.

In addition to refer to event kinds, one can describe events in `assume` command according to what attribute or contact it provided.
This is typically used to state that an event should happen after that the assumptions of the element are requested.
For instance in our running example, one might want to add the following two `assume` commands.
Note how we do not repeat the values `True` and `Priest`: we only specify in `assume` commands which attribute or contact has been provided, not its value.
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

### Event Translations

Finally, events should have translations.
This is crucial as these translations will be the ones shown to the players.
As for categories, attributes, and contacts, translations are defined by the `translation` keyword.
However, the syntax is here more powerful as it enables to use the names and words associated to the characters defined in the element.
For instance, it will be possible to use the names of `A`, `B`, and `C` in the example.
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

## Conclusion

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

## About Comments

Comments can be nested: `(* (* *) *)`.
This especially useful when commenting out large portions of code: there is no need to “break” comments inside.

If a comment starts with a star, like `(* * My Section *)`, it means that it starts a thematic section.
If it has two stars, like `(* ** My Subsection *)`, it defines a subsection.
These are of course still ignored by the parser, but it is a nice way to indicate that the elements in a section are related in some ways.

## Unique and Duplicable Elements

By default, an element will not be applied more than once per character: if a character is already involved in an element, it can’t be involved again (even with a different instantiation).
For instance, let us consider the following snippet from [notary.murder](../data/elements/notary.murder):
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
Here is a snippet from [religion.murder](../data/elements/religion.murder):
```murder
unique element HarmonyDescription
begin
  let G be player with attribute Specy as God
                  with attribute DivineAllegiance as Harmony
```
This element describes a particular god.
Only one player can play this god in a particular scenario: the `unique` keyword enforces that.

As the scenario element writer, you might sometimes know that a scenario element can’t possibly be applied more than once.
This is for instance the case of most of the (technical) scenario elements defined in [language.murder](../data/elements/language.murder).
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
For instance, here is a snippet from [religion.murder](../data/elements/religion.murder):
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

## Advanced Constraints

Some elements do just not work if some conditions are met.
Typically, if someone is part of the same family, one can hardly assume that they only met for the first time during their activity.
It is thus important to be able to restrict the application of scenario elements.
This can be done with the `not as` keyword.

For instance, here is a snippet from [dreams.murder](../data/elements/dreams.murder):
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
For instance, here is a snippet from [family.murder](../data/elements/family.murder):
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
For instance, here is a snippet from [family.murder](../data/elements/family.murder):
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

## Event Kinds

We said in the Event Assumptions section that events can be marked with event kinds, such as `Personal` or `Work`.
Such event kinds can be declared using the `declare event` command.
Here is for instance for the event `Personal` is declared in [identity.murder](../data/elements/identity.murder):
```murder
declare event Personal
```
That’s it.
Thanks to this declaration, one can declare events in scenario elements as `Personal`, as shown in the Event Assumptions section above:
```murder
  provide event lasting weeks to A and B
  begin
    event Personal
  end
```
Note that a given event may very well have more than one event kind: it just needs to include more than one `event` command.

It is possible to include dependencies between events.
For that, just add `event` commands in a block after the `declare event` command.
For instance, here is a snippet from [job.murder](../data/elements/job.murder):
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
For instance, here is a snippet from [notary.murder](../data/elements/notary.murder):
```murder
declare event MoneyExchange
begin
  event Personal
  category Money
end
```
This declaration means that the event kind `MoneyExchange` depends on the event kind `Personal` (that is, any exchange of money personally involves a character), but also that it depends on the category `Money`: any scenario element providing an event of kind `MoneyExchange` will be dependent on the `Money` category.

## Phantom Events

Some events are just there to express some constraints.
Such events are usually not associated to any translations, and it would be silly to display them in character sheets.
The `phantom` keyword tells the program that this event is indeed just there for expressing constraints and should not be exported.

For instance, to express in a scenario element that a character `B` is older than a character `A`, one can provide an event as follows (this is a snippet from [identity.murder](../data/elements/identity.murder)):
```murder
  provide immediate phantom event to A and B
  begin
    assume no event Birth to A before
    assume no event Birth to B after
  end
```
This event is declared `phantom` and is thus associated no translations (any translation will be rejected by the program in such an event).
This event assumes some constraints about the birth events of `A` and `B`, enforcing `B` to be older.
This event is `immediate`, so it won’t conflict with another event: if it were to last years, this would make an artificial hole in `A` and `B`’s life which would be difficult to explain.
This event is thus purely here to ensure that indeed, `A`’s birth happened before than `B`.

Another usage of phantom events is to space two events.
To illustrate, here is a snippet from [romance.murder](../data/elements/romance.murder):
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

## Blocking Events

By default, an event “books” the space for a player according to its duration: two events lasting years can’t intersect themselves if they involve the same characters, but an event lasting years and an event lasting minutes can.
Sometimes, one may want to overwrite this default behaviour by preventing any other event from happening at the same time if they involve a common character.
For instance, in [wounds.murder](../data/elements/wounds.murder), a scenario element describes a terrible event for a victim who ends up in an hospital:
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
For instance, here is a snippet from [identity.murder](../data/elements/identity.murder):
```murder
element BirthOld
begin
  let P be player

  provide immediate event to P
  begin
    event Birth
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
This is by the way because of this trick that the event kind `Birth` is not declared to be dependent on the event kind `Personal`: if `Birth` was `Personal` (although it morally is), one could not prevent other events to be placed before a special “beginning of actual life”-event (in this case, the blocking phantom event).

## Compatibilities

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
For instance, [family.murder](../data/elements/family.murder) declares a value `True` for `FamilyRelation` in addition to all the specific values for family relations.
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

## Strictness

TODO: describe the strictness flag on attribute and contacts.

