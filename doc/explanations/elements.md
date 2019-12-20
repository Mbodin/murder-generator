
This file aims to provide a detailed explanation on how to write scenario elements.
I strongly advise reading [memory.md](./memory.md) before reading this tutorial, as the notions of the memory model will be extensively used here.
This file presents the fundamental features to write elements, then [advanced,md](./advanced.md) present more advanced features.

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
This is a snippet from [religion.murder](../../data/elements/religion.murder), defining the category named `Religion`.
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

There is no particular place where each command needs to be: despite the category `Religion` is defined in [religion.murder](../../data/elements/religion.murder), it can be referred to in any `.murder` file in the [data](../../data) folder.
Organising the files is thus just a matter of dividing each notions into files that are meaningful to you.

The two keywords `begin` and `end` define a block that describes the defined category.
In its block, a category block defines how its name should be translated into the available languages, and includes a description of it.
The `translation` and `description` keywords are very similar in syntax: they are followed by a language-code, then a string of characters between `"`.
The language code is a two-letter or three-letter code associated to the language in [translations.json](../../web/translations.json) (see [translations.md](../references/translations.md)).
It is not necessary to have a translation in all available languages, but having translations in as many languages as possible is better.

In addition to the `translation` and `description` declarations, a category can also be marked as being dependent of another category.
This is done through the `category` keyword.
For instance, here follows a snippet from [secrets.murder](../../data/elements/secrets.murder).
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
Here is for instance a snippet from [identity.murder](../../data/elements/identity.murder) declaring the `Gender` attribute.
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
In this case `Male` and `Female` (more genders are actually defined in [identity.murder](../../data/elements/identity.murder)).
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

In [identity.murder](../../data/elements/identity.murder), the attribute values `Male` and `Female` have other `add` and `compatible` commands.
The `add` commands are explained in [language.md](./language.md), whilst the `compatible` commands are explained in [advanced.md](./advanced.md).
In most attribute declarations, you won’t need to use neither the `compatible with` nor the `add` commands.

As for categories, an attribute can be marked to depend on a category.
For instance, here is a snippet from [religion.murder](../../data/elements/religion.murder) where the attribute `DivineAllegiance` is marked as being dependent on the category `Religion`.
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
Here is for instance a snippet from [relations.murder](../../data/elements/relations.murder).
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
An example can be found in [objectives.murder](../../data/elements/objectives.murder), where the contact `Goal` is defined.
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

Once you have a good idea for your element, find a place in a `.murder` file in the [data](../../data) folder where it would fit (it can be placed anywhere as soon as it is not itself inside a block).
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
For instance, here is a snippet from [job.murder](../../data/elements/job.murder) where the character `P` has two constraints: not only he or she can’t be a seller, but its family relation to the character `S` has to be `None`.
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
The keyword `event` is similar to `category` in that it marks the event as being of kind `Personal`.
Personal events are events personally involving characters (instead of just describing things to them).
Other kinds of events exist (`Work`, `Dream`, etc.) and they can simply be declared in the same way than categories—outside an element—with the `declare event` command.
This command is detailed in [advanced.md](./advanced.md).

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
The file [advanced.md](./advanced.md) presents more advanced features: you will probably only rarely use them, but they may be very useful.
Now you can read the already written elements in the files of the [data](../../data) folder, and write your own!

