
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

TODO: Tutorial to write elements.

# Advanced Features

This section presents various features that may be useful when writing scenario elements.
There is no particular order in its subsections: they can be read in any order.

TODO: describe the strictness flag on attribute and contacts.

TODO: describe how to “break” compatible attributes and contacts by asking both “Female” and “not NonBinary”.

## Miscellaneous

Comments can be nested: `(* (* *) *)`.
This especially useful when commenting out large portions of code: there is no need to “break” comments inside.

