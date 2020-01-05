
This file describes the grammar of the files in the [data](../../data/elements) folder.
The file [elements.md](../explanations/elements.md) provides explanation for these data files.

# About this reference

Data files in [data](../../data/elements) are `murder` files.
These files are sequences of declarations.
The order of these declaration, or the file in which these declarations are, does not matter for the program.

In this file, user-defined named are placed between `<` and `>`.
For instance `<Category name>`.
Most of these names have to start by an uppercase letter—for instance `Fantasy`.
The only exception is for language-related features: `<language code>` and `<tag>` both have to start with a lowercase letter—for instance `en` and `adj`.
In this file, `<tags>` means a (possibly empty) sequence of `:<tag>`, `:+<tag>`, and `:-<tag>`.
In general, sequences are expressed by `<...>` in this file.

Some declarations expect a block.
In such blocks, some command kinds are expected.
Their respective order mostly does not matter, with some exceptions.
For instance, each event declared in an element are assured to be scheduled in the final scenario in the order of their declaration in the element.
Blocks starts with `begin` and ends with `end`.
They are expected immediately after the declaration.
Empty blocks can be safely removed.

Some declarations and command come with variants.
These variants have a slightly different meaning than the original command, but they behave similarly for the grammar.
In particular, if the original command expected a block with some commands, so will the variant.

# Declarations

## Declaring Attribute

```murder
declare attribute <Attribute name>
```
Commands expected in block: `category` and `translation` (simple).

Variant:
```murder
declare internal attribute <Attribute name>
```

## Declaring Attribute Constructor

```murder
attribute <Attribute name> <Attribute constructor name>
```
Commands expected in block: `category`, `translation` (simple), `add translation`, `add` (tag), and `compatible`.

Variant:
```murder
internal attribute <Attribute name> <Attribute constructor name>
```

## Declaring Contact

```murder
declare contact <Contact name>
```
Commands expected in block: `category` and `translation` (simple).

Variant:
```murder
declare internal contact <Contact name>
```

## Declaring Contact Constructor

```murder
contact <Contact name> <Contact constructor name>
```
Commands expected in block: `category`, `translation` (simple), `add translation`, `add` (tag), and `compatible`.

Variant:
```murder
internal contact <Contact name> <Contact constructor name>
```

## Declaring Object Kind

```murder
declare object <Object kind name>
```
Commands expected in block: `category` and `translation` (simple).

## Declaring Category

```murder
category <Category name>
```
Commands expected in block: `category`, `translation` (simple), `description`.

## Declaring Event Kind

```murder
declare event <Event kind name>
```
Commands expected in block: `category` and `event`.

## Declaring Grammar Notion

```murder
declare translation <language code>:<tag>
```

## Declaring Element

```murder
element <Element name>
```
Commands expected in block: `category`, `let`, `provide`, `add difficulty`, `add complexity`, `remove difficulty`, and `remove complexity`.

Variants:
```murder
duplicable element <Element name>
unique element <Element name>
```

# Commands

## Category Dependency

```murder
category <Category name>
```

## Event Dependency

```murder
event <Event kind name>
```

## Compatibility

```murder
compatible with <Attribute name or Contact name>
```

## Adding Difficulty

```murder
add difficulty to <Character name> 
```

Variant:
```murder
add difficulty to <Character name> and <...> and <Character name>
```

## Adding Complexity

```murder
add complexity to <Character name> 
```

Variant:
```murder
add complexity to <Character name> and <...> and <Character name>
```

## Removing Difficulty

```murder
remove difficulty to <Character name> 
```

Variant:
```murder
remove difficulty to <Character name> and <...> and <Character name>
```

## Removing Complexity

```murder
remove complexity to <Character name> 
```

Variant:
```murder
remove complexity to <Character name> and <...> and <Character name>
```

## Let Declarations

### Constraints

In this section `<constraints>` represent a (possibly empty) sequence of the following:
```murder
with attribute <Attribute name> as <Attribute constructor name>
with attribute <Attribute name> as <Attribute constructor name> or <...> or <Attribute constructor name>
with attribute <Attribute name> not as <Attribute constructor name>
with attribute <Attribute name> not as <Attribute constructor name> or <...> or <Attribute constructor name>
with contact <Contact name> to <Character name> as <Contact constructor name>
with contact <Contact name> to <Character name> as <Contact constructor name> or <...> or <Contact constructor name>
with contact <Contact name> to <Character name> not as <Contact constructor name>
with contact <Contact name> to <Character name> not as <Contact constructor name> or <...> <Contact constructor name>
with contact <Contact name> to any other player as <Contact constructor name>
with contact <Contact name> to any other player as <Contact constructor name> or <...> or <Contact constructor name>
```

### Character

```murder
let <Character name> be player <constraints>
```

Variant:
```murder
let any other player be <constraints>
```

### Object

```murder
let <Object name> be <Object kind name> <constraints>
```

## Provide

### Relation

```murder
provide relation from <Character name> to <Character name> as <relation>
```

Variant:
```murder
provide relation between <Character name> and <Character name> as <relation>
```

In this command, `<relation>` can be any of the followings:
```murder
neutral
strong neutral
hate
strong hate
trust
strong trust
chaotic
strong chaotic
undetermined
strong undetermined
avoidance
strong avoidance
```

### Attribute

```murder
provide attribute <Attribute name> to <Character name> as <Attribute constructor name>
```

Variants:
```murder
provide attribute <Attribute name> to <Character name> as <Attribute constructor name> or <...> or <Attribute constructor name>
provide attribute <Attribute name> to any other player as <Attribute constructor name>
provide attribute <Attribute name> to any other player as <Attribute constructor name> or <...> or <Attribute constructor name>
provide attribute <Attribute name> to any player as <Attribute constructor name>
provide attribute <Attribute name> to any player as <Attribute constructor name> or <...> or <Attribute constructor name>
provide strict attribute <Attribute name> to <Character name> as <Attribute constructor name>
provide strict attribute <Attribute name> to any other player as <Attribute constructor name>
provide strict attribute <Attribute name> to any other player as <Attribute constructor name> or <...> or <Attribute constructor name>
provide strict attribute <Attribute name> to any player as <Attribute constructor name>
provide strict attribute <Attribute name> to any player as <Attribute constructor name> or <...> or <Attribute constructor name>
```

### Contact

```murder
provide contact <Contact name> from <Character name> to <Character name> as <Contact constructor name>
```

Variants:
```murder
provide contact <Contact name> from <Character name> to <Character name> as <Contact constructor name> or <...> or <Attribute constructor name>
provide contact <Contact name> between <Character name> and <Character name> as <Contact constructor name>
provide contact <Contact name> between <Character name> and <Character name> as <Contact constructor name> or <...> or <Attribute constructor name>
provide strict contact <Contact name> from <Character name> to <Character name> as <Contact constructor name>
provide strict contact <Contact name> from <Character name> to <Character name> as <Contact constructor name> or <...> or <Attribute constructor name>
provide strict contact <Contact name> between <Character name> and <Character name> as <Contact constructor name>
provide strict contact <Contact name> between <Character name> and <Character name> as <Contact constructor name> or <...> or <Attribute constructor name>
```

### Event

```murder
provide event lasting <duration> to <Character name>
```
Commands expected in block: `event`, `assume`, `provide attribute`, `provide contact`, `sentence, `translation` (grammar), and `translation` (simple).

Variants:
```murder
provide event lasting <duration> to <Character name> and <...> and <Character name>
provide blocking event lasting <duration> to <Character name>
provide blocking event lasting <duration> to <Character name> and <...> and <Character name>
provide phantom event lasting <duration> to <Character name>
provide phantom event lasting <duration> to <Character name> and <...> and <Character name>
provide blocking phantom event lasting <duration> to <Character name>
provide blocking phantom event lasting <duration> to <Character name> and <...> and <Character name>
provide <time> event to <Character name> and <...> and <Character name>
provide <time> blocking event to <Character name>
provide <time> blocking event to <Character name> and <...> and <Character name>
provide <time> phantom event to <Character name>
provide <time> phantom event to <Character name> and <...> and <Character name>
provide <time> blocking phantom event to <Character name>
provide <time> blocking phantom event to <Character name> and <...> and <Character name>
```

`<duration>` can be any of the following:
```murder
seconds
minutes
days
weeks
years
decades
```

`<time>` can be any of the following:
```murder
immediate
very short
short
medium
long
very long
life
```

## Assume

### An Event Kind

```murder
assume event <Event kind name> to <Character name> before
```

Variants:
```murder
assume event <Event kind name> to <Character name> after
assume no event <Event kind name> to <Character name> before
assume no event <Event kind name> to <Character name> after
assume event <Event kind name> to <Character name> and <...> and <Character name> before
assume event <Event kind name> to <Character name> and <...> and <Character name> after
assume no event <Event kind name> to <Character name> or <...> or <Character name> before
assume no event <Event kind name> to <Character name> or <...> or <Character name> after
```

### An Attribute

```murder
assume event providing attribute <Attribute name> to <Character name> before
```

Variants:
```murder
assume event providing attribute <Attribute name> to <Character name> after
assume no event providing attribute <Attribute name> to <Character name> before
assume no event providing attribute <Attribute name> to <Character name> after
assume event providing attribute <Attribute name> to <Character name> and <...> and <Character name> before
assume event providing attribute <Attribute name> to <Character name> and <...> and <Character name> after
assume no event providing attribute <Attribute name> to <Character name> or <...> or <Character name> before
assume no event providing attribute <Attribute name> to <Character name> or <...> or <Character name> after
```

### A Contact

```murder
assume event providing contact <Contact name> from <Character name> to <Character name> before
```

Variants:
```murder
assume event providing contact <Contact name> from <Character name> to <Character name> after
assume no event providing contact <Contact name> from <Character name> to <Character name> before
assume no event providing contact <Contact name> from <Character name> to <Character name> after
assume event providing contact <Contact name> from <Character name> and <...> and <Character name> to <Character name> and <...> and <Character name> before
assume event providing contact <Contact name> from <Character name> and <...> and <Character name> to <Character name> and <...> and <Character name> after
assume no event providing contact <Contact name> from <Character name> or <...> or <Character name> to <Character name> or <...> or <Character name> before
assume no event providing contact <Contact name> from <Character name> or <...> or <Character name> to <Character name> or <...> or <Character name> after
```

## Add Translations

```murder
add translation <language code><tags> "<translation string>"
```

## Add tag

```murder
add <language code>:<tag>
```

## Translations

### Simple

```murder
translation <language code> "<translation string>"
```

### Grammar

```murder
translation <language code> <translation items>
```

Translation items is a (possibly empty) sequence of `"<translation string>"` and `<Character name><tags>`.
In particular, it can be a simple translation.

## Description

```murder
description <language code> "<description string>"
```

## Sentence

```murder
sentence
```
Commands expected in block: `translation` (grammar).

