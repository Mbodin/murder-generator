
This file presents the language features that can be found in the [data](../../data) files.
For the documentation related to the program interface, see [translations.md](../references/translations.md).

This file is divided into the following parts:
- the first part describes grammar-less translations (typically associated to `category`. `attribute`, and `contact` translations),
- the second part describes how grammar-based translations can be defined (in particular in events),
- the third part describes the file [language.murder](../../data/elements/language.murder) and how it defines grammar constructs,
- the last part describes advanced usages.

In all this file, we assume that you know the language code of the language that you are translating.
This language code is the two-letter or three-letter code associated to the language in [translations.json](../../web/translations.json) (see [translations.md](../references/translations.md) for more details).
Let us also remind that it is not a prerequisite to translate all of the program data in all available languages, just that it will make parts of the data unusable for some language, thus restricting the number of elements considered by the program.

# Grammar-less Translations

These constructs are actually already described in [elements.md](./elements.md).
Their structure is quite simple: in the block defining the object (usually an `attribute` or a `contact`), it consists of a `translation` command, followed by the language code, followed by the translation between quotes.
There can be as many `translation` command for all languages, but a single language should not be repeated.

This applies for both the `declare attribute` and `declare contact` commands (that define the attribute and the contact) and the `attribute` and `contact` commands (that define values for the `attribute` or the `contact`).
For instance, the following snippet from [relations.murder](../../data/elements/relations.murder) declares a contact `Know` and one of its value `True`.
```murder
declare contact Know
begin
  translation en "Knows"
  translation eo "Konas"
  (* Other translations *)
end

contact Know True
begin
  translation en "yes"
  translation eo "jes"
  (* Other translations *)
end
```
Usually, the translation for the `declare` variant starts with an uppercase letter and translation for values with a lowercase letter.
It is however up to the translater to follow these guidelines.
The given translation will be used thoroughly in the exported file, whenever one of these component needs a localised version.

Another place where we grammar-less translations are used in `category` blocks.
Such blocks can contains two kinds of translations: the `translation` commands, which we have already explained, and the `description` commands, which behave very similarly.
The given description is used in the beginning of the generation, to describe the category to the user.
Here is a snippet from [religion.murder](../../data/elements/religion.murder):
```murder
category Religion
begin
  translation en "Religion"
  translation eo "Religio"
  (* Other translations *)

  description en "God(s), priests, workships, etc."
  description eo "Dio(j), preĝistoj, diservoj, ktp."
  (* Other translations *)
end
```

The grammar-less translations are pretty simple.
Let us now focus on grammar-based translations.

# Grammar-based Translations

Grammar-based translations are translations that can include context-dependent features.
Such translations can for instance define different translations depending on the context, or insert the name of a character in a sentence.
Such grammar-based translations are the ones that can be found in elements, and more particularly, events.

Such translations are also based on the `translation` keyword, but this command accepts a more variety arguments.
One can still attach a grammar-less sentence to these `translation` commands, as in the first section.
However, one can also cut them into pieces, and insert character’s names in between.
Here is for instance a simplified snippet from [romance.murder](../../data/elements/romance.murder):
```murder
element TwoPeopleFallingInLove
begin
  let A be player
  let B be player

  provide event lasting seconds to A and B
  begin
    event Personal

    sentence
    begin
      translation en "During a train trip, " A " and " B " saw each other through a window."
      (* Other translations *)
    end

    sentence
    begin
      translation en "They immediately fell in love."
      (* Other translations *)
    end
  end
end
```
In this snippet, we can see that the second sentence does not use any grammar element: it is a simple translation between quotes.
The first sentence is actually composed of five parts:
- a first part `"During a train trip, "` (notice the final space),
- a character identifier `A`, that will be replaced by the actual name of this character,
- another part `" and "` (also, notice the trailing and final space),
- the identifier of another character `B` (that will also be replaced by the character name),
- the end of the sentence `" saw each other through a window."`.
The program will simply replace `A` and `B` by their name (say `John` and `Mary`) and concatenate all parts of the translation to produce `During a train trip, John and Mary saw each other through a window.`.

The spaces around `" and "` are important as the program directly concatenate the translations: if one were to have written `"and"` instead of `" and "`, the final sentence would have been `During a train trip, JohnandMary saw each other through a window.`, which is not what is here wanted.
Sometimes, one doesn’t want these spaces.
For instance, in English, the genitive “’s” is placed immediately after the name (“John’s”, not “John ’s”): one would write a command like `translation en "This is " A "’s example."` to account for this

Sometimes, one doesn’t want to place the name of a character, but a word that depends on this character.
This can be a pronoun like “he” or “she”, but also possessive pronouns like “his” and “her”, or clitics like “him” and “her”.
This is the example of the file [basic.md](../tutorials/basic.md).
This is where the grammar takes all its meaning.

This is done through grammar annotations.
They all starts by a `:` symbol and are added after the character identifier.
For instance, in English, the grammar annotation requesting the possessive (“his”/“her”) is `:+pos`, leading to the translation part `A:+pos`.
Here is an example from [job.murder](../../data/elements/job.murder)
```murder
element ThiefBecausePoor
begin
  let P be player with attribute Richness as Poor

  provide event lasting weeks to P
  begin
    event Work
    assume event providing attribute Richness to P before

    provide attribute Job to P as Thief

    translation en "Due to the circumstances, " P " chose thief as " P:+pos " profession."
    (* Other translations *)
  end
end
```
The first `P` will be translated by the character’s name (say `John`), but the second `P:+pos` has the possessive grammar annotation and is instead translated by, say, `his`, leading to the sentence `Due to the circumstances, John chose thief as his profession.`.

The grammar annotations are language dependent and defined in the file [language.murder](../../data/elements/language.murder).
Here are some common grammar annotations:
- `:+sbeg` if the word is at the beginning of the sentence,
- `:+pro` for pronoun,
- `:+pos` for possessive.
These annotations are used extensively thorough the files of [elements](../../data/elements).
Note that these annotations compose: if `P:+pro` is `he`, then `P:+sbeg:+pro` is `He`.
Some combinations may not make sense.
For instance a word can’t be both a personal pronoun and a possessive one, so `P:+pro:+pos` is meaningless.

Here are some more advanced common annotations:
- `:+nom` for a description of the character (“the man”, “the seller”, etc.),
- `:-def:+nom` for an undefinite description (“a man”, “a seller”, etc.),
- `:+empty` for an empty translation,
- `:fem` to filter for female characters and `:mas` to filter for male characters.
Let us explain the last one.

Here is a snippet from [religion.murder](../../data/elements/religion.murder):
```murder
unique element InternalPeaceDescription
begin
  let G be player with attribute Specy as God
                  with attribute DivineAllegiance as InternalPeace

  provide immediate event to G
  begin
    event Description
    assume no event Personal to G before

    sentence
    begin
      translation en G:+sbeg " is the god of peace, painlessness, and ataraxia."
      translation en G:+sbeg:fem " is the goddess of peace, painlessness, and ataraxia."
      (* Other translations *)
    end
  end
end
```
The first translation is unfortunate as the word “god” is not fit for female gods, where “goddess” is more appropriate.
We thus provide two translations: one with “god”, and one with “goddess”.
We furthermore add in the second translation the `:fem` filter on `G`: this translation is only valid if `G` is female.
If more than one translation is given, the program considers the one with the most specific filters: in this case, if `G` is female, both `G:+sbeg` and `G:+sbeg:fem` are valid translation, but the second one is more specific, and is thus favored by the program.
If `G` is male, only `G:+sbeg` is valid and the program chooses this one by default.
If several translations are as specific, the program chooses one at random.
This can be useful to provide some diversity in the translations.
For instance, the element `LeaderOfTheVerySecretSocietyLearnsAboutMembers` from [secrets.murder](../../data/elements/secrets.murder) defines several translations equally specific:
```murder
    sentence
    begin
      translation en "The idea was good, but stealthiness is to be improved: " L:+pro " quickly realised that someone wanted to give " L:+cli " a message… and stayed still to avoid attracting attention."
      translation en "The idea was good, but dexterity is to be improved: the letter fell over in a quite visible way as soon as " P:+pro " went away."
      (* Other translations *)
    end
```
This means that this sentence will be randomly translated into one variant or the other.

The example about `G` above assumed that `G` appeared in the sentence.
But was if it is not the case, but we still need to make the translation depend on `G`?
This is where the `:+empty` annotation comes in: this annotation makes the translation empty, and can thus be safely added into any translation.
It can naturally be composed with the `:mas` and `:fem` annotations.
Hence, in [family.murder](../../data/elements/family.murder), we can read these translations:
```murder
    sentence
    begin
      translation en B:+sbeg:+pro " has been told over and over that " B:+pro " had a sibling."
      translation en B:+sbeg:+pro " has been told over and over that " B:+pro " had a sister." A:+empty:fem
      translation en B:+sbeg:+pro " has been told over and over that " B:+pro " had a brother." A:+empty:mas
    end
```
Let us say that `B` is a girl for the sake of the example (and thus that `B:+sbeg:+pro` is translated by `She`).
This sentence can be translated in either `She has been told over and over that she had a sibling.`, `She has been told over and over that she had a sister.`, or `She has been told over and over that she had a brother.`, depending on `A`, despite this character not being directly referenced in the sentence.

As all these features are language dependent, we can’t present them in detail: see the file [language.murder](../../data/elements/language.murder) to know what are the grammatical notions that has been declared for your language.

# The File [language.murder](../../data/elements/language.murder)

This file describes all the grammatical notions of a language.
These notions roughly correspond to case in case-based languages, but they can be more general.
To declare a new grammatical notion, use the `declare translation` command, followed by the command, followed by `:`, followed by an identifier for your grammatical notion.
For instance, here are some declarations for English:
```murder
declare translation en:nom (* Nouns *)
declare translation en:pro (* Pronouns *)
declare translation en:adj (* Adjectives *)
declare translation en:adv (* Adverbs *)
```
Note that no symbol `+` or `-` appear in such declarations.
In general, try to at least include the notions presented in the previous section (unless they really doesn’t make sense in your language).

These notions can then be link to attributes.
This is done using the `add` command directly in the block declaring the attribute.
For instance, consider the following snippet from [identity.murder](../../data/elements/identity.murder):
```murder
attribute Gender Male
begin
  translation en "male"
  (* Other translations *)

  add en:mas
  (* Other commands *)

  add translation en:adj:sin "male"
  (* Other commands *)
end

attribute Gender Female
begin
  translation en "female"
  (* Other translations *)

  add en:fem
  (* Other commands *)

  add translation en:adj:sin "female"
  (* Other commands *)
end
```
This snippet adds the grammatical notions `mas` for male characters and `fem` for female characters.

In the same snippet, we can see a `add translation` command.
These are conditional translations: if it respects the filters provided after the `add translation` command, then the character can be translated by this (grammar-less) translation.
This means that writing `P:+adj:+sin` in an event translation will trigger the `add translation` defined above, yielding either `male` or `female` depending on the case.

In most cases, these translations are not particularly attached to an attribute in particular.
For this reason [language.murder](../../data/elements/language.murder) defines an ad-hoc attribute whose only purpose is to define these attribute-less translations.
This file also makes sure that every character in the game are associated this attribute.
For English, this is done as follows:
```murder
declare internal attribute GrammarEN
begin
  translation en "English grammar"
  (* Other translations *)
end

attribute GrammarEN Normal
begin
  translation en "normal"
  (* Other translations *)

  add en:sin
  add en:def
  add translation en:empty ""
  add translation en:pro:sin "it"
  add translation en:pro:sin:mas "he"
  add translation en:pro:sin:fem "she"
  (* Lots of translations *)
end

unique element NormalGrammarEN
begin
  provide strict attribute GrammarEN to any other player as Normal
end
```
The whole file [language.murder](../../data/elements/language.murder) is organised as such, for each language.
Note that in English, each person is associated the grammar notions `sin` and `def` by default: by default, a person is thus singular and definite.

In the rare case that a language uses different grammar for different people (for instance, if it differentiate monarchs and normal person), it is possible to define several values for the ad-hoc grammar attribute, with all their associated translations.
This is by the way why each language has to define its own grammar: the possible values might differ from languages to languages.

We can now explain the grammar annotations introduced in the previous section: in event translations, the grammar annotations are temporary changes on the grammar notions associated to a character.
The explicit provision of grammar annotation in event translations is performed by using a `:+` annotation: `P:+sbeg` means “temporary adds the `sbeg` grammar annotation to `P` and translate it”.
Similarly, one can use a `:-` to temporarily remove an annotation in an event translation.
Thus, `P`:-def:+nom` means “temporarily removed the grammar notion `def` to `P`, temporarily adds the `nom` grammar notion, and translate `P`”.
If no `+` or `-` is written in the event translation, it is then a filter on the grammar notions associated to the character.

Note that these are the same for the `add translation` commands: most of the time, these commands just filter on the grammar notions associated to the current character, but they can also provide or remove these by using `:+` or `:-` annotations.
This is especially used in languages whose words are genderred.
For instance, in French, there are two plural pronouns: “ils” and “elles”.
The second one is used if all the member of the described group are female, and the first one is used for all other cases.
The first one is however male.
This leads to the following declaration:
```murder
attribute GrammarFR Normal
begin
  translation fr "normale"
  translation en "normal"
  (* Other translations *)

  add translation fr:pro:plu:-fem:+mas "ils"
  add translation fr:pro:plu:fem "elles"
  (* Other commands *)
end
```
Either all the members of the group are female, and them the `:pro:plu:fem` filter applies (and as the program always chooses the most specific translation, it is guaranteed to choose this one).
Otherwise, the first declaration (with no gender-related filters—only `:pro:plu`) is chosen, leading to `ils` as a translation. 
However, `ils` is male in French: we also have to remove the `fem` grammar notions and ensure that the `mas` is present (it probably is, but it is better to be safe).
This is done using the grammar annotations `:-fem:+mas`.
Overall, this leads to the `:pro:plu:-fem:+mas` grammar annotation.
When called in an event translation using `G:+pro:+plu`, the group `G` will pick the right translation and its grammatical gender will be (temporarily, for the time of the sentence) updated accordingly.

To sum up, the file [language.murder](../../data/elements/language.murder) defines grammar notions.
These grammar notions can be provided either by linking them to an attribute using the `add` command in the attribute declaration, or explicitly in event translations.
This same file also provides an ad-hoc attribute for each language to store some default grammar notions and translations, and ensures that it is applied to every character using an ad-hoc element.

# Advances Usages

We have seen that we can add translations in attribute declaration.
This is useful for attributes leading to a description, such as genders, species, or jobs.
For instance, the following job declaration also declares translations for the `nom` grammar notion.
```murder
attribute Job Seller
begin
  translation en "seller"
  (* Other translations *)

  add translation en:nom:sin "a seller"
  add translation en:nom:plu "sellers"
  add translation en:nom:def:sin "the seller"
  add translation en:nom:def:plu "the sellers"
  (* Other translations *)
end
```

