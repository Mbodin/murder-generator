
This documentation file explains how the file [translations.json](../../web/translations.json) works.
This last file contains all the translation of the interface (graphical and text-based) of the project.
It is a [JSON](https://en.wikipedia.org/wiki/JSON), a usual file to store informations.
Its structure can be summarised as follows:
```json
[
  { "iso639": "eo",
    "name": "Esperanto",
    ...
    "key": "value"
  },
  ...
  { "iso639": "en",
    "name": "English",
    ...
    "key": "value"
  }
]
```
For each language, there is a new object in the global array, with keys and values, separated by commas.

The most important key is `iso639`, which corresponds to the language code.
This language code it is the usual [ISO 629](https://en.wikipedia.org/wiki/List_of_ISO_639-2_codes) code, two-letter if there is one, otherwise the three-letter one: “en” for English, “de” for German, etc.
This language code will be use thoroughly in the data files to identify the language, and thus has to be consistent.
In other words, although some languages have both a two-letter code and a three-letter code, these two codes will be treated as different by the program: it is important to choose the two-letter code if there is one, otherwise the three-letter code.
Regional language codes like `en-GB` are not recognised by the program.

Later on, a language object just consists in a sequence of key and values, the key being the identifier of the chain used by the program, and the value being the translation in the current language.
Please refer to the other languages to know how a key should be translated.
Another way is to just copy/paste a language object to start translating in another language, then checking the program interface in this language to see how the translation is used.

A special key is `name`: it represents the name of the language in the language itself.
So a translation object for German would for instance look like the following:
```json
  { "iso639": "de",
    "name": "Deutsch",
    ...
  }
```

Punctuation can be unusual in links (typically in keys like `there`, next`, etc.).
This is because I use special styles that add arrows when links are hovered (so `there` becomes `there →`).
This might not fit some languages well.
For instance, it might not be easy to always rephrase a given value so that the link is at the end of the sentence.
It might also be completely messed up in languages that write right-to-left.
If you want run into problems when translating a particular key, please [contact me](https://github.com/Mbodin).

After translating the interface for a new language, you may want to also translate the scenario elements: see [language.md](../explanations/language.md) for instructions.

