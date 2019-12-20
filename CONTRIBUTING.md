
Thank you for your interest about this program!

There are various ways that you can contribute if you want to.
Here is a non-exhaustive list.

# Using the Program

The simplest way to help with this program is actually just to use it.
Run the program, either [online](https://mbodin.github.io/murder-generator/) or through the native version (see the installation section of the [README](./README.md) file for more information).

If you notice any issue when running the program (error messages, bugs, invalid generated file, etc.), please [report it](https://github.com/Mbodin/murder-generator/issues).
If applicable, include the JSON generated file and your browser name and version (or operating system if running the native version): this can really help.
Although Github is only available in English, feel free to report issues in the language you used the program.

If you notice typos, grammatical mistakes, or very strange generated situations that you believe should not be generated (like characters unaware that another character is one of their parents, without any justification for this), report them.
Consider sending the generated JSON file with the scenario as well as describing which parameters you used for the generation (for instance which themes you used, etc.), as this really helps debugging.

# Playing with the Generated Scenarios

In addition to just using the program, why not actually using the generated scenarios?
These scenarios are indeed meant to be played!
If you have any feedback, do not hesitate to [contact me](https://github.com/Mbodin).
I can’t promise a quick answer, but feedbacks are always greatly appreciated!
If you have comments about a specific generated scenario, please include the JSON file (also, optionally, pictures of you playing it!), as it really helps me to understand how I can improve the program.

Here are the things that I would be interested to know if you use the generated scenario with other players:
- How did you use the program?  Did you generate a lot of scenarios and picked the one you preferred?  Or did you just generated one on the go?
- Did you have to rewrite parts of the generated scenario to be playable or interesting?
- Which extracted format was the most useful for you?  Or did you just print the final webpage?

Also, I would be very glad if you were to use this program in a different context than traditional role-playing games.
In particular, if you used it for education purposes (for instance in a language or theater course), I would be very interested in knowing whether I could improve the program for your particular use case.

# Writing Scenario Elements

What this program does is just combining scenario elements together to create one big scenario.
These scenarios elements are defined in a specific `.murder` format and are stored in the [data/elements](./data/elements) folder.
If you would like to participate in the writing, [the documentation](./doc/index.md) explains how to write such elements, in particular [elements.md](./doc/explanations/elements.md) for a detailed explanation, or [basic.md](./doc/tutorials/basic.md) for a simple tutorial.
Do not hesitate to send pull requests.

Note that the elements in the [data/elements](./data/elements) folder are subject, as for the whole program, to the [Affero General Public License version 3 (AGPLv3)](./LICENSE): if you do not want your work to fall into this license, please refrain yourself from sending me a pull request.
If you would like to write elements in another license, you can [contact me](https://github.com/Mbodin) to see how we can link all these elements together.

# Enhancing Scenario Elements

It is easy when writing scenario elements to forget that a situation can happen.
For instance, [job.murder](./data/elements/job.murder) contains the following lines:
```murder
element SellerIsEasilyKnown
begin
  let S be player with attribute Job as Seller
  let P be player with attribute Job not as Seller
                  with contact FamilyRelation to S as None
```
This is the beginning of an element stating that sellers and clients know each others.
The first version of these lines looked like this:
```murder
element SellerIsEasilyKnown
begin
  let S be player with attribute Job as Seller
  let P be player
```
There were no constraints on the player `P`!
This means that `S` and `P` could have been from the same family, which would have been weird: if these two characters are from the same family, they probably know each others before `S` even became a seller.

Such constraints are easy to forget when writing scenario elements.
Most of the strange generated scenarios can be solved by adding constraints on elements like the ones above.
If you don’t want to write elements by yourself but would be willing to reread the already-written elements, any pull request is welcomed.
As for writing elements, [elements.md](./doc/explanations/elements.md) can be a good read (in particular the “advanced” features) to enhance scenario elements.

# Translating

Currently this program is available in three languages: English, French, and Esperanto.
If you would like to translate it into another language, you are very welcomed to!
This program does not use any translation framework, unfortunately.
The reason is that grammar plays quite an important role in the generated sentences, which makes usual translation framework less useful.

There are two different things that need to be translated: the interface and the scenario elements.
The interface’s translations are stored in [translations.json](./web/translations.json), whose content is explained in [translations.md](./doc/references/translations.md).
The scenario elements are present in the [data/elements](./data/elements) folder.
See [language.md](./doc/explanations/language.md) for how the language part of these files works and on how to add a language there.

Note that only the interface has to be fully translated to be able to use the program.
However, if a scenario element is left untranslated, some sentences may appear as a `Missing translation` message in the output scenario, or simply never be picked by the generator.
As for the other means of contributing, pull requests are of course very welcomed.

If you would be interested in translating the documentation (or helping with the documentation), [contact me](https://github.com/Mbodin).

# Adding Features

The means of contributing above were about using the program and updating its data.
If you prefer to contribute to its actual source code, you are also very welcomed.
This program is written in OCaml and its source code can be found in the [src](./src) folder (as well as in [lib](./lib) for source code that could be easily reused elsewhere).

A good way to contribute is to add an extraction format in the [export.ml](./src/export.ml) file: if you consider that a format which is not currently exported to (or whose current extraction is not useful enough for you) is relevant, write an extractor!
For instance, there is currently no extractor to LaTeX or Patoline: if you have a good idea on what a generated TeX/Patoline file should look like, do try to write an extractor to these.

Pull requests for other features are also welcomed.
Examples of features that could be added to the program:
- instead of just writing repeatitive sentences, maybe one could reuse [some recent advances in machine learning](https://openai.com/blog/better-language-models/) (see [this website](https://talktotransformer.com/) or [this one](https://colab.research.google.com/github/nickwalton/AIDungeon/blob/master/AIDungeon_2.ipynb) for interactive versions) to generate ever changing-scenarios.
- actually generate proper character sheets in a relevant format.
- currently, `.murder` files are parsed client-side, which means that the client has to include a full parser for these files.  Maybe it would be worth trying to preparse these files and send a small `.json` file instead, removing the need to send the parser to the user and thus preserving bandwidth.
- there are currently no program to help people that would like to write new scenario elements: maybe an editor or editor-features would be interesting.
- on a similar idea, a program to help non-specialists translating the program into other languages would be a very good asset.

