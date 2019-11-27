
This file describes how the source code of this project is organised.
This program is written in OCaml.
Signature files `.mli` are commented, but `.ml` files only documents what is not present in the corresponding `.mli` file.

The project is divided into several folders:
- [src/](../../src) contains all the source code of the generator specific to this project.
- [lib/](../../lib) contains all the source code of the generator that is not specific to this project, and could thus be reused in other projects.
- [data/](../../data) contains all the data used by the program, in particular the scenario elements in its subfolder [elements](../../data/elements).  The generator uses these scenario elements to combine them and create a fully-fledged scenario.  In particular, [language.murder](../../data/elements/language.murder) describes how languages features are implemented, and is described in [language.md](./language.md).
- [dummy](../../dummy) contains source files that are meant to be updated at each compilation.  These files are created by the [build.sh](../../build.sh) script.  These files are not meant to be manually changed or committed.
- [web](../../web) contains resources for the online interface.

Here follows a description of the most important files of the [src/](../../src) folder.

We first describe the chain of files that reads the data in [data/](../../data) to generate the outputs.
- [lexer.mll](../../src/lexer.mll) is the file reading the data and producing a sequence of lexemes for the parser.
- [parser.mly](../../src/parser.mly) reads the lexemes given by [lexer.mll](../../src/lexer.mll) and converts them into the structure described in [ast.mli](../../src/ast.mli).
- [driver.mli](../../src/driver.mli) and [driver.ml](../../src/driver.ml) convert the structure of [ast.mli](../../src/ast.mli) into the internal representation of elements described in [element.mli](../../src/element.mli).  See [elements.md])(./elements.md) for a description of these elements as in [data/](../../data).
- [solver.mli](../../src/solver.mli) and [solver.ml](../../src/solver.ml) use these internal structures to build a scenario.
- Finally, [export.mli](../../src/export.mli) and [export.ml](../../src/export.ml) convert such scenarios into different formats.

The file [main.ml](../../src/main.ml) glues all these files together.
It is the main file actually being executed in the user’s browser or in the user’s terminal.
Alternatively, the file [tests.ml](../../src/tests.ml) contains various tests for the program.

The files defining structures are the following:
- [state.mli](../../src/state.mli) and [state.ml](../../src/state.ml) describe the state manipulated by [solver.ml](../../src/solver.ml).  In particular, it contains everything about character attributes and contacts, as well as their associated difficulty and complexity.  It also carries the events of the current state, but does not define this structure.  See [memory.md](./memory.md) for more information.
- [history.ml](../../src/history.ml) and [history.mli](../../src/history.mli) define the graph of events used in [state.ml](../../src/state.ml).  It however does not define the events themselves, relying on [events.mli](../../src/events.mli).  See the event section of [memory.md](./memory.md) for more information.
- [events.ml](../../src/events.ml) and [events.mli](../../src/events.mli) define the events used in [history.ml](../../src/history.ml), in particular their kind and constraints.
- [element.mli](../../src/element.mli) and [element.ml](../../src/element.ml) define the elements and their interaction with the state.  See [elements.md](./elements.md) for more information.
- [ast.mli](../../src/ast.mli) describes a temporary structure created by [parser.mly](../../src/parser.mly) and interpreted by [driver.ml](../../src/driver.ml).

In addition to these main files, additional files are used for more specific usages:
- [attribute.mli](../../src/attribute.mli) and [attribute.ml](../../src/attribute.ml) describe how attributes and contacts are manipulated by the state.  As stated in [memory.md](./memory.md), attributes and contacts are very similar, and are thus treated in a single module.
- [relation.mli](../../src/relation.mli) describes how character relations are defined and manipulated.  See the section about difficulty and complexity of [memory.md](./memory.md) for more information.
- [pool.mli](../../src/pool.mli) and [pool.ml](../../src/pool.ml) provide a structure for [solver.ml](../../src/solver.ml) to easily manipulate scenario elements.  In particular this structure stores the elements that still have a chance to be applied.
- [translation.mli](../../src/translation.mli) and [translation.ml](../../src/translation.ml) describe how internal elements are translated into human-readable sentences.  See [translations.md](./translations.md) for more information.
- [names.mli](../../src/names.mli) and [names.ml](../../src/names.ml) describe how character names are generated in the main interface, but is no longer used during the generation.

