
# Description

This project is a [murder party](https://en.wikipedia.org/wiki/Murder_mystery_game) generator.
More precisely, this project aims at generating live-action role-playing-game scenarios involving a lot of player-versus-player.
It does not necessarily generate a murder mystery: the scenario will not necessarily consist of an investigation.
The generated scenarios are not meant to be extraordinary in term of content, but they do aim at providing interesting interactions between players.

The generator is available online at [this address](https://mbodin.github.io/murder-generator/).

The generated scenarios can be freely used for any role-playing game you want, in any context: it can be a party amongst friends, an exercise in a theater or language course, part of a role-playing-game festival, etc.
The generated scenarios can also be used as a help to write custom scenarios.

Scenarios are made by combining scenario elements, defined in the [data/elements](./data/elements) folder.
These elements range from very simple ones (for instance by just stating that a given player is female) to relatively complex ones (like stating that a player is a double agent, or that a player is responsible for a contract between several other players).
Each of these elements yields some specific relations, and the generator tries to even them out along players according to the player’s preferences.

If you want to contribute, you are very welcomed to: more information can be found in [CONTRIBUTING.md](./CONTRIBUTING.md).
There are various ways to contribute, and not all of them include programming.

Program documentation can be found in [index.md](./doc/index.md).
In particular, documentation about its source code can be found in [structure.md](./doc/references/structure.md).
Documentation also includes material to write scenario elements, notably [elements.md](./doc/explanations/elements.md) for a detailed explanation, or [basic.md](./doc/tutorials/basic.md) for a simple tutorial.

The program was mainly written because writing murder parties take quite a long time.
I already have written some scenarios (which are available [here](https://github.com/Mbodin/murders)), and quickly realised how difficult it is to have a balanced scenario.
My main frustration was that there was always a “core scenario” whose characters I really like… but around this core scenario, there was always some “filling characters”, just there to confuse the tracks.
This program tries to avoid such “filling characters” and attempts to provide interesting relations between all players.
I hope that this leads to scenarios which are interesting to play.

# Installation

You do not need to compile the program to use it, as it is already available [online](https://mbodin.github.io/murder-generator/).
Compiling the program yourself however provides an additional native version of the program, which is sensibly faster (but not as pretty as the online interface).

To compile the program, you will need to install `npm` and `esy`.
On Debian, this would look like that:
```bash
sudo apt install npm
npm install --global esy
```

To compile this project, you will need to install some dependencies.
The following line will fetch and compile all dependencies.  This might take some time.
```bash
esy install
```
All commands cited below may take a much longer time the first time that they are run.

The [dummy](./dummy) folder contains OCaml files meant to be instantiated some actual usage of files in the repository.  The following command will update them.
This command should be called every time one updates the repository.
```bash
esy local
```

To compile the project into JavaScript and update the file [main.js](./web/main.js), do the following.
```bash
esy js
```

At this stage, you can open the file `index.html` on a browser and enjoy the generator.  Note that this requires to set up a server.
This README does not aim at explaining how to set it up; if you are using Github, you can push these changes online and access the corresponding [github.io](https://github.io) address.
If you have Python 2 installed, typing `esy server` will set up a local server: accessing `localhost:8000` should then do it.

The JavaScript webpage is (relatively) pretty, but its generation is significantly slower than a native version.
Such a native version can be built and run through the following commands.
```bash
esy native
```

Once the native program has been built, you can run it using `./main.native` or on some system `esy ./main.native`.
You get a terminal interface similar to the web interface.  The interface wait for a number input, corresponding to one of the displayed links.  For instance, if it is displayed `[3]`, then typing `3` then enter would activate this link.
One can also change the width of the display by typing `0` and entering the new width.  To exist, emit an end-of-file message to the terminal (which is usually done by simultaneously typing the control and `d` keys).

Optionally, one can perform some tests using the following command.
```bash
esy test
```

Note that the `esy local` and `esy js` commands above are updating committed files!  To revert them, type the following command.  Please do it before committing anything.
```bash
esy checkout # To be done before any commit.
```

# Licence

Copyright © 2019 Martin Constantino–Bodin

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

The program is under the GNU Affero General Public License version 3 (AGPLv3).
See the file [LICENSE](./LICENSE) for more information.
This license covers its entire source code (which can be found in the [src](./src) folder), its local libraries (which can be found in the [lib](./lib) folder), the web resources (which can be found in the [web](./web) folder), as well as the game data (which can be found in the [data](./data) folder).

Note that the generated scenarios (and more generally, any material generated by this program) are not associated any license: you are free to use, adapt, and more importantly play with all the products of this program without any constraint.

