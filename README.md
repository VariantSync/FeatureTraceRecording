# Feature Trace Recording

[![Language](https://img.shields.io/badge/Language-Haskell-purple)](https://www.haskell.org/)
[![Documentation](https://img.shields.io/badge/Documentation-Read-purple)][documentation]
[![Build Status](https://travis-ci.com/pmbittner/FeatureTraceRecording.svg?branch=master)](https://travis-ci.com/pmbittner/FeatureTraceRecording)
[![License](https://img.shields.io/badge/License-GNU%20LGPLv3-blue)](LICENSE.LGPL3)
[![Install](https://img.shields.io/badge/Install-Instructions-blue)](INSTALL.md)
[![Status](https://img.shields.io/badge/ESEC%2FFSE'21-Badge%20Application-blue)](STATUS.md)

Artifact repository for the paper _Feature Trace Recording_, accepted at _ESEC/FSE 2021_.
Authors are [Paul Maximilian Bittner][paul], [Alexander Schultheiß][alexander], [Thomas Thüm][thomas], [Timo Kehrer][timo], [Jeffrey M. Young][jeffrey], and [Lukas Linsbauer][lukas].

The artefact mainly consists of a library written in the Haskell language that implements feature trace recording.
The library is accompanied with a demo application that uses the library to reproduce our motivating example (Alice and Bob using feature trace recording in Section 2 in our paper) as well as examples of the edit patterns we used to evaluate feature trace recording (Section 5).


## How to Run the Demo
Our library is written in Haskell and uses the _Stack_ build system (see [REQUIREMENTS.md](REQUIREMENTS.md)).
Instructions for installing Stack, building our library and running the demo are given in the [INSTALL.md](INSTALL.md).


## Documentation

A detailed documentation can be found in `docs/index.html` and can be browsed on the [Github page][documentation].

Some interesting code locations are:
- `showExamples` function in [`app/Main.hs`](app/Main.hs): Here you can choose which examples to run and in which format the source code should be displayed. Choose from:
    - `userFormat` (default): The perspective of the developer who is editing source code while traces are recorded in the background. This is the format used in the figures in the paper. The tool will show the presence conditions of the snapshots.
    - `userFormatDetailed`: A variation of `userFormat` where traces and presence conditions can be investigated seperately at the same time. Code is coloured in the colour of its feature trace while presence conditions are indicated by coloured lines on the left.
    - `astFormat`: Shows the abstract syntax tree of the source code with feature traces as formulas.
    - `tikzFormat`: Tikz export of abstract syntax trees with traces. Used for figures in the paper.

- [`src/feature/recording/FeatureTraceRecording.hs`](src/feature/recording/FeatureTraceRecording.hs): This file includes type definitions and interfaces for feature trace recording to make it configurable (e.g., plug in custom recording functions).

- [`src/feature/recording/DefaultFeatureTraceRecording.hs`](src/feature/recording/DefaultFeatureTraceRecording.hs): The implementation of feature trace recording. Here you can find Algorithm 1 from the paper (`defaultFeatureTraceRecording`) and the recording functions for insertions, deletions, moves, and updates.

- [`src/feature/FeatureTrace.hs`](src/feature/FeatureTrace.hs): Here you can find definitions for feature traces and presence conditions.

- [`src/tree/grammars/SimpleJava.hs`](src/tree/grammars/SimpleJava.hs): Example implementation for a simplified Java grammar used for the examples in our paper. Here you can see the different rules of the grammar as well as the classifiation of terminal symbols (node types) as _mandatory_, _optional_, or _wrappers_.

- [`app/examples/`](app/examples/): In this directory, the source code for the examples can be found. The motivating example from the paper is implemented in [`StackPopAlice.hs`](app/examples/StackPopAlice.hs) and [`StackPopBob.hs`](app/examples/StackPopBob.hs). The reproduction of the edit patterns used in our evaluation can be found in [`EditPatterns.hs`](app/examples/EditPatterns.hs).

- [`src/propositions/NullPropositions.hs`](src/propositions/NullPropositions.hs): Operators for the ternary logic with `null`.
We provide truthtables for the logic in [meta/Truthtable.md](meta/Truthtable.md).
The implementation is based on our implementation for propositional logic in [`src/propositions/Propositions.hs`](src/propositions/Propositions.hs).
You can inspect the truth tables for the ternary logic by uncommenting the respective line (`showTruthtables`) in the `main` function in [`app/Main.hs`](app/Main.hs) and running the project again.


[paul]: https://www.uni-ulm.de/in/sp/team/paul-maximilian-bittner/
[alexander]: https://www.informatik.hu-berlin.de/de/forschung/gebiete/mse/mitarb/alexander-schultheiss.html
[thomas]: https://www.uni-ulm.de/in/sp/team/thuem/
[timo]: https://www.informatik.hu-berlin.de/de/forschung/gebiete/mse/mitarb/kehrerti.html
[jeffrey]: https://www.uni-ulm.de/in/sp/team/former-employees-and-doctorands/jeffrey-young/
[lukas]: https://www.tu-braunschweig.de/isf/team/lukas-linsbauer

[documentation]: https://pmbittner.github.io/FeatureTraceRecording/