# Feature Trace Recording

[![Language](https://img.shields.io/badge/Language-Haskell-purple)](https://www.haskell.org/)
[![Documentation](https://img.shields.io/badge/Documentation-Read-purple)][documentation]
[![Preprint](https://img.shields.io/badge/Preprint-Read-purple)][preprint]
[![Build Status](https://travis-ci.com/pmbittner/FeatureTraceRecording.svg?branch=esecfse21)](https://travis-ci.com/pmbittner/FeatureTraceRecording)
[![License](https://img.shields.io/badge/License-GNU%20LGPLv3-blue)](LICENSE.LGPL3)
[![Install](https://img.shields.io/badge/Install-Instructions-blue)](INSTALL.md)
[![DOI](https://zenodo.org/badge/286511539.svg)](https://zenodo.org/badge/latestdoi/286511539)
<!-- [![Status](https://img.shields.io/badge/ESEC%2FFSE'21-Badge%20Application-blue)](STATUS.md) -->

[<img src="https://www.acm.org/binaries/content/gallery/acm/publications/artifact-review-v1_1-badges/artifacts_evaluated_reusable_v1_1.png" alt="ACM Artifacts Evaluated Reusable" width="114" height="113" />][paper]

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


## Please cite as
```tex
@inproceedings{BST+:ESECFSE21,
	author = {Paul Maximilian Bittner and Alexander Schulthei\ss{} and Thomas Th{\"{u}}m and Timo Kehrer and Jeffrey M. Young and Lukas Linsbauer},
	title = {{Feature Trace Recording}},
	booktitle = {Proc.\ Europ.\ Software Engineering Conf./Foundations of Software Engineering (ESEC/FSE)},
	location = {Athens, Greece},
	publisher = {ACM},
	address = {New York, NY, USA},
	year = 2021,
	month = AUG,
	note = {To appear}
}
```


## Limitations

So far, this library contains no implementation for parsing and diffing source code.
Instead, users of this library have to provide [Abstract Syntax Trees (ASTs)](https://pmbittner.github.io/FeatureTraceRecording/AST.html) and [edits to them](https://pmbittner.github.io/FeatureTraceRecording/Edits.html) as input for feature trace recording (see `runFTR` and `runFTRWithIntermediateSteps` in [FeatureTraceRecording.hs](https://pmbittner.github.io/FeatureTraceRecording/FeatureTraceRecording.html)).
Examples for creating edits and ASTs can be found in the demos for Alice ([docs](https://pmbittner.github.io/FeatureTraceRecording/StackPopAlice.html), [src](app/examples/StackPopAlice.hs)), Bob ([docs](https://pmbittner.github.io/FeatureTraceRecording/StackPopBob.html), [src](app/examples/StackPopBob.hs)), and edit patterns ([docs](https://pmbittner.github.io/FeatureTraceRecording/EditPatterns.html), [src](app/examples/EditPatterns.hs)).

Currently, the library also does not provide (de-)serialisation of feature traces.


## Contact

Don't hesitate to open issues or pull-request or to contact us directly (paul.bittner@uni-ulm.de). We are thankful for any help, constructive criticism, or interest. :)


[paul]: https://www.uni-ulm.de/in/sp/team/paul-maximilian-bittner/
[alexander]: https://www.informatik.hu-berlin.de/de/forschung/gebiete/mse/mitarb/alexander-schultheiss.html
[thomas]: https://www.uni-ulm.de/in/sp/team/thuem/
[timo]: https://www.informatik.hu-berlin.de/de/forschung/gebiete/mse/mitarb/kehrerti.html
[jeffrey]: https://www.uni-ulm.de/in/sp/team/former-employees-and-doctorands/jeffrey-young/
[lukas]: https://www.tu-braunschweig.de/isf/team/lukas-linsbauer

[documentation]: https://pmbittner.github.io/FeatureTraceRecording/
[preprint]: https://github.com/SoftVarE-Group/Papers/raw/master/2021/2021-ESECFSE-Bittner.pdf
[paper]: https://doi.org/10.1145/3468264.3468531