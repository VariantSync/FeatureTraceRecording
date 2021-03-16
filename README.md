# Prototype of Feature Trace Recording

This code is supplementary material for the submission _Feature Trace Recording_ at _ESEC/FSE 2021_.

## How to Run the Project
This prototype is written in Haskell and uses the _Stack_ build system.
Instructions for installing stack can be found [here](https://docs.haskellstack.org/en/stable/README/).
On linux, `sudo apt-get install haskell-stack` (or `sudo pacman -S stack` if you are using pacman) should do the job.
After you installed stack, please open a terminal and navigate to the repository's directory (the directory containing this `README.md`).
Our prototype prints coloured text to the terminal, so it might not be compatible with every terminal.
We tested it within the Windows Terminal, Windows Powershell, Windows Subsystem for Linux (WSL) and the default terminal on Manjaro. (It might not work with Git Bash.)
You can then run the prototype as follows:

    cd <path/to/this/repository>
    stack run

## What Is There to See
Our prototype runs several independent examples.
Each example is separated by a red headline and consists of a series of edits applied to a source code fragment.
For each edit, we first show its type and the feature context under which that edit was made:

    ==== Run ins_tree(11, 4, 0) under context = "SafeStack" giving us ====

where an abstract syntax tree (AST) whose root has ID 11 is inserted below node 4 in the current AST at index 0 under feature context "SafeStack".
(The first edit will always be `identity` under context `null`. This is a technical detail necessary to show the initial state of the example.)
Afterwards, the code that is a result of this edit is shown (similar to Figure 2 in the paper):

    void pop() {
        if (!empty()) {
        }
        storage[head--] = null;
    }

By default, the following examples are executed in this order:

1. Motivating Example
    - Alice's part of the motivating example shown in Figure 1 in the paper.
    - Bob's part of the motivating example shown in Figure 3 in the paper. As the synchronisation of code and feature traces across clones is subject to future work, this example simulates how we envision the synchronisation.
2. Edit Patterns from the Evaluation (Section 5): For each pattern we show how to reproduce it in the general case and when an outer scope (eg., a method) is already assigned the target mapping. We omitted AddIfdef* as it is just a repitition of AddIfdef with arbitrary contexts and code fragments. As AddIfdefElse has to be reproduced using two variants, we need two different examples here, one for the if-branch and one for the else-branch.

## If You Cannot Get it Running ...
..., you can find screenshots of the prototype's output in the docs folder:

- [docs/Alice.png](docs/Alice.png): Shows the output for Figure 1 in the paper. Alice performs several edits to the `pop` method.
- [docs/Bob.png](docs/Bob.png): Shows how synchronising Alice's changes to Bob's variant would look like. As the synchronisation of code and feature traces across clones is subject to future work, this example simulates how we envision the synchronisation.
- [docs/Patterns.png](docs/Patterns.png): Shows how our tool can reproduce the edit patterns described in the paper.

## Interesting Code Locations

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
We provide truthtables for the logic in [docs/Truthtable.md](docs/Truthtable.md).
The implementation is based on our implementation for propositional logic in [`src/propositions/Propositions.hs`](src/propositions/Propositions.hs).
You can inspect the truth tables for the ternary logic by uncommenting the respective line (`showTruthtables`) in the `main` function in [`app/Main.hs`](app/Main.hs) and running the project again.
