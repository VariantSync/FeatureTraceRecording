# Prototype of Feature Trace Recording

This code is supplementary material for the submission _Feature Trace Recording_ at _PUT VENUE HERE_.

## How to Run the Project
This prototype is written in Haskell and uses the Stack build system.
Instructions for installing stack can be found [here](https://docs.haskellstack.org/en/stable/README/).
On linux, `sudo apt-get install haskell-stack` (or `sudo pacman -S stack` if you are using pacman) should do the job.
After you installed stack, please open a terminal and navigate to the repositorie's directory (the directory containing this `README.md`).
Our prototype prints coloured text to the terminal, so it might not be compatible with every terminal.
We tested it within the Windows Terminal, Windows Powershell, Windows Subsystem for Linux (WSL) and the default terminal on Manjaro. (It does not work with Git Bash.)
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
    - Alice's part of the motivating example shown in Figure 2 in the paper.
    - Bob's part of the motivating example shown in Figure 3 in the paper. As the synchronisation of code and feature traces across clones is subject to future work, this example simulates how we envision the synchronisation.
2. Code Change Patterns from the Evaluation (Section 6): As there are sometimes several ways for reproducing a pattern (e.g., depending on the presence of an outer presence condition), some patterns are shown multiple times. We omitted AddIfdef* as it is just a repitition of AddIfdef with arbitrary contexts and code fragments. As AddIfdefElse has to be reproduced using two variants, we need two different examples here, one for the if-branch and one for the else-branch.
    - _AddIfdef_
    - _AddIfdefElse_IfBranch_
    - _AddIfdefElse_ElseBranch_
    - _AddIfdefWrapElse_
    - _AddIfdefWrapThen_
    - _AddNormalCode_nonvariational_ (Adding code that belongs to all clones.)
    - _AddNormalCode_outerpc_ (Adding code without any associated trace into a tree-optional scope that is already traced.)
    - _RemNormalCode_null_ (Removing code that does not have a presence condition)
    - _RemNormalCode_notnull_ (Removing code that has a feature trace and thereby a presence condition)
    - _RemIfdef_

## Interesting Code Locations

- `showExamples` function in [`app/Main.hs`](app/Main.hs): Here you can choose which examples to run and in which format the source code should be displayed. Choose from:
    - `userFormat` (default): The perspective of the developer who is editing source code while traces are recorded in the background. This is the format used in the figures in the paper. Feature traces are indicated by colours.
    - `userFormatDetailed`: A variation of `userFormat` where traces and presence conditions can be investigated seperately at the same time. Code is coloured in the colour of its feature trace while presence conditions are indicated by coloured lines on the left.
    - `astFormat`: Shows the abstract syntax tree of the source code with feature traces as formulas.
    - `tikzFormat`: Tikz export of abstract syntax trees with traces. Used for figures in the paper.

- [`src/feature/recording/FeatureTraceRecording.hs`](src/feature/recording/FeatureTraceRecording.hs): This file includes type definitions and interfaces for feature trace recording to make it configurable (e.g., plug in custom recording functions).

- [`src/feature/recording/DefaultFeatureTraceRecording.hs`](src/feature/recording/DefaultFeatureTraceRecording.hs): The implementation of the feature trace recording. Here you can find Algorithm 1 from the paper (`defaultFeatureTraceRecording`) and the recording functions for insertions, deletions, moves, and updates.

- [`src/feature/FeatureTrace.hs`](src/feature/FeatureTrace.hs): Here you can find definitions for feature traces and presence conditions.

- [`src/tree/grammars/SimpleCXX.hs`](src/tree/grammars/SimpleCXX.hs): Example implementation for a simplified C++ grammar used for the examples in our paper. Here you can see the different rules of the grammar as well as the classifiation of terminal symbols (node types) as _mandatory_, _optional_, or _tree-optional_.

- [`app/examples/`](app/examples/): In this directory, the source code for the examples can be found. The motivating example from the paper is implemented in [`StackPopAlice.hs`](app/examples/StackPopAlice.hs) and [`StackPopBob.hs`](app/examples/StackPopBob.hs). The reproduction of the code change patterns used in our evaluation can be found in [`CodeChangePatterns.hs`](app/examples/CodeChangePatterns.hs).

- [`src/propositions/NullPropositions.hs`](src/propositions/NullPropositions.hs): Operators for our ternary logic with `null`, based on our implementation of propositional logic in [`src/propositions/Propositions.hs`](src/propositions/Propositions.hs). You can inspect the truth tables for this logic by uncommenting the respective line (`showTruthtables`) in the `main` function in [`app/Main.hs`](app/Main.hs) and running the project again.
