## Installing Stack

As explained in the [REQUIREMENTS.md](REQUIREMENTS.md), the [_Stack_][stack] build system is our only installation requirement.
Detailed installation instruction are given on the respective [installation webpage][stackinstall].

- Linux: You can install stack via `sudo apt-get install haskell-stack` (or `sudo pacman -S stack` if you are using pacman).
Further instructions for installing stack including specific linux distributions are given [here][stackinstall].
- Windows 64-bit: Go to the [stack installation page][stackinstall]. Download and run the _Windows 64-bit Installer_.
- MacOS: Please follow the instructions on the [installation webpage][stackinstall].

## Running the Demo

After you installed stack, please open a terminal and navigate to the repository's directory (the directory containing this `INSTALL.md`).
```shell
cd <path/to/this/repository>
```
Our demo prints coloured text to the terminal, so it might not be compatible with every terminal.
We tested it within the Windows Terminal, Windows Powershell, Windows Subsystem for Linux (WSL), and the default terminal on Manjaro.
(It might not work with Git Bash.)
You can then build the library and run the demo as follows:

```shell
stack run
```

## Expected Output

The demo will run three examples as described in the [README](README.md):

1. Alice's part of the motivating example shown in Figure 1 in the paper.
2. Bob's part of the motivating example shown in Figure 3 in the paper.
3. Examples of Edit Patterns from the Evaluation (Section 5).

If you see the following output after `stack run`, the build process and execution of the demo were successful.
The colours in your terminal might deviate from the following screenshots:

![Alice](meta/Alice.png)
![Bob](meta/Bob.png)
![Patterns](meta/Patterns.png)

[stack]: https://docs.haskellstack.org/en/stable/README/
[stackinstall]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
