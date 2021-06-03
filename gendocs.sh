# This shell scripts generates the documentation website for this library.
# It runs haddock (the haskell documentation generation tool) on all relevant files.

# find all files for which we want go generate documentation (demo in app and library in src)
# xargs appends that for us to the stack command
find app src -name "*.hs" | xargs stack exec -- haddock --package-name=ftr --html --quickjump --hoogle --hyperlinked-source --odir=docs
# Generate latex documentation (buggy though :( )
# find app src -name "*.hs" | xargs stack exec -- haddock --package-name=ftr --latex --quickjump --hoogle --hyperlinked-source --odir=docs/latex

# convert html documentation to pdf (generates only the frontpage though)
# pandoc docs/html/index.html -o docs/pdf/FeatureTraceRecording.pdf