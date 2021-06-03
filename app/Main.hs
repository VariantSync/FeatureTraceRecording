{- |
Description: Module of the @main@ function to run demos and print meta-information.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Module of the @main@ function to run demos and print meta-information.
-}
module Main where

import Control.Monad.State ( State )

import UUID ( UUID )
import Util (removeQuotes,  genIndent )
import Tree ( prettyPrint )
import AST
import Grammar
import ASTPrettyPrinter
import Edits ( edit_identity )
import Logic
import Propositions
import NullPropositions 
import FeatureTrace 
import FeatureTraceRecording
import FeatureColour (FeatureFormulaColourPalette)
import Example
import TikzExport ( astToTikzWithTraceDefault )

import StackPopAlice (example)
import StackPopBob (example)
import EditPatterns

import Data.List (intercalate)

-- imports for Terminal printing ---------
import Data.Text.Prettyprint.Doc
    ( Doc, (<+>), annotate, hardline, Pretty(pretty) )
import System.Terminal
import Truthtable (generatetruthtablesfor)

-- | Style defining how to print 'AST's.
data CodePrintStyle
    = ShowAST  -- ^ Prints the 'AST' by showing each 'Node' in the hierarchy in an XML-like format.
    | ShowCode -- ^ Prints the 'AST' as the actual source code that it represents.
    | ShowTikz -- ^ Print the 'AST' as tikz code to use for our paper.
    deriving (Show)
-- | Format defining whether to 'FeatureTrace's or presence conditions ('pc').
data TraceDisplay
    = Trace -- ^ Show the feature mapping of each node (i.e., the formula a node is directly annotated with).
    | PC    -- ^ Show the presence condition of each node (i.e., the conjunction of its feature mapping with all feature mappings inherited from ancestors).
    deriving (Show, Eq)
-- | Style defining how to print 'FeatureTrace's.
data TraceStyle
    = Text   -- ^ Show feature mapping formulas as plain text.
    | Colour -- ^ Encode features as colours to visualize feature mappings by colouring source code.
    | None   -- ^ Do not show feature traces at all.
    deriving (Show, Eq)

{- |
Format in which code and (recorded) feature mappings should be printed to the terminal.
-}
data OutputFormat = OutputFormat {
    codeStyle :: CodePrintStyle,
    traceDisplay :: TraceDisplay,
    traceStyle :: TraceStyle,
    withTraceLines :: Bool -- ^ Whether there should be vertical lines next to the shown code on the left that indicate presence condtions.
}

-- Some presets for output formats:

{- |
The perspective of the developer, who is editing code while traces are recorded in the background
This is the format used in the figures in the paper.
-}
userFormat :: OutputFormat
userFormat = OutputFormat {
    codeStyle = ShowCode,
    traceDisplay = PC,
    traceStyle = Colour,
    withTraceLines = False
}

{- |
A variation of 'userFormat' where traces and presence conditions can be investigated seperately at the same time.
Code is coloured in the colour of its trace while presence conditions are indicated by coloured lines on the left.
-}
userFormatDetailed :: OutputFormat
userFormatDetailed = OutputFormat {
    codeStyle = ShowCode,
    traceDisplay = Trace,
    traceStyle = Colour,
    withTraceLines = True
}

{- |
Shows the 'AST' of the source code with 'FeatureTrace's as formulas.
-}
astFormat :: OutputFormat
astFormat = OutputFormat {
    codeStyle = ShowAST,
    traceDisplay = Trace,
    traceStyle = Text,
    withTraceLines = False
}

{- |
Tikz export of 'AST' with 'FeatureTrace's.
Used for figures in the paper.
-}
tikzFormat :: OutputFormat
tikzFormat = OutputFormat {
    codeStyle = ShowTikz,
    traceDisplay = Trace,
    traceStyle = None,
    withTraceLines = False
}
        
{- |
Entry point of the demo.
The main method will run and print the output of all examples (Alice, Bob, and the edit patterns).
You can change the format of the printed source code and feature mappings by changing the @format@ parameter inside 'main'.
Additionally, you might want to look at the truthtable of the ternary logic by Sobocinski we use (by uncommenting the line @showTruthtables@).
-}
main :: IO ()
main =
    {-
    Select your OutputFormat here.
    Above, there is a list of presets you can choose from.
    -}
    let format = userFormat in
    do
        showExamples format
        -- showTruthtables

{- |
Runs the motivating example from the paper and examples for all edit patterns with the given 'OutputFormat'.
First, runs Alice's example where she records feature traces upon editing the pop method of a class Stack in Java (Figure 1 in the paper).
Second, shows how Bob could propagate Alice's edits and recorded feature traces to his variant as envisioned in future research.
Third, shows an instance of each edit pattern from our evaluation.
-}
showExamples :: OutputFormat -> IO ()
showExamples format = withTerminal $ runTerminalT $
    let run = runStepwise format in
    do
        putDoc hardline
        headline "Running Feature Trace Recording Demo"
        
        headline ">>> [Motivating Example] <<<"
        run StackPopAlice.example
        run StackPopBob.example
        
        headline ">>> [Edit Patterns] <<<"
        run EditPatterns.addIfdef
        run EditPatterns.addIfdefWithPC
        -- We omitted AddIfdef* as it is just a repitition of the previous pattern with arbitrary contexts and code fragments.
        -- AddIfDefElse has to be reproduced using two variants.
        -- Hence, we need two different examples here, one for the if-branch and one for the else-branch.
        run EditPatterns.addIfdefElse_IfBranch
        run EditPatterns.addIfdefElse_ElseBranch
        run EditPatterns.addIfdefElse_IfBranchWithPC
        run EditPatterns.addIfdefElse_ElseBranchWithPC
        run EditPatterns.addIfdefWrapElse
        run EditPatterns.addIfdefWrapThen
        -- Adding non-variational code (code that belongs to all clones)
        run EditPatterns.addNormalCode_nonvariational
        -- Adding code without any associated trace into a tree-optional scope that is already traced.
        run EditPatterns.addNormalCode_outerpc
        -- Removing code that does not have a presence condition
        run EditPatterns.remNormalCode_null
        -- Removing code that has a feature trace and thereby a presence condition
        run EditPatterns.remNormalCode_notnull
        -- Removing code that has a feature trace
        run EditPatterns.remIfdef
    

-- | Turns the given text into a headline in the terminal.
-- We indicate headlines with a red background.
headline :: (MonadColorPrinter m) => String -> m()
headline text = putDoc $ hardline <+> (annotate (background red) $ pretty text) <+> hardline <+> hardline

-- | Runs the given 'Example' in the given 'OutputFormat' step by step (i.e., showing all intermediate results).
runStepwise :: (MonadColorPrinter m, Grammar g, ASTPrettyPrinter g) => OutputFormat -> State UUID (Example m g String) -> m ()
runStepwise format ex =
    let example = finalizeExample ex
        result = printTraces format example (Example.runExampleWithDefaultFTR example) in
    (putDoc $ result <+> hardline) >> flush

-- | Prints the given 'AST' with the given 'FeatureTrace's in the given 'OutputFormat'.
-- If the 'OutputFormat' mandates to visualize feature mappings as colours (see 'TraceStyle'),
-- the given 'FeatureFormulaColourPalette' will be used to assign colours to feature formulas.
printASTWithTrace :: (MonadColorPrinter m, Grammar g, ASTPrettyPrinter g, Show a, Eq a) =>
    OutputFormat -> FeatureFormulaColourPalette m -> AST g a -> FeatureTrace g a -> Doc (Attribute m)
printASTWithTrace format featureColourPalette tree trace = 
    let 
        codestyle = codeStyle format
        tracestyle = traceStyle format
        tracedisplay = traceDisplay format
        withtracelines = withTraceLines format
        nodePrint trace n = case tracestyle of
                        None -> pretty.removeQuotes.show $ value n
                        Colour -> paint (trace n) $ removeQuotes.show $ value n
                        Text -> pretty $ concat ["<", NullPropositions.prettyPrint $ trace n, ">", removeQuotes.show $ value n]
        stringPrint trace n s = case tracestyle of
                        Colour -> paint (trace n) s
                        _ -> pretty s
        indentGenerator trace n i = if tracestyle == Colour && tracedisplay == Trace && withtracelines && optionaltype n == Optional
                        then mappend (paint (trace n) "|") (pretty $ genIndent (i-1))
                        else pretty $ genIndent i
        paint formula = (annotate (foreground $ featureColourPalette formula)).pretty
        in
        case codestyle of
            ShowAST -> (case tracestyle of
                None -> pretty.show
                Colour -> Tree.prettyPrint 0 pretty (\n -> paint (trace n) $ show n)
                Text -> pretty.(FeatureTrace.prettyPrint trace)) tree
            ShowTikz -> pretty $ astToTikzWithTraceDefault (trace, tree)
            ShowCode -> showCodeAs mempty (indentGenerator trace) (stringPrint trace) (nodePrint trace) tree

-- | Prints the given list of versions that were produced from the given example.
printTraces :: (MonadColorPrinter m, Grammar g, ASTPrettyPrinter g, Show a, Eq a) =>
    OutputFormat -> Example m g a -> [Version g a] -> Doc (Attribute m)
printTraces format example tracesAndTrees = 
    let
        featureColourPalette = colours example
        tracedisplay = traceDisplay format
        toPC = \trace tree -> if tracedisplay == PC then pc tree trace else trace
        in
        mappend (mappend (pretty "\n") $ annotate (background red) $ pretty $ intercalate "\n  " [
            "Running "++name example
            -- "codeStyle      = "++show codestyle,
            -- "traceDisplay   = "++show tracedisplay,
            -- "traceStyle     = "++show tracestyle
            ])
        $ flip foldr
            mempty
            (\((edit, fc), (trace, tree)) s ->
                mconcat [
                    hardline,
                    hardline,
                    pretty $ concat ["==== Run ", show edit, " under context = "],
                    annotate (foreground $ featureColourPalette fc) $ pretty $ NullPropositions.prettyPrint fc,
                    pretty $ " giving us ====",
                    hardline,
                    printASTWithTrace format featureColourPalette tree (toPC trace tree),
                    s])
        $ zip
        -- We have to do this as the first entry in tracesAndTrees will be the initial state of the program
        (alsoShowInitialStateInHistory (history example))
        tracesAndTrees

-- | Helper function to show the initial state of the given history in 'printTraces'.
-- Prepends an identity edit and dummy 'FeatureContext'.
-- The context could be anything and thus is set to 'Nothing' (/null/).
alsoShowInitialStateInHistory :: History g a -> History g a
alsoShowInitialStateInHistory h = (edit_identity, Nothing):h

-- | Helper function to help with type inference.
-- Returns all atomic values of a 'PropositionalFormula' (over strings).
propositional_values :: [PropositionalFormula String]
propositional_values = lvalues

-- | Helper function to help with type inference.
-- Returns all atomic values of a 'NullableFormula' (over strings).
nullableFormula_values :: [NullableFormula String]
nullableFormula_values = lvalues

-- | Prints truthtables for common operators in 'PropositionalFormula's and 'NullableFormula's (not, and, or, implies, equiv)
showTruthtables :: IO()
showTruthtables = withTerminal $ runTerminalT $
    do
        headline "Propositional Logic"
        putDoc.pretty $ generatetruthtablesfor propositional_values
        putDoc $ hardline <+> hardline
        headline "Ternary Logic With Null"
        putDoc.pretty $ generatetruthtablesfor nullableFormula_values