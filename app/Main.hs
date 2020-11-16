module Main where

import Control.Monad.State ( State, runState )

import UUID ( UUID )
import Util (removeQuotes,  genIndent )
import Tree ( prettyPrint )
import AST
import Edits ( edit_identity )
import Logic
import Propositions
import NullPropositions 
import FeatureTrace (FeatureTrace,  augmentWithTrace, pc, prettyPrint )
import FeatureTraceRecording

import Example

import TikzExport ( astToTikzWithTraceDefault )

import StackPopAlice (example)
import StackPopBob (example)
import CodeChangePatterns

import Data.List (intercalate)

-- Terminal printing ---------
import Control.Concurrent ()

import Data.Text.Prettyprint.Doc
    ( Doc, (<+>), annotate, hardline, Pretty(pretty) )
import System.Terminal
import Truthtable (generatetruthtablesfor)


data OutputFormat = OutputFormat {codeStyle :: CodePrintStyle, traceDisplay :: TraceDisplay, traceStyle :: TraceStyle, withTraceLines :: Bool, hideMandatoryNodes :: Bool}
data CodePrintStyle = ShowAST | ShowCode | ShowTikz deriving (Show)
data TraceDisplay = Trace | PC deriving (Show, Eq)
data TraceStyle = Text | Colour | None deriving (Show, Eq)

-- Some presets for output formats:

{-
The perspective of the developer who is editing code while traces are recorded in the background
This is the format used in the figures in the paper.
-}
userFormat :: OutputFormat
userFormat = OutputFormat {
    codeStyle = ShowCode,
    traceDisplay = PC,
    traceStyle = Colour,
    withTraceLines = False,
    hideMandatoryNodes = False
}

{-
A variation of 'userFormat' where traces and presence conditions can be investigated seperately at the same time.
Code is coloured in the colour of its trace while presence conditions are indicated by coloured lines on the left.
-}
userFormatDetailed :: OutputFormat
userFormatDetailed = OutputFormat {
    codeStyle = ShowCode,
    traceDisplay = Trace,
    traceStyle = Colour,
    withTraceLines = True,
    hideMandatoryNodes = False
}

{-
Shows the Abstract Syntax Tree of the source code with feature traces as formulas.
-}
astFormat :: OutputFormat
astFormat = OutputFormat {
    codeStyle = ShowAST,
    traceDisplay = Trace,
    traceStyle = Text,
    withTraceLines = False,
    hideMandatoryNodes = False
}

{-
Tikz export of AST with traces.
Used for figures in the paper.
-}
tikzFormat :: OutputFormat
tikzFormat = OutputFormat {
    codeStyle = ShowTikz,
    traceDisplay = Trace,
    traceStyle = None,
    withTraceLines = False,
    hideMandatoryNodes = False
}
        
main :: IO ()
-- main = showTruthtables
main = (<>showTruthtables) $ withTerminal $ runTerminalT $
    {-
    Select your OutputFormat here.
    Above, there is a list of presets you can choose from.
    -}
    let format = userFormat in
    do
        putDoc hardline
        headline "Running Feature Trace Recording Prototype"
        
        headline ">>> [Motivating Example] <<<"
        runExample format StackPopAlice.example
        runExample format StackPopBob.example
        
        headline ">>> [Code Change Patterns] <<<"
        runExample format CodeChangePatterns.addIfdef
        -- We omitted AddIfdef* as it is just a repitition of the previous pattern with arbitrary contexts and code fragments.
        -- AddIfDefElse has to be reproduced using two variants.
        -- Hence, we need two different examples here, one for the if-branch and one for the else-branch.
        runExample format CodeChangePatterns.addIfdefElse_IfBranch
        runExample format CodeChangePatterns.addIfdefElse_ElseBranch
        runExample format CodeChangePatterns.addIfdefWrapElse
        runExample format CodeChangePatterns.addIfdefWrapThen
        -- Adding non-variational code (code that belongs to all clones)
        runExample format CodeChangePatterns.addNormalCode_nonvariational
        -- Adding code without any associated trace into a tree-optional scope that is already traced.
        runExample format CodeChangePatterns.addNormalCode_outerpc
        -- Removing code that does not have a presence condition
        runExample format CodeChangePatterns.remNormalCode_null
        -- Removing code that has a feature trace and thereby a presence condition
        runExample format CodeChangePatterns.remNormalCode_notnull
        -- Removing code that has a feature trace
        runExample format CodeChangePatterns.remIfdef

headline :: (MonadColorPrinter m) => String -> m()
headline text = putDoc $ hardline <+> (annotate (background red) $ pretty text) <+> hardline <+> hardline

runExample :: (MonadColorPrinter m, Grammar g) => OutputFormat -> State UUID (Example m g String) -> m ()
runExample format ex =
    let example = finalizeExample ex
        result = printTraces format example (runFTR example) in
    (putDoc $ result <+> hardline) >> flush

finalizeExample :: State UUID (Example m g a) -> Example m g a
finalizeExample ex = fst $ runState ex 0

runFTR :: (Grammar g, Show a, Eq a) => Example m g a -> [(FeatureTrace g a, AST g a)]
runFTR example = featureTraceRecordingWithIntermediateSteps
        defaultFeatureTraceRecording
        (Example.startTrace example)
        (Example.startTree example)
        (Example.editscript example)
        (Example.featurecontexts example)

printTraces :: (MonadColorPrinter m, Grammar g, Show a, Eq a) => OutputFormat -> Example m g a -> [(FeatureTrace g a, AST g a)] -> Doc (Attribute m)
printTraces format example tracesAndTrees = 
    let
        featureColourPalette = colours example
        codestyle = codeStyle format
        tracestyle = traceStyle format
        tracedisplay = traceDisplay format
        withtracelines = withTraceLines format
        hidemandatory = hideMandatoryNodes format
        -- Some helper functions for output formatting
        toPC = \trace tree -> if tracedisplay == PC then pc tree trace else trace
        treeAbstract = if hidemandatory then abstract else id
        treePrint = \tree trace -> case codestyle of
            ShowAST -> (case tracestyle of
                None -> pretty.show
                Colour -> Tree.prettyPrint 0 pretty (\n -> paint (trace n) $ show n)
                Text -> pretty.(FeatureTrace.prettyPrint).(augmentWithTrace trace)) tree
            ShowTikz -> pretty $ astToTikzWithTraceDefault trace tree
            ShowCode -> showCodeAs mempty (indentGenerator trace) (stringPrint trace) (nodePrint trace) tree
            where nodePrint trace n = case tracestyle of
                      None -> pretty.removeQuotes.show $ value n
                      Colour -> paint (trace n) $ removeQuotes.show $ value n
                      Text -> pretty $ concat ["<", NullPropositions.prettyPrint $ trace n, ">", removeQuotes.show $ value n]
                  stringPrint trace n s = case tracestyle of
                      Colour -> paint (trace n) s
                      _ -> pretty s
                  indentGenerator trace n i = if tracestyle == Colour && tracedisplay == Trace && withtracelines && optionaltype n == Treeoptional
                      then mappend (paint (trace n) "|") (pretty $ genIndent (i-1))
                      else pretty $ genIndent i
                  paint formula = (annotate (foreground $ featureColourPalette formula)).pretty
        in
        mappend (annotate (background red) $ pretty $ intercalate "\n  " [
            "\nRunning "++name example
            -- "codeStyle      = "++show codestyle,
            -- "traceDisplay   = "++show tracedisplay,
            -- "traceStyle     = "++show tracestyle,
            -- "withTraceLines = "++show withtracelines,
            -- "hideMandatory  = "++show hidemandatory
            ])
        $ flip foldr
            mempty
            (\(fc, edit, (trace, tree)) s ->
                mconcat [
                    hardline,
                    hardline,
                    pretty $ concat ["==== Run ", show edit, " under context = "],
                    annotate (foreground $ featureColourPalette fc) $ pretty $ NullPropositions.prettyPrint fc,
                    pretty $ " giving us ====",
                    hardline,
                    treePrint tree trace,
                    s])
        $ zip3
            (Nothing:(featurecontexts example)) -- Prepend dummy feature context here as fc for initial tree. The context could be anything so Nothing is the simplest one.
            (edit_identity:(editscript example)) -- Prepend identity edit here to show initial tree.
            ((\(trace, tree) -> (toPC trace tree, treeAbstract tree)) <$> tracesAndTrees)


propositional_values :: [PropositionalFormula String]
propositional_values = lvalues

nullableFormula_values :: [NullableFormula String]
nullableFormula_values = lvalues

showTruthtables :: IO()
showTruthtables = withTerminal $ runTerminalT $
    do
        headline "Propositional Logic"
        putDoc.pretty $ generatetruthtablesfor propositional_values
        putDoc $ hardline <+> hardline
        headline "VariantSync Logic"
        putDoc.pretty $ generatetruthtablesfor nullableFormula_values