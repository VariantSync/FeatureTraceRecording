module Main where

import Control.Monad.State ( State, runState )

import UUID ( UUID )
import Util ( genIndent )
import Tree ( prettyPrint )
import AST
import Edits ( edit_identity )
import NullPropositions ( prettyPrint )
import FeatureTrace (FeatureTrace,  augmentWithTrace, pc, prettyPrint )
import FeatureTraceRecording 
import FTRDirect ( builder )
import FTRTwoStep ( builder )

import Example

import FeatureColour ( colourOf )
import TikzExport ( astToTikzWithTraceDefault )

import Div (divExample)
import VR ( vrExample )
import StackPopAlice (example)
import StackPopBob (example)

import Data.Maybe ( fromJust )
import Data.List (intercalate)

-- Terminal printing ---------
import Control.Concurrent ()

import Data.Text.Prettyprint.Doc
    ( Doc, (<+>), annotate, hardline, Pretty(pretty) )
import System.Terminal
    ( MonadColorPrinter(foreground),
      withTerminal,
      putDoc,
      runTerminalT,
      MonadMarkupPrinter(Attribute),
      MonadPrinter(flush) )
import System.Terminal.Internal (LocalTerminal)

data OutputFormat = OutputFormat {codeStyle :: CodePrintStyle, traceDisplay :: TraceDisplay, traceStyle :: TraceStyle, withTraceLines :: Bool, hidePlainNodes :: Bool}
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
    hidePlainNodes = False
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
    hidePlainNodes = False
}

{-
Shows the AST with trace formulas.
-}
debugFormat :: OutputFormat
debugFormat = OutputFormat {
    codeStyle = ShowAST,
    traceDisplay = Trace,
    traceStyle = Text,
    withTraceLines = False,
    hidePlainNodes = False
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
    hidePlainNodes = False
}

main :: IO ()
main = withTerminal $ runTerminalT $
    -- select your OutputFormat here. Above, there is a list of presets you can choose from.
    let format = userFormat in
    do
        printer format StackPopAlice.example
        printer format StackPopBob.example

printer :: (MonadColorPrinter m, Grammar g) => OutputFormat -> State UUID (Example m g String) -> m ()
printer format ex =
    let example = finalizeExample ex
        result = printTraces format example (runFTR example) in
    (putDoc $ result <+> hardline) >> flush

finalizeExample :: State UUID (Example m g a) -> Example m g a
finalizeExample ex = fst $ runState ex 0

runFTR :: Grammar g => Example m g String -> [(FeatureTrace g String, AST g String)]
runFTR example = featureTraceRecordingWithIntermediateSteps
        FTRTwoStep.builder
        (Example.startTrace example)
        (Example.startTree example)
        (Example.editscript example)
        (Example.featurecontexts example)

printTraces :: (MonadColorPrinter m, Grammar g) => OutputFormat -> Example m g String -> [(FeatureTrace g String, AST g String)] -> Doc (Attribute m)
printTraces format example tracesAndTrees = 
    let
        featureColourPalette = colours example
        codestyle = codeStyle format
        tracestyle = traceStyle format
        tracedisplay = traceDisplay format
        withtracelines = withTraceLines format
        hideplain = hidePlainNodes format
        -- Some helper functions for output formatting
        toPC = \trace tree -> if tracedisplay == PC then pc tree trace else trace
        treeAbstract = if hideplain then abstract else id
        treePrint = \tree trace -> case codestyle of
            ShowAST -> (case tracestyle of
                None -> pretty.show
                Colour -> Tree.prettyPrint 0 pretty (\n -> paint (trace n) $ show n)
                Text -> pretty.(FeatureTrace.prettyPrint).(augmentWithTrace trace)) tree
            ShowTikz -> pretty $ astToTikzWithTraceDefault trace tree
            ShowCode -> showCodeAs mempty (indentGenerator trace) (stringPrint trace) (nodePrint trace) tree
            where nodePrint trace n = case tracestyle of
                      None -> pretty $ value n
                      Colour -> paint (trace n) $ value n
                      Text -> pretty $ concat ["<", NullPropositions.prettyPrint $ trace n, ">", value n]
                  stringPrint trace n s = case tracestyle of
                      Colour -> paint (trace n) s
                      _ -> pretty s
                  indentGenerator trace n i = if tracestyle == Colour && tracedisplay == Trace && withtracelines && ntype n == Legator
                      then mappend (paint (trace n) "|") (pretty $ genIndent (i-1))
                      else pretty $ genIndent i
                  paint formula = (annotate (foreground $ FeatureColour.colourOf featureColourPalette formula)).pretty
        in
        mappend (pretty $ intercalate "\n  " [
            "\nRunning Feature Trace Recording:",
            "example        = "++name example,
            "codeStyle      = "++show codestyle,
            "traceDisplay   = "++show tracedisplay,
            "traceStyle     = "++show tracestyle,
            "withTraceLines = "++show withtracelines,
            "hidePlainNodes = "++show hideplain])
        $ flip foldr
            mempty
            (\(fc, edit, (trace, tree)) s ->
                mconcat [
                    hardline,
                    hardline,
                    pretty $ concat ["==== Run ", show edit, " under context = "],
                    annotate (foreground $ FeatureColour.colourOf featureColourPalette fc) $ pretty $ NullPropositions.prettyPrint fc,
                    pretty $ " giving us ====",
                    hardline,
                    treePrint tree trace,
                    s])
        $ zip3
            (Nothing:(featurecontexts example)) -- Prepend dummy feature context here as fc for initial tree. The context could be anything so Nothing is the simplest one.
            (edit_identity:(editscript example)) -- Prepend identity edit here to show initial tree.
            ((\(trace, tree) -> (toPC trace tree, treeAbstract tree)) <$> tracesAndTrees)
