module Main where

import Control.Monad.State

import UUID
import Util
import Tree
import AST
import SimpleCXX
import Edits
import Propositions
import NullPropositions
import SAT
import FeatureTrace
import FeatureTraceRecording
import FTRDirect
import FTRTwoStep

import Example

import FeatureColour

import Div
import VR ( vrExample )
import StackPop (example)

import Data.Maybe ( fromJust )
import Data.List (intercalate)

-- Terminal printing ---------
import Control.Concurrent ()
import Control.Monad
import Control.Monad.IO.Class

import Data.Text.Prettyprint.Doc
import System.Terminal

-- import Prelude hiding ((<>))
-----------------------------

data CodePrintStyle = ShowAST | ShowCode deriving (Show)
data TraceDisplay = Trace | PC deriving (Show, Eq)
data TraceStyle = Text | Colour | None deriving (Show, Eq)

main :: IO ()
main = withTerminal $ runTerminalT printer

printer :: (MonadColorPrinter m) => m ()
printer = (putDoc $ runFTR <+> hardline) >> flush
    
runFTR :: (MonadColorPrinter m) => Doc (Attribute m)
runFTR = fst . flip runState 0 $ do
    -- Example to run
    example <- StackPop.example
    let
        -- Debug settings
        codeStyle = ShowCode -- One of: ShowAST, ShowCode
        traceDisplay = PC -- One of: Trace, PC
        traceStyle = Colour -- One of: Text, Colour, None
        withTraceLines = True
        abstractTrees = False
        -- Get necessary data from Example
        editScript = editscript example
        featureContexts = featurecontexts example
        featureColourPalette = colours example
        -- Select the FeatureTraceRecording implementation to run
        recordBuilder = FTRTwoStep.builder
        -- Run the ftr
        tracesAndTrees = featureTraceRecordingWithIntermediateSteps
            recordBuilder
            (Example.startTrace example)
            (Example.startTree example)
            editScript
            featureContexts
        -- tracesAndTrees = [featureTraceRecording recordBuilder trace0 tree0 editscript featureContexts]
        -- Some helper variables for output formatting
        toPC = \trace tree -> if traceDisplay == PC then pc tree trace else trace
        treeAbstract = if abstractTrees then abstract else id
        treePrint = \tree trace -> case codeStyle of
            ShowAST -> (case traceStyle of
                None -> pretty.show
                Colour -> Tree.prettyPrint 0 pretty (\n -> paint (trace n) $ show n)
                Text -> pretty.(FeatureTrace.prettyPrint).(augmentWithTrace trace)) tree
            ShowCode -> showCodeAs mempty (indentGenerator trace) (stringPrint trace) (nodePrint trace) tree
            where nodePrint trace n = case traceStyle of
                      None -> pretty $ value n
                      Colour -> paint (trace n) $ value n
                      Text -> pretty $ concat ["<", NullPropositions.prettyPrint $ trace n, ">", value n]
                  stringPrint trace n s = case traceStyle of
                      Colour -> paint (trace n) s
                      _ -> pretty s
                  indentGenerator trace n i = if traceStyle == Colour && traceDisplay == Trace && withTraceLines && ntype n == Legator
                      then mappend (paint (trace n) "|") (pretty $ genIndent (i-1))
                      else pretty $ genIndent i
                  paint formula = (annotate (foreground $ FeatureColour.colourOf featureColourPalette formula)).pretty
    return
        $ mappend (pretty $ intercalate "\n  " [
            "\nRunning Feature Trace Recording with",
            "codeStyle      = "++show codeStyle,
            "traceDisplay   = "++show traceDisplay,
            "traceStyle     = "++show traceStyle,
            "withTraceLines = "++show withTraceLines,
            "abstractTrees  = "++show abstractTrees])
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
            (Nothing:featureContexts) -- Prepend dummy feature context here as fc for initial tree. The context could be anything so Nothing is the simplest one.
            (edit_identity:editScript) -- Prepend identity edit here to show initial tree.
            ((\(trace, tree) -> (toPC trace tree, treeAbstract tree)) <$> tracesAndTrees)
