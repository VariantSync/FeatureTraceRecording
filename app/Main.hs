module Main where

import Control.Monad.State

import UUID
import Util
import Tree
import AST
import Edits
import Propositions
import NullPropositions
import SAT
import FeatureTrace
import FeatureTraceRecording
import FTRDirect
import FTRTwoStep

import FeatureColour

import Div

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

data CodePrintStyle = ShowAST | ShowCode
data TraceDisplay = Trace | PC deriving Eq
data TraceStyle = Text | Colour | None deriving Eq

main :: IO ()
main = withTerminal $ runTerminalT printer

printer :: (MonadColorPrinter m) => m ()
printer = (putDoc $ runFTR <+> hardline) >> flush
    
runFTR :: (MonadColorPrinter m) => Doc (Attribute m)
runFTR = fst . flip runState 0 $ do
    tree0 <- div0
    tree_assert <- div_assert
    tree_error <- div_error
    tree_condition <- div_condition
    tree_div <- div_div
    let 
        -- Debug settings
        codeStyle = ShowCode
        traceDisplay = PC
        traceStyle = Colour
        withTraceLines = True
        abstractTrees = False
        featureColourPalette = Div.colourOf
        -- The initial feature trace of the first tree.
        trace0 = emptyTrace
        -- Some helper variables for edits
        id_reciprocal_body = uuidOf . fromJust $ find (\(Tree n _) -> value n == "body") tree0
        id_return = uuidOf . fromJust $ find (\(Tree n _) -> valuetype n == ASTT_Return) tree0
        id_cond_body = uuidOf . fromJust $ find (\(Tree n _) -> value n == "body") tree_condition
        id_div_body = uuidOf . fromJust $ find (\(Tree n _) -> value n == "body") tree_div
        -- The edits "made by the developer"
        editscript = [
            edit_ins_tree tree_assert id_reciprocal_body 0
          , edit_ins_partial tree_condition id_reciprocal_body 0 0 id_cond_body 0
          , edit_del_tree (uuidOf tree_assert)
          , edit_ins_tree tree_error id_cond_body 0
          , edit_ins_tree tree_div (uuidOf tree0) 0
          , edit_move_tree (uuidOf tree_condition) id_div_body 0
          , edit_move_tree id_return id_div_body 1 -- now we are done with div5.txt
            ]
        -- The feature contexts assigned to each edit
        featureContexts = [
            Just $ PVariable feature_Debug
          , Just $ PVariable feature_Reciprocal
          , Just $ PVariable feature_Reciprocal -- Error by user. Should actually be PTrue
          , Just $ PVariable feature_Reciprocal
          , Just $ PVariable feature_Division
          , Just $ PVariable feature_Division
          , Just $ PVariable feature_Division
            ]
        -- Select the FeatureTraceRecording implementation to run
        recordBuilder = FTRTwoStep.builder
        -- Run the ftr
        tracesAndTrees = featureTraceRecordingWithIntermediateSteps recordBuilder trace0 tree0 editscript featureContexts
        -- tracesAndTrees = [featureTraceRecording recordBuilder trace0 tree0 editscript featureContexts]
        -- Some helper variables for output formatting
        toPC = \trace tree -> if traceDisplay == PC then pc tree trace else trace
        treeAbstract = if abstractTrees then abstract else id
        treePrint = \tree trace -> case codeStyle of
            ShowAST -> (case traceStyle of
                None -> pretty.show
                Colour -> Tree.prettyPrint 0 pretty (\n -> paint (trace n) $ show n)
                Text -> pretty.(FeatureTrace.prettyPrint).(augmentWithTrace trace)) tree
            ShowCode -> showCodeAs (pretty "") (indentGenerator trace) (stringPrint trace) (nodePrint trace) tree
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
        $ flip foldr mempty (\(fc, edit, (trace, tree)) s ->
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
            (edit_identity:editscript) -- Prepend identity edit here to show initial tree.
            ((\(trace, tree) -> (toPC trace tree, treeAbstract tree)) <$> tracesAndTrees)
