{- |
Description: Examples for edit patterns used in our evaluation.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

This module provides 'Example's to reproduce each edit pattern we inspected in the evaluation
of feature trace recording (Section 5 in our paper).
As each pattern describes a possible /type/ of edit and not an edit itself, each example shows
one possible instance for a pattern (not the pattern itself as this is not possible).
For some patterns, we thus /have to/ show multiple examples as described in the paper.
Most notably, distinguishing between the case when no feature traces are present (/in general/ case)
and the case when an optional outer scope of the edit code fragment is already mapped to the target feature mapping.
-}
module Examples.EditPatterns where

import Examples.Example as Example
import Tree.Grammars.SimpleJava
import UUID ( UUID )
import Control.Monad.State ( State )
import System.Terminal
import Feature.Feature
import Feature.FeatureTrace
import Propositions.NullPropositions as NullPropositions
import Feature.Recording.FeatureTraceRecording
import Feature.FeatureColour
import Tree.Edits
import Tree.AST
import Propositions.Propositions
import Tree.Tree
import Data.Maybe (fromJust)

-- | A dummy feature that is edited in each example.
-- We name it @m@ (mapping) here according to the variable used in the paper.
feature_m :: Feature
feature_m = toFeature "m"

-- | Another dummy feature that can be edited.
feature_FOO :: Feature
feature_FOO = toFeature "FOO"

-- | Name for the 'AST' root of each example instance.
-- We use file nodes to represent the roots of the 'AST's.
rootName :: String
rootName = "some file"

-- | Describe the feature mapping of a node with the given name as text.
buildPCName :: String -> FeatureFormula -> String
buildPCName nodename f = " (with \""++nodename++"\" mapped to "++(NullPropositions.prettyPrint f)++")"

-- | Colours for features and feature formulas used in the pattern examples.
featurecolours :: MonadColorPrinter m => FeatureFormulaColourPalette m
featurecolours p
    | p == (Just $ PNot $ PVariable $ feature_m) = magenta
    | p == (Just $ PVariable $ feature_m) = green
    | p == (Just $ PVariable $ feature_FOO) = yellow
    | otherwise = white

-- | 'AST' representing an empty file.
emptyfile :: State UUID SSJavaAST
emptyfile = sequence $ sjava_file rootName []

-- | 'AST' representing the source code in the /then/ branch of a preprocessor annotation.
code_then :: State UUID SSJavaAST
code_then = sequence $ sjava_exprstatement $ sjava_funccall "codeThenCase" []

-- | 'AST' representing the source code in the /else/ branch of a preprocessor annotation.
code_else :: State UUID SSJavaAST
code_else = sequence $ sjava_exprstatement $ sjava_funccall "codeElseCase" []

-- | 'AST' representing a method 'void foo()'.
somefunction :: State UUID SSJavaAST
somefunction = sequence $ sjava_methoddef "void" "foo" [] []

{- |
Create an 'Example' for a specific pattern.
Calls 'createPatternExampleWithStartTrace' and defaults its first argument to an empty feature trace that assigns /null/ to every node ('emptyTrace').
-}
createPatternExample :: (MonadColorPrinter m) => String -> SSJavaAST -> History SimpleJavaGrammar String -> Example m SimpleJavaGrammar String
createPatternExample = createPatternExampleWithStartTrace emptyTrace

{-
Create an 'Example' for a specific pattern.
Arguments are
* the initial feature trace (e.g., 'emptyTrace'),
* the name of the pattern,
* the 'AST' that represents the initial version of the source code, and
* a sequence of edits to apply to the given 'AST' when running the example.
-}
createPatternExampleWithStartTrace :: (MonadColorPrinter m) => FeatureTrace SimpleJavaGrammar String -> String -> SSJavaAST -> History SimpleJavaGrammar String -> Example m SimpleJavaGrammar String
createPatternExampleWithStartTrace startTrace name start edits =
    Example {
        Example.name = name,
        colours = featurecolours,
        startVersion = (startTrace, start),
        history = edits
    }

{- |
'Example' instance of the /AddIfdef/ pattern in general case.
-}
addIfdef :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdef = do
    start <- emptyfile
    lcd <- code_then
    return $ createPatternExample "AddIfdef (general case)" start
        [(edit_ins_tree lcd (uuidOf start) 0, Just $ PVariable feature_m)]

{- |
'Example' instance of the /AddIfdef/ pattern when an outer optional scope is already mapped to the target feature mapping.
-}
addIfdefWithPC :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefWithPC = do
    plainAddIfdef <- addIfdef
    let (startTrace, startAST) = startVersion plainAddIfdef
        fileNode = element $ fromJust $ findByGrammarType SJava_File startAST
        ultraFormula = Just $ PVariable feature_m
    return $ plainAddIfdef{
        Example.name = "AddIfdef"++(buildPCName (value fileNode) ultraFormula),
        startVersion = (\v -> if v == fileNode then ultraFormula else (startTrace v), startAST),
        history = (\(edit, _) -> (edit, Nothing)) <$> history plainAddIfdef
    }

{- |
'Example' instance of the /AddIfdefElse/ pattern in general case.
As /AddIfdefElse/ has to be reproduced using two variants, we need two different examples here, one for the /then/-branch and one for the /else/-branch.
This is the 'Example' of the /then/ branch.
-}
addIfdefElse_IfBranch :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefElse_IfBranch = addIfdef >>= \ifbranch -> return ifbranch {Example.name = "AddIfdefElse (if branch) (general case)"}

{- |
'Example' instance of the /AddIfdefElse/ pattern when an outer optional scope is already mapped to the target feature mapping.
As /AddIfdefElse/ has to be reproduced using two variants, we need two different examples here, one for the /then/-branch and one for the /else/-branch.
This is the 'Example' of the /then/ branch.
-}
addIfdefElse_IfBranchWithPC :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefElse_IfBranchWithPC = addIfdefWithPC >>= \ifbranch -> return ifbranch {Example.name = "AddIfdefElse (if branch)"++(buildPCName rootName $ Just $ PVariable feature_m)}

{- |
'Example' instance of the /AddIfdefElse/ pattern in general case.
As /AddIfdefElse/ has to be reproduced using two variants, we need two different examples here, one for the /then/-branch and one for the /else/-branch.
This is the 'Example' of the /else/ branch.
-}
addIfdefElse_ElseBranch :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefElse_ElseBranch = do
    start <- emptyfile
    alert <- code_else
    return $ createPatternExample "AddIfdefElse (else branch) (general case)" start
        [(edit_ins_tree alert (uuidOf start) 0, Just $ PNot $ PVariable feature_m)]

{- |
'Example' instance of the /AddIfdefElse/ pattern when an outer optional scope is already mapped to the target feature mapping.
As /AddIfdefElse/ has to be reproduced using two variants, we need two different examples here, one for the /then/-branch and one for the /else/-branch.
This is the 'Example' of the /else/ branch.
-}
addIfdefElse_ElseBranchWithPC :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefElse_ElseBranchWithPC = do
    start <- emptyfile
    alert <- code_else
    let context = Just $ PNot $ PVariable feature_m
    return $ createPatternExampleWithStartTrace
        (\v -> if v == (element start) then context else Nothing)
        ("AddIfdefElse (else branch)"++(buildPCName (value $ element start) context))
        start
        [(edit_ins_tree alert (uuidOf start) 0, Nothing)]

{- |
'Example' instance of the /AddIfdefWrapElse/ pattern.
-}
addIfdefWrapElse :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefWrapElse = do
    file <- emptyfile
    lcd <- code_then
    alert <- code_else
    -- The start tree already has the alert line in it.
    let start = (run $ edit_ins_tree alert (uuidOf file) 0) file in
        return $ createPatternExample "AddIfdefWrapElse" start
        $ zip [
            edit_del_tree $ uuidOf alert,
            edit_ins_tree lcd (uuidOf file) 0
        ]
        [
            Just $ PVariable feature_m,
            Just $ PVariable feature_m
        ]

{- |
'Example' instance of the /AddIfdefWrapThen/ pattern.
-}
addIfdefWrapThen :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefWrapThen = do
    file <- emptyfile
    lcd <- code_then
    alert <- code_else
    -- The start tree already has the alert line in it.
    let start = (run $ edit_ins_tree lcd (uuidOf file) 0) file in
        return $ createPatternExample "AddIfdefWrapThen" start
        $ zip [
            edit_del_tree $ uuidOf lcd,
            edit_ins_tree alert (uuidOf file) 0
        ]
        [
            Just $ PNot $ PVariable feature_m,
            Just $ PNot $ PVariable feature_m
        ]

{- |
'Example' instance of the /AddNormalCode/ pattern where non-variational code is added (code mapped to /true/).
-}
addNormalCode_nonvariational :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addNormalCode_nonvariational = do
    start <- emptyfile
    lcd <- code_then
    return $ createPatternExample "AddNormalCode (non-variable)" start
        [(edit_ins_tree lcd (uuidOf start) 0, Just PTrue)]

{- |
'Example' instance of the /AddNormalCode/ pattern when an outer optional scope is already mapped to the target feature mapping.
-}
addNormalCode_outerpc :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addNormalCode_outerpc = do
    file <- emptyfile
    foo <- somefunction
    lcd <- code_then
    let
        outer_pc = Just $ PVariable feature_FOO
        start = (run $ edit_ins_tree foo (uuidOf file) 0) file
        statementsOfFoo = uuidOf . fromJust $ findByGrammarType SJava_Statements foo
        in
        return $ createPatternExampleWithStartTrace
            -- change the start trace so that there is already a trace
            (\n -> if n == element foo then outer_pc else Nothing)
            "AddNormalCode (with outer PC)"
            start
            -- Here, any feature context would be feasible that is weaker than outer_pc (e.g., outer_pc itself).
            [(edit_ins_tree lcd statementsOfFoo 0, Just PTrue)]
            
{- |
'Example' instance of the /RemNormalCode/ pattern when no feature traces are present initially.
-}
remNormalCode_null :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
remNormalCode_null = do
    file <- emptyfile
    lcd <- code_then
    let start = (run $ edit_ins_tree lcd (uuidOf file) 0) file in
        return $ createPatternExample "RemNormalCode (without traces)" start
            [(edit_del_tree (uuidOf lcd), Just PTrue)]

{- |
'Example' instance of the /RemNormalCode/ pattern when feature traces are present initally.
-}
remNormalCode_notnull :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
remNormalCode_notnull = do
    file <- emptyfile
    lcd <- code_then
    let 
        start = (run $ edit_ins_tree lcd (uuidOf file) 0) file
        existingtrace = Just $ PVariable feature_m
        in
        return $ createPatternExampleWithStartTrace
            (\n -> if n == element lcd then existingtrace else Nothing)
            "RemNormalCode (with trace)"
            start
            [(edit_del_tree (uuidOf lcd), Nothing {-null-})]

{- |
'Example' instance of the /RemIfdef/ pattern.
This behaves the same as 'remNormalCode_notnull'.
-}
remIfdef :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
remIfdef = remNormalCode_notnull >>= \r -> return r {Example.name = "RemIfdef"}
