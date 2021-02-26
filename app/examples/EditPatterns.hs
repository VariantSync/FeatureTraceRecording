module EditPatterns where

import Example
import SimpleJava
import UUID ( UUID )
import Control.Monad.State ( State )
import System.Terminal
import Feature
import FeatureTrace
import NullPropositions
import FeatureTraceRecording
import FeatureColour
import Edits
import AST
import Propositions
import Tree
import Data.Maybe (fromJust)

feature_ULTRA_LCD :: Feature
feature_ULTRA_LCD = toFeature "m"

feature_FOO :: Feature
feature_FOO = toFeature "FOO"

rootName :: String
rootName = "some file"

buildPCName :: String -> FeatureFormula -> String
buildPCName nodename f = " (with \""++nodename++"\" mapped to "++(NullPropositions.prettyPrint f)++")"

featurecolours :: MonadColorPrinter m => FeatureFormulaColourPalette m
featurecolours p
    | p == (Just $ PNot $ PVariable $ feature_ULTRA_LCD) = magenta
    | p == (Just $ PVariable $ feature_ULTRA_LCD) = green
    | p == (Just $ PVariable $ feature_FOO) = yellow
    | otherwise = white

emptyfile :: State UUID SSJavaAST
emptyfile = sequence $ sjava_file rootName []

lcd_setstatusalertpgm :: State UUID SSJavaAST
lcd_setstatusalertpgm = sequence $ sjava_exprstatement $ sjava_funccall "lcd_setalertstatuspgm" [sjava_varref "lcd_msg"]

alertstatuspgm :: State UUID SSJavaAST
alertstatuspgm = sequence $ sjava_exprstatement $ sjava_funccall "alertstatuspgm" [sjava_varref "msg"]

somefunction :: State UUID SSJavaAST
somefunction = sequence $ sjava_methoddef "void" "foo" [] []

createPatternExample :: (MonadColorPrinter m) => String -> SSJavaAST -> History SimpleJavaGrammar String -> Example m SimpleJavaGrammar String
createPatternExample = createPatternExampleWithStartTrace emptyTrace

createPatternExampleWithStartTrace :: (MonadColorPrinter m) => FeatureTrace SimpleJavaGrammar String -> String -> SSJavaAST -> History SimpleJavaGrammar String -> Example m SimpleJavaGrammar String
createPatternExampleWithStartTrace startTrace name start edits =
    Example {
        Example.name = name,
        colours = featurecolours,
        startVersion = (startTrace, start),
        history = edits
    }

addIfdef :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdef = do
    start <- emptyfile
    lcd <- lcd_setstatusalertpgm
    return $ createPatternExample "AddIfdef (general case)" start
        [(edit_ins_tree lcd (uuidOf start) 0, Just $ PVariable feature_ULTRA_LCD)]

addIfdefWithPC :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefWithPC = do
    plainAddIfdef <- addIfdef
    let (startTrace, startAST) = startVersion plainAddIfdef
        fileNode = element $ fromJust $ findByGrammarType SJava_File startAST
        ultraFormula = Just $ PVariable feature_ULTRA_LCD
    return $ plainAddIfdef{
        Example.name = (Example.name plainAddIfdef)++(buildPCName (value fileNode) ultraFormula),
        startVersion = (\v -> if v == fileNode then ultraFormula else (startTrace v), startAST),
        history = (\(edit, fc) -> (edit, Nothing)) <$> history plainAddIfdef
    }

addIfdefElse_IfBranch :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefElse_IfBranch = addIfdef >>= \ifbranch -> return ifbranch {Example.name = "AddIfdefElse (if branch) (general case)"}

addIfdefElse_IfBranchWithPC :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefElse_IfBranchWithPC = addIfdefWithPC >>= \ifbranch -> return ifbranch {Example.name = "AddIfdefElse (if branch) "++(buildPCName rootName $ Just $ PVariable feature_ULTRA_LCD)}

addIfdefElse_ElseBranch :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefElse_ElseBranch = do
    start <- emptyfile
    alert <- alertstatuspgm
    return $ createPatternExample "AddIfdefElse (else branch)" start
        [(edit_ins_tree alert (uuidOf start) 0, Just $ PNot $ PVariable feature_ULTRA_LCD)]

addIfdefElse_ElseBranchWithPC :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefElse_ElseBranchWithPC = do
    start <- emptyfile
    alert <- alertstatuspgm
    let context = Just $ PNot $ PVariable feature_ULTRA_LCD
    return $ createPatternExampleWithStartTrace
        (\v -> if v == (element start) then context else Nothing)
        ("AddIfdefElse (else branch)"++(buildPCName (value $ element start) context))
        start
        [(edit_ins_tree alert (uuidOf start) 0, Nothing)]

addIfdefWrapElse :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefWrapElse = do
    file <- emptyfile
    lcd <- lcd_setstatusalertpgm
    alert <- alertstatuspgm
    -- The start tree already has the alert line in it.
    let start = (run $ edit_ins_tree alert (uuidOf file) 0) file in
        return $ createPatternExample "AddIfdefWrapElse" start
        $ zip [
            edit_del_tree $ uuidOf alert,
            edit_ins_tree lcd (uuidOf file) 0
        ]
        [
            Just $ PVariable feature_ULTRA_LCD,
            Just $ PVariable feature_ULTRA_LCD
        ]

addIfdefWrapThen :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addIfdefWrapThen = do
    file <- emptyfile
    lcd <- lcd_setstatusalertpgm
    alert <- alertstatuspgm
    -- The start tree already has the alert line in it.
    let start = (run $ edit_ins_tree lcd (uuidOf file) 0) file in
        return $ createPatternExample "AddIfdefWrapThen" start
        $ zip [
            edit_del_tree $ uuidOf lcd,
            edit_ins_tree alert (uuidOf file) 0
        ]
        [
            Just $ PNot $ PVariable feature_ULTRA_LCD,
            Just $ PNot $ PVariable feature_ULTRA_LCD
        ]

addNormalCode_nonvariational :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addNormalCode_nonvariational = do
    start <- emptyfile
    lcd <- lcd_setstatusalertpgm
    return $ createPatternExample "AddNormalCode (non-variational)" start
        [(edit_ins_tree lcd (uuidOf start) 0, Just PTrue)]

addNormalCode_outerpc :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
addNormalCode_outerpc = do
    file <- emptyfile
    foo <- somefunction
    lcd <- lcd_setstatusalertpgm
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
            

remNormalCode_null :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
remNormalCode_null = do
    file <- emptyfile
    lcd <- lcd_setstatusalertpgm
    let start = (run $ edit_ins_tree lcd (uuidOf file) 0) file in
        return $ createPatternExample "RemNormalCode (without traces)" start
            [(edit_del_tree (uuidOf lcd), Just PTrue)]

remNormalCode_notnull :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
remNormalCode_notnull = do
    file <- emptyfile
    lcd <- lcd_setstatusalertpgm
    let 
        start = (run $ edit_ins_tree lcd (uuidOf file) 0) file
        existingtrace = Just $ PVariable feature_ULTRA_LCD
        in
        return $ createPatternExampleWithStartTrace
            (\n -> if n == element lcd then existingtrace else Nothing)
            "RemNormalCode (with trace)"
            start
            [(edit_del_tree (uuidOf lcd), Nothing {-null-})]

{- This behaves the same as remNormalCode_notnull. -}
remIfdef :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
remIfdef = remNormalCode_notnull >>= \r -> return r {Example.name = "RemIfdef"}