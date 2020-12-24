module CodeChangePatterns where

import Example
import SimpleCXX
import UUID ( UUID )
import Control.Monad.State ( State )
import System.Terminal
import Feature
import FeatureTrace
import FeatureTraceRecording
import FeatureColour
import Edits
import AST
import Propositions
import Tree
import Data.Maybe (fromJust)

feature_ULTRA_LCD :: Feature
feature_ULTRA_LCD = toFeature "ULTRA_LCD"

feature_FOO :: Feature
feature_FOO = toFeature "FOO"

featurecolours :: MonadColorPrinter m => FeatureFormulaColourPalette m
featurecolours p
    | p == (Just $ PNot $ PVariable $ feature_ULTRA_LCD) = magenta
    | p == (Just $ PVariable $ feature_ULTRA_LCD) = green
    | p == (Just $ PVariable $ feature_FOO) = yellow
    | otherwise = white


emptyfile :: State UUID SSCXXAST
emptyfile = sequence $ scxx_file "some file" []

lcd_setstatusalertpgm :: State UUID SSCXXAST
lcd_setstatusalertpgm = sequence $ scxx_exprstatement $ scxx_funccall "lcd_setalertstatuspgm" [scxx_varref "lcd_msg"]

alertstatuspgm :: State UUID SSCXXAST
alertstatuspgm = sequence $ scxx_exprstatement $ scxx_funccall "alertstatuspgm" [scxx_varref "msg"]

somefunction :: State UUID SSCXXAST
somefunction = sequence $ scxx_funcdef "void" "foo" [] []

createPatternExample :: (MonadColorPrinter m) => String -> SSCXXAST -> History SimpleCXXGrammar String -> Example m SimpleCXXGrammar String
createPatternExample = createPatternExampleWithStartTrace emptyTrace

createPatternExampleWithStartTrace :: (MonadColorPrinter m) => FeatureTrace SimpleCXXGrammar String -> String -> SSCXXAST -> History SimpleCXXGrammar String -> Example m SimpleCXXGrammar String
createPatternExampleWithStartTrace startTrace name start edits =
    Example {
        Example.name = name,
        colours = featurecolours,
        startVersion = (startTrace, start),
        history = edits
    }

addIfdef :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addIfdef = do
    start <- emptyfile
    lcd <- lcd_setstatusalertpgm
    return $ createPatternExample "AddIfdef" start
        [(edit_ins_tree lcd (uuidOf start) 0, Just $ PVariable feature_ULTRA_LCD)]

addIfdefElse_IfBranch :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addIfdefElse_IfBranch = addIfdef >>= \ifbranch -> return ifbranch {Example.name = "AddIfdefElse (if branch)"}

addIfdefElse_ElseBranch :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addIfdefElse_ElseBranch = do
    start <- emptyfile
    alert <- alertstatuspgm
    return $ createPatternExample "AddIfdefElse (else branch)" start
        [(edit_ins_tree alert (uuidOf start) 0, Just $ PNot $ PVariable feature_ULTRA_LCD)]

addIfdefWrapElse :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
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

addIfdefWrapThen :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
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

addNormalCode_nonvariational :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addNormalCode_nonvariational = do
    start <- emptyfile
    lcd <- lcd_setstatusalertpgm
    return $ createPatternExample "AddNormalCode (non-variational)" start
        [(edit_ins_tree lcd (uuidOf start) 0, Just PTrue)]

addNormalCode_outerpc :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addNormalCode_outerpc = do
    file <- emptyfile
    foo <- somefunction
    lcd <- lcd_setstatusalertpgm
    let
        outer_pc = Just $ PVariable feature_FOO
        start = (run $ edit_ins_tree foo (uuidOf file) 0) file
        statementsOfFoo = uuidOf . fromJust $ findByGrammarType SCXX_Statements foo
        in
        return $ createPatternExampleWithStartTrace
            -- change the start trace so that there is already a trace
            (\n -> if n == element foo then outer_pc else Nothing)
            "AddNormalCode (with outer PC)"
            start
            -- Here, any feature context would be feasible that is weaker than outer_pc (e.g., outer_pc itself).
            [(edit_ins_tree lcd statementsOfFoo 0, Just PTrue)]
            

remNormalCode_null :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
remNormalCode_null = do
    file <- emptyfile
    lcd <- lcd_setstatusalertpgm
    let start = (run $ edit_ins_tree lcd (uuidOf file) 0) file in
        return $ createPatternExample "RemNormalCode (without traces)" start
            [(edit_del_tree (uuidOf lcd), Just PTrue)]

remNormalCode_notnull :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
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
remIfdef :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
remIfdef = remNormalCode_notnull >>= \r -> return r {Example.name = "RemIfdef"}