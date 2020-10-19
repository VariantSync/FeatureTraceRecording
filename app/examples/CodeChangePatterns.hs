module CodeChangePatterns where

import Example
import SimpleCXX
import UUID ( UUID )
import Control.Monad.State ( State )
import System.Terminal (yellow, Color, black, green, MonadColorPrinter)
import FeatureTrace
import Edits
import AST
import Propositions
import Tree
import Data.Maybe (fromJust)

feature_ULTRA_LCD :: Feature
feature_ULTRA_LCD = toFeature "ULTRA_LCD"

feature_FOO :: Feature
feature_FOO = toFeature "FOO"

featureColourPalette :: MonadColorPrinter m => Feature -> Color m
featureColourPalette f | f == feature_ULTRA_LCD = green
                       | f == feature_FOO = yellow
                       | otherwise = black


emptyfile :: State UUID SSCXXAST
emptyfile = sequence $ scxx_file "some file" []

lcd_setstatusalertpgm :: State UUID SSCXXAST
lcd_setstatusalertpgm = sequence $ scxx_exprstatement $ scxx_funccall "lcd_setalertstatuspgm" [scxx_varref "lcd_msg"]

alertstatuspgm :: State UUID SSCXXAST
alertstatuspgm = sequence $ scxx_exprstatement $ scxx_funccall "alertstatuspgm" [scxx_varref "msg"]

somefunction :: State UUID SSCXXAST
somefunction = sequence $ scxx_funcdef "void" "foo" [] []

createPatternExample :: (MonadColorPrinter m) => String -> SSCXXAST -> EditScript SimpleCXXGrammar String -> [FeatureFormula] -> Example m SimpleCXXGrammar String
createPatternExample name start edits contexts =
    Example {
        Example.name = name,
        colours = featureColourPalette,
        startTrace = emptyTrace,
        startTree = start,
        editscript = edits,
        featurecontexts = contexts
    }

addIfdef :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addIfdef = do
    start <- emptyfile
    lcd <- lcd_setstatusalertpgm
    return $ createPatternExample "AddIfdef" start
        [edit_ins_tree lcd (uuidOf start) 0]
        [Just $ PVariable feature_ULTRA_LCD]

addIfdefElse_IfBranch :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addIfdefElse_IfBranch = addIfdef >>= \ifbranch -> return ifbranch {Example.name = "AddIfdefElse (if branch)"}

addIfdefElse_ElseBranch :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addIfdefElse_ElseBranch = do
    start <- emptyfile
    alert <- alertstatuspgm
    return $ createPatternExample "AddIfdefElse (else branch)" start
        [edit_ins_tree alert (uuidOf start) 0]
        [Just $ PNot $ PVariable feature_ULTRA_LCD]

addIfdefWrapElse :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addIfdefWrapElse = do
    file <- emptyfile
    lcd <- lcd_setstatusalertpgm
    alert <- alertstatuspgm
    -- The start tree already has the alert line in it.
    let start = (run $ edit_ins_tree alert (uuidOf file) 0) file in
        return $ createPatternExample "AddIfdefWrapElse" start
        [
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
        [
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
        [edit_ins_tree lcd (uuidOf start) 0]
        [Just PTrue]

addNormalCode_outerpc :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
addNormalCode_outerpc = do
    file <- emptyfile
    foo <- somefunction
    lcd <- lcd_setstatusalertpgm
    let
        outer_pc = Just $ PVariable feature_FOO
        start = (run $ edit_ins_tree foo (uuidOf file) 0) file
        statementsOfFoo = uuidOf . fromJust $ findByRule SCXX_Statements foo
        in
        return $
            (createPatternExample "AddNormalCode (with outer PC)" start
                [edit_ins_tree lcd statementsOfFoo 0]
                -- Here, any feature context would be feasible that is weaker than outer_pc (e.g., outer_pc itself).
                [Just PTrue])
            -- change the start trace so that there is already a trace
            {startTrace = \n -> if n == element foo then outer_pc else Nothing}

remNormalCode_null :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
remNormalCode_null = do
    file <- emptyfile
    lcd <- lcd_setstatusalertpgm
    let start = (run $ edit_ins_tree lcd (uuidOf file) 0) file in
        return $ createPatternExample "RemNormalCode (without traces)" start
            [edit_del_tree (uuidOf lcd)]
            [Just PTrue]

remNormalCode_notnull :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
remNormalCode_notnull = do
    file <- emptyfile
    lcd <- lcd_setstatusalertpgm
    let 
        start = (run $ edit_ins_tree lcd (uuidOf file) 0) file
        existingtrace = Just $ PVariable feature_ULTRA_LCD
        in
        return $
            (createPatternExample "RemNormalCode (with trace)" start
            [edit_del_tree (uuidOf lcd)]
            [Nothing {-null-}])
            {startTrace = \n -> if n == element lcd then existingtrace else Nothing}

{- This behaves the same as remNormalCode_notnull. -}
remIfdef :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
remIfdef = remNormalCode_notnull >>= \r -> return r {Example.name = "RemIfdef"}