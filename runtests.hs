{-# LANGUAGE OverloadedStrings #-}
import Util
import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit
import Model

main = hspec $ describe "validateContent"
    [ it "adds IDs to DITA content" $
        "<p id=\"x2\">foo</p><p id=\"x1\">bar</p>" @=? validateContent TFDitaConcept "<p>foo</p><p id=\"x1\">bar</p>"
    , it "strips duplicate ids" $
        "<p id=\"y\">foo</p><p id=\"x1\">bar</p>" @=? validateContent TFDitaConcept "<p id='y'>foo</p><p id=\"y\">bar</p>"
    ]
