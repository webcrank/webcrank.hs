{-# LANGUAGE OverloadedStrings #-}

module ConnegTests where

import Test.Tasty
import Test.Tasty.HUnit

import Webcrank

connegTests = testGroup "conneg tests"
  [ chooseMediaTypeTests
  , chooseConnegTests
  ]

chooseMediaTypeTests = testGroup "chooseMediaType"
  [ testMediaTypeChosen [textHtml] [MediaType "*" "*" []] textHtml
  , testMediaTypeChosen [textHtml] [MediaType "text" "*" []] textHtml
  , testMediaTypeChosen [textHtml] [textHtml] textHtml
  , testMediaTypeNotChosen [textHtml] [MediaType "foo" "*" []]
  , testMediaTypeNotChosen [textHtml] [textXml]
  , testMediaTypeNotChosen [textHtml] [textXml]
  , testMediaTypeChosen [textHtml, imageJpeg] [imageJpeg, textHtml] imageJpeg
  , testMediaTypeChosen [textHtml, imageJpeg] [MediaType "image" "*" [], MediaType "text" "*" []] imageJpeg
  , testMediaTypeChosen [textHtml, imageJpeg] [MediaType "image" "*" [], MediaType "image" "png" []] imageJpeg
  ]

testMediaTypeChosen ps as m = testCase n t where
  n = show ps ++ " matches " ++ show m
  t = chooseMediaType ps as @?= Just m

testMediaTypeNotChosen ps as = testCase n t where
  n = show ps ++ " does not match " ++ show as
  t = chooseMediaType ps as @?= Nothing

chooseConnegTests = testGroup "chooseConneg"
  [ testChooseConneg "identity" ["identity"] [("*", 1.0)] (Just "identity")
  , testChooseConneg "identity" ["gzip", "compress"] [("gzip", 1.0)] (Just "gzip")
  , testChooseConneg "identity" ["gzip", "compress"] [("compress", 1.0), ("gzip", 1.0)] (Just "compress")
  , testChooseConneg "identity" ["gzip", "compress"] [("*", 1.0), ("gzip", 0.5)] (Just "gzip")
  , testChooseConneg "identity" ["identity", "gzip"] [("compress", 1.0), ("*", 0.5)] (Just "identity")
  , testChooseConneg "identity" ["identity"] [("compress", 1.0), ("gzip", 1.0)] Nothing
  , testChooseConneg "identity" ["identity", "gzip"] [("*", 0.0)] Nothing
  , testChooseConneg "identity" ["identity", "gzip"] [("compress", 1.0), ("*", 0.0)] Nothing
  ]

testChooseConneg d ps as m = testCase n t where
  n = show d ++ ":" ++ show ps ++ maybe " does not match " (const " matches ") m ++ show as
  t = chooseConneg d ps as @?= m
