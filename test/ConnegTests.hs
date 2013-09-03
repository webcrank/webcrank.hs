{-# LANGUAGE OverloadedStrings #-}

module ConnegTests where

import Test.Tasty
import Test.Tasty.HUnit

import Webcrank

connegTests = testGroup "conneg tests"
  [ chooseMediaTypeTests
--   , chooseCharsetTests
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

testMediaTypeChosen ps as m = testCase n test where
  n = show ps ++ " matches " ++ show m
  test = chooseMediaType ps as @?= Just m

testMediaTypeNotChosen ps as = testCase n test where
  n = show ps ++ " does not match " ++ show as
  test = chooseMediaType ps as @?= Nothing

chooseCharsetTests = testGroup "chooseCharset"
  [ testCase "write me!" undefined
  ]
