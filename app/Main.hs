{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-cmm -ddump-asm #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Int
import Data.Word
import Numeric.QuoteQuot

as1 :: Int8 -> Int8
as1 = $$(quoteQuot (7 :: Int8))
as2 :: Int8 -> Int8
as2 = $$(quoteQuot (14 :: Int8))
bs2 :: Word8 -> Word8
bs2 = $$(quoteQuot (7 :: Word8))

main = pure ()
