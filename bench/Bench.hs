-- |
-- Copyright:   (c) 2020-2022 Andrew Lelechenko
-- Licence:     BSD3
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Main (main) where

import GHC.Exts
import Numeric.QuoteQuot
import Test.Tasty.Bench
import GHC.Int
import GHC.Word

measureWord :: String -> (Word -> Word) -> Benchmark
measureWord name f = bench name $
  whnf (\(W# n#) -> W# (go 0## n#)) 100000000
  where
    go acc# 0## = acc#
    go acc# k# = go (let !(W# fk) = f (W# k#) in acc# `plusWord#` fk) (k# `minusWord#` 1##)
{-# INLINE measureWord #-}

measureInt :: String -> (Int -> Int) -> Benchmark
measureInt name f = bench name $
  whnf (\(I# n#) -> I# (go 0# n#)) 100000000
  where
    go acc# 0# = acc#
    go acc# k# = go (let !(I# fk) = f (I# k#) in acc# +# fk) (k# -# 1#)
{-# INLINE measureInt #-}

measureWord8 :: String -> (Word8 -> Word8) -> Benchmark
measureWord8 name f = bench name $
  whnf (\(W# n#) -> fromIntegral @_ @Int $ W8# (go (wordToWord8# 0##) n#)) 100000000
  where
    go acc# 0## = acc#
    go acc# k# = go (let !(W8# fk) = f (W8# (wordToWord8# k#)) in acc# `plusWord8#` fk) (k# `minusWord#` 1##)
{-# INLINE measureWord8 #-}

measureInt8 :: String -> (Int8 -> Int8) -> Benchmark
measureInt8 name f = bench name $
  whnf (\(I# n#) -> fromIntegral @_ @Int $ I8# (go (intToInt8# 0#) n#)) 100000000
  where
    go acc# 0# = acc#
    go acc# k# = go (let !(I8# fk) = f (I8# (intToInt8# k#)) in acc# `plusInt8#` fk) (k# -# 1#)
{-# INLINE measureInt8 #-}

measureWord16 :: String -> (Word16 -> Word16) -> Benchmark
measureWord16 name f = bench name $
  whnf (\(W# n#) -> fromIntegral @_ @Int $ W16# (go (wordToWord16# 0##) n#)) 100000000
  where
    go acc# 0## = acc#
    go acc# k# = go (let !(W16# fk) = f (W16# (wordToWord16# k#)) in acc# `plusWord16#` fk) (k# `minusWord#` 1##)
{-# INLINE measureWord16 #-}

measureInt16 :: String -> (Int16 -> Int16) -> Benchmark
measureInt16 name f = bench name $
  whnf (\(I# n#) -> fromIntegral @_ @Int $ I16# (go (intToInt16# 0#) n#)) 100000000
  where
    go acc# 0# = acc#
    go acc# k# = go (let !(I16# fk) = f (I16# (intToInt16# k#)) in acc# `plusInt16#` fk) (k# -# 1#)
{-# INLINE measureInt16 #-}

measureWord32 :: String -> (Word32 -> Word32) -> Benchmark
measureWord32 name f = bench name $
  whnf (\(W# n#) -> fromIntegral @_ @Int $ W32# (go (wordToWord32# 0##) n#)) 100000000
  where
    go acc# 0## = acc#
    go acc# k# = go (let !(W32# fk) = f (W32# (wordToWord32# k#)) in acc# `plusWord32#` fk) (k# `minusWord#` 1##)
{-# INLINE measureWord32 #-}

measureInt32 :: String -> (Int32 -> Int32) -> Benchmark
measureInt32 name f = bench name $
  whnf (\(I# n#) -> fromIntegral @_ @Int $ I32# (go (intToInt32# 0#) n#)) 100000000
  where
    go acc# 0# = acc#
    go acc# k# = go (let !(I32# fk) = f (I32# (intToInt32# k#)) in acc# `plusInt32#` fk) (k# -# 1#)
{-# INLINE measureInt32 #-}

#define benchWord(n) \
  bgroup (show (n :: Word)) \
    [ measureWord "quot" (`quot` (n :: Word)) \
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Word) ++ "\" && $(NF-2) == \"Word\"") \
    $ measureWord "quoteQuot" $$(quoteQuot (n :: Word)) \
    ]

#define benchInt(n) \
  bgroup (show (n :: Int)) \
    [ measureInt "quot" (`quot` (n :: Int)) \
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Int) ++ "\" && $(NF-2) == \"Int\"") \
    $ measureInt "quoteQuot" $$(quoteQuot (n :: Int)) \
    ]

#define benchWord8(n) \
  bgroup (show (n :: Word8)) \
    [ measureWord8 "quot" (`quot` (n :: Word8)) \
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Word8) ++ "\" && $(NF-2) == \"Word8\"") \
    $ measureWord8 "quoteQuot" $$(quoteQuot (n :: Word8)) \
    ]

#define benchInt8(n) \
  bgroup (show (n :: Int8)) \
    [ measureInt8 "quot" (`quot` (n :: Int8)) \
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Int8) ++ "\" && $(NF-2) == \"Int8\"") \
    $ measureInt8 "quoteQuot" $$(quoteQuot (n :: Int8)) \
    ]

#define benchWord16(n) \
  bgroup (show (n :: Word16)) \
    [ measureWord16 "quot" (`quot` (n :: Word16)) \
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Word16) ++ "\" && $(NF-2) == \"Word16\"") \
    $ measureWord16 "quoteQuot" $$(quoteQuot (n :: Word16)) \
    ]

#define benchInt16(n) \
  bgroup (show (n :: Int16)) \
    [ measureInt16 "quot" (`quot` (n :: Int16)) \
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Int16) ++ "\" && $(NF-2) == \"Int16\"") \
    $ measureInt16 "quoteQuot" $$(quoteQuot (n :: Int16)) \
    ]

#define benchWord32(n) \
  bgroup (show (n :: Word32)) \
    [ measureWord32 "quot" (`quot` (n :: Word32)) \
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Word32) ++ "\" && $(NF-2) == \"Word32\"") \
    $ measureWord32 "quoteQuot" $$(quoteQuot (n :: Word32)) \
    ]

#define benchInt32(n) \
  bgroup (show (n :: Int32)) \
    [ measureInt32 "quot" (`quot` (n :: Int32)) \
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Int32) ++ "\" && $(NF-2) == \"Int32\"") \
    $ measureInt32 "quoteQuot" $$(quoteQuot (n :: Int32)) \
    ]

main :: IO ()
main = defaultMain
  [ bgroup "Word"
    [ benchWord(3)
    , benchWord(5)
    , benchWord(7)
    , benchWord(14)
    , benchWord(10000)
    ]
  , bgroup "Int"
    [ benchInt(3)
    , benchInt(5)
    , benchInt(7)
    , benchInt(14)
    , benchInt(10000)
    ]
  , bgroup "Word8"
    [ benchWord8(3)
    , benchWord8(5)
    , benchWord8(7)
    , benchWord8(14)
    ]
  , bgroup "Int8"
    [ benchInt8(3)
    , benchInt8(5)
    , benchInt8(7)
    , benchInt8(14)
    ]
  , bgroup "Word16"
    [ benchWord16(3)
    , benchWord16(5)
    , benchWord16(7)
    , benchWord16(14)
    , benchWord16(10000)
    ]
  , bgroup "Int16"
    [ benchInt16(3)
    , benchInt16(5)
    , benchInt16(7)
    , benchInt16(14)
    , benchInt16(10000)
    ]
  , bgroup "Word32"
    [ benchWord32(3)
    , benchWord32(5)
    , benchWord32(7)
    , benchWord32(14)
    , benchWord32(10000)
    ]
  , bgroup "Int32"
    [ benchInt32(3)
    , benchInt32(5)
    , benchInt32(7)
    , benchInt32(14)
    , benchInt32(10000)
    ]
  ]
