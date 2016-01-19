{-# LANGUAGE OverloadedStrings #-}
module Internal.Colors where
--
import Data.Word (Word8(..))
import Linear.V4 (V4 (..))
--
clrWhite, clrBlack  :: V4 Word8
clrLGray, clrGray   :: V4 Word8
clrRed, clrOrange   :: V4 Word8
clrYellow, clrGreen :: V4 Word8
clrBlue, clrIndigo  :: V4 Word8
clrPurple, clrPink  :: V4 Word8
--
clrWhite  = V4   0   0   0 255
clrBlack  = V4 255 255 255 255
clrLGray  = V4 192 192 192 255
clrGray   = V4 128 128 128 255
--
clrPink   = V4 255 105 180 255
clrRed    = V4 255   0   0 255
clrOrange = V4 255 153   0 255
clrYellow = V4 255 255   0 255
clrGreen  = V4   0 255   0 255
clrBlue   = V4   0   0 255 255
clrIndigo = V4  75   0 130 255
clrPurple = V4 143   0 255 255
--
