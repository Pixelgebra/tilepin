{-# LANGUAGE OverloadedStrings #-}
module Internal.Render where
--
import qualified SDL
--
import Data.Word (Word8(..))
import Data.Text (Text(..),pack,append)
import Data.Monoid (Last(..))
import Data.Foldable (foldMap)
import Data.Maybe (maybe,catMaybes)
import Linear.Affine (Point(..))
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
--
import Internal.Types
import Internal.Colors
--
-- ===============================
-- draw auxiliary line for current mouse cursor
drawAuxLines :: SDL.Renderer
             -> WH -- ^ window W/H
             -> Pt -- ^ mouse X/Y
             -> IO ()
drawAuxLines rdr (V2 winW winH) (P (V2 mouseX mouseY)) = do
   SDL.rendererDrawColor rdr SDL.$= clrWhite
   SDL.drawLine rdr (P$V2 0 mouseY) (P$V2 winW mouseY)
   SDL.rendererDrawColor rdr SDL.$= clrBlack
   SDL.drawLine rdr (P$V2 mouseX 0) (P$V2 mouseX winH)
