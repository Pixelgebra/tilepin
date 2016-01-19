{-# LANGUAGE OverloadedStrings #-}
module Internal.Types where
--
import qualified SDL
--
import Data.Word       (Word8(..))
import Linear.V2       (V2 (..))
import Linear.V3       (V3 (..))
import Linear.V4       (V4 (..))
import Linear.Affine   (Point(..))
import Foreign.C.Types (CInt(..))
-- ===============================
-- type alias for x-y location
type Pt = Point V2 CInt
-- ===============================
-- type alias for width-height
type WH = V2 CInt
