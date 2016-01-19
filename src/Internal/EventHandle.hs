{-# LANGUAGE OverloadedStrings #-}
module Internal.EventHandle where
--
import qualified SDL
--
import Data.Word (Word8(..))
import Data.Text (Text(..),pack,append)
import Data.Monoid (Last(..))
import Data.Foldable (foldMap)
import Data.Maybe (maybe,catMaybes)
import Foreign.C.Types (CInt(..))
--
import Internal.Types
-- ===============================
--
detectiveQuit :: [SDL.Event] -> Bool
detectiveQuit
   = any (== SDL.QuitEvent)
   . map SDL.eventPayload
--
detectiveMouseClick :: [SDL.Event] -> Maybe Pt
detectiveMouseClick
   = getLast
   . foldMap ((Last).clickPicker.SDL.eventPayload)
--
detectiveKeyPressed :: [SDL.Event] -> [SDL.Scancode]
detectiveKeyPressed
   = catMaybes
   . map (keypressPicker . SDL.eventPayload)
--
-- ===============================
clickPicker :: SDL.EventPayload -> Maybe Pt
clickPicker (SDL.MouseButtonEvent mmed)
   | SDL.mouseButtonEventMotion mmed == SDL.Released
      = Just $ fmap CInt $ SDL.mouseButtonEventPos mmed
   | SDL.mouseButtonEventMotion mmed == SDL.Pressed
      =Nothing
clickPicker _ = Nothing
--
keypressPicker :: SDL.EventPayload -> Maybe SDL.Scancode
keypressPicker (SDL.KeyboardEvent kbed) =
   case SDL.keyboardEventKeyMotion kbed of
      SDL.Released -> Just $
         SDL.keysymScancode $ SDL.keyboardEventKeysym kbed
      SDL.Pressed  -> Nothing
keypressPicker _ = Nothing
--
kbeHandler :: SDL.Renderer -> SDL.Scancode -> IO ()
kbeHandler rdr SDL.ScancodeEscape = do
   print SDL.ScancodeEscape
kbeHandler rdr SDL.ScancodeReturn = do
   print SDL.ScancodeF
kbeHandler rdr SDL.ScancodeF = do
   print SDL.ScancodeF
kbeHandler rdr _ = return ()
