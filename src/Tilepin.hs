{-# LANGUAGE OverloadedStrings #-}
module Tilepin where
--
import qualified SDL
--
import Data.Word (Word8(..))
import Data.Text (Text(..),pack,append)
import Data.Monoid (Last(..))
import Data.Foldable (foldMap)
import Data.Maybe (maybe)
--
import Foreign.C.Types (CInt(..))
--
import Linear.Affine (Point(..))
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
--
import Control.Concurrent (threadDelay)
import Control.Monad (unless,forM_)
--
import System.Environment (getArgs)
--
main :: IO ()
main = do
   args' <- getArgs
   let path = head $ args' ++ ["./tiles.bmp"]
   -- ***************************************** --
   {- operating image loading and parameters setup -}
   -- ----------------------------------------- --
   sf <- SDL.loadBMP path
   (V2 gW gH) <- SDL.surfaceDimensions sf
   let dim = Just $ SDL.Rectangle (P $ V2 0 0) (V2 gW gH)
   let winWH@(V2 winW winH) = (V2 gW gH)
   let transpDelta = 30
   -- ***************************************** --
   {- SDL and window initialization -}
   -- ----------------------------------------- --
   SDL.initialize [SDL.InitVideo]
   SDL.setMouseLocationMode SDL.AbsoluteLocation
   window <- SDL.createWindow "TIlepin"
      SDL.defaultWindow
         { SDL.windowPosition = SDL.Centered
         , SDL.windowInitialSize = V2 gW gH}
   renderer <- SDL.createRenderer window (-1)
      SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedVSyncRenderer
         , SDL.rendererTargetTexture = True}
   SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
   SDL.showWindow window
   -- ***************************************** --
   {- applying color key, and,
      generating working texture -}
   -- ----------------------------------------- --
   SDL.surfaceColorKey sf SDL.$=
      Just (V4 maxBound maxBound maxBound maxBound)
   imgTx <- SDL.createTextureFromSurface renderer sf
   SDL.freeSurface sf
   -- ***************************************** --
   -- ----------------------------------------- --
   let loop cutPs = do
         -- *********************************** --
         events <- SDL.pollEvents
         -- quit event detecting
         let quit = any (== SDL.QuitEvent) $
               map SDL.eventPayload events
         -- mouse click(released) event detecting
         let mousePressedXY = getLast $
               foldMap ((Last).clickPicker.SDL.eventPayload) events
         -- update current cutting points
         let newCutPs =
               maybe cutPs (:cutPs) mousePressedXY
         -- get current mouse location
         mouseXY <- SDL.getAbsoluteMouseLocation
         -- *********************************** --
         SDL.rendererDrawColor renderer SDL.$=
            V4 180 180 180 maxBound
         SDL.clear renderer
         --
         SDL.copy renderer imgTx dim dim
         -- update window title
         SDL.windowTitle window SDL.$=
            append "Tilepin - " ((pack $ show mouseXY))
         -- draw dynamic aux line
         drawAuxLines renderer winWH mouseXY
         -- draw all cutting points so far we have
         forM_ newCutPs $ drawAuxLines renderer winWH
         --
         SDL.present renderer
         threadDelay 50000
         unless quit $ loop newCutPs
   loop []
   SDL.destroyWindow window
   SDL.destroyRenderer renderer
   SDL.quit
--
drawAuxLines :: SDL.Renderer
             -> V2 CInt -- ^ window W/H
             -> Point V2 CInt -- ^ mouse X/Y
             -> IO ()
drawAuxLines rdr (V2 winW winH) (P (V2 mouseX mouseY)) = do
   SDL.rendererDrawColor rdr SDL.$=
      V4 minBound minBound minBound maxBound
   SDL.drawLine rdr (P$V2 0 mouseY) (P$V2 winW mouseY)
   --
   SDL.rendererDrawColor rdr SDL.$=
      V4 maxBound maxBound maxBound maxBound
   SDL.drawLine rdr (P$V2 mouseX 0) (P$V2 mouseX winH)
--
clickPicker :: SDL.EventPayload -> Maybe (Point V2 CInt)
clickPicker (SDL.MouseButtonEvent mmed) =
   case SDL.mouseButtonEventMotion mmed of
      SDL.Released -> Just $ fmap CInt $ SDL.mouseButtonEventPos mmed
      SDL.Pressed  -> Nothing
clickPicker _ = Nothing
-- .
