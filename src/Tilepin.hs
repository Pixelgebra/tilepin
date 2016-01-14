{-# LANGUAGE OverloadedStrings #-}
module Tilepin where
--
import qualified SDL
--
import Data.Word (Word8(..))
import Linear.Affine (Point(..))
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Foreign.C.Types (CInt)
--
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
--
import System.Environment (getArgs)
--
main :: IO ()
main = do
   args' <- getArgs
   let path = head $ args' ++ ["./tiles.bmp"]
   -- ***************************************** --
   sf <- SDL.loadBMP path
   (V2 gW gH)<- SDL.surfaceDimensions sf
   let dim = Just $ SDL.Rectangle (P $ V2 0 0) (V2 gW gH)
   -- ***************************************** --
   SDL.initialize [SDL.InitVideo]
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
   imgTx <- SDL.createTextureFromSurface renderer sf
   -- SDL.surfaceColorKey tempSf SDL.$=
   --    (Just (V4 223 113 38 maxBound))
   SDL.freeSurface sf
   -- ***************************************** --
   let
      loop = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
         SDL.rendererDrawColor renderer SDL.$=
            V4 minBound minBound maxBound maxBound
         SDL.clear renderer
         --
         SDL.copy renderer imgTx dim dim
         --
         SDL.present renderer
         threadDelay 50000
         unless quit loop
   loop
   SDL.destroyWindow window
   SDL.destroyRenderer renderer
   SDL.quit





-- .
