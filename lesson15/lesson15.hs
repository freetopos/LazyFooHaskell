{-
	The MIT License
	Copyright (c) 2009 Korcan Hussein

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in
	all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
	THE SOFTWARE.
-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Timer

screenWidth     = 640
screenHeight    = 480
screenBpp       = 32
framesPerSecond = 20

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

type TimerState a = StateT Timer IO a
data FrameRateTest = FrameRateTest {
    fps         :: Timer,
    updateTimer :: Timer,
    frame       :: Int
}

data AppConfig = AppConfig {
    screen :: Surface,
    image  :: Surface
}

type AppState = StateT FrameRateTest IO
type AppEnv = ReaderT AppConfig AppState

getFrame :: MonadState FrameRateTest m => m Int
getFrame = liftM frame get

putFrame :: MonadState FrameRateTest m => Int -> m ()
putFrame frm = modify $ \s -> s { frame = frm }

getFPS :: MonadState FrameRateTest m => m Timer
getFPS = liftM fps get

putFPS :: MonadState FrameRateTest m => Timer -> m ()
putFPS timer = modify $ \s -> s { fps = timer }

modifyFPS :: MonadState FrameRateTest m => (Timer -> m Timer) -> m ()
modifyFPS act = getFPS >>= act >>= putFPS

getUpdate :: MonadState FrameRateTest m => m Timer
getUpdate = liftM updateTimer get

putUpdate :: MonadState FrameRateTest m => Timer -> m ()
putUpdate t = modify $ \s -> s { updateTimer = t }

modifyUpdate :: MonadState FrameRateTest m => (Timer -> m Timer) -> m ()
modifyUpdate act = getUpdate >>= act >>= putUpdate

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getImage :: MonadReader AppConfig m => m Surface
getImage = liftM image ask

initEnv :: IO (AppConfig, FrameRateTest)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Frame Rate Test" []
    
    image <- loadImage "testing.png" Nothing--(Just (0x00, 0xff, 0xff))

    fps    <- start defaultTimer
    update <- start defaultTimer
    return (AppConfig screen image, FrameRateTest fps update frame) 
 where
    frame = 0

loop :: AppEnv ()
loop = do
    quit <- whileEvents $ \_ -> return ()
    
    image  <- getImage
    screen <- getScreen
    applySurface' 0 0 image screen Nothing
    
    liftIO $ Graphics.UI.SDL.flip screen
    
    frame <- fmap (+1) getFrame
    putFrame frame    
    ticks  <- liftIO . getTimerTicks =<< getUpdate
    when (ticks > 1000) $ do
        fps <- getFPS
        liftIO $ do
            avgPerSec <- ((fromIntegral frame /) . (/ 1000.0) . fromIntegral) `fmap` getTimerTicks fps
            let caption = "Average Frames Per Second: " ++ show avgPerSec
            setCaption caption []
        modifyUpdate $ liftIO . start
    
    unless quit loop
 where
    applySurface' x y src dst clip = liftIO (applySurface x y src dst clip)

whileEvents :: (MonadIO m) => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> FrameRateTest -> IO ()
runLoop = evalStateT . runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.    		
    (env, state) <- initEnv
    runLoop env state