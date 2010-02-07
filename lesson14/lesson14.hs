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

import Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.TTF.General as TTFG

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

data FrameRateTest = FrameRateTest {
    fps   :: Timer,
    frame :: Int,
    cap   :: Bool
}

data AppConfig = AppConfig {
    screen     :: Surface,
    background :: Surface,
    message    :: Surface
}

type AppState = StateT FrameRateTest IO
type AppEnv = ReaderT AppConfig AppState

getFrame :: MonadState FrameRateTest m => m Int
getFrame = liftM frame get

getCap :: MonadState FrameRateTest m => m Bool
getCap = liftM cap get

getFPS :: MonadState FrameRateTest m => m Timer
getFPS = liftM fps get

putFPS :: MonadState FrameRateTest m => Timer -> m ()
putFPS timer = modify $ \s -> s { fps = timer }

putFrame :: MonadState FrameRateTest m => Int -> m ()
putFrame frm = modify $ \s -> s { frame = frm }

modifyFPS :: MonadState FrameRateTest m => (Timer -> m Timer) -> m ()
modifyFPS act = getFPS >>= act >>= putFPS

-- This commented section is meant to be a more general way of implemenating modifyFPS
----withFPS :: (MonadState FrameRateTest m, MonadIO m) => StateT Timer IO a -> m a
--withFPS :: StateT Timer IO a -> AppEnv a
--withFPS act = do
--    state <- get
--    (out, res) <- liftIO $ runStateT act $ fps state
--    put $ state { fps=res }
--    return out
--
--modify' :: MonadState s m => (s -> m s) -> m ()
--modify' act = do
--    s <- get
--    s' <- act s
--    put s'

initEnv :: IO (AppConfig, FrameRateTest)
initEnv = do

    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Frame Rate Test" []
    
    background <- loadImage "background.png" Nothing--(Just (0x00, 0xff, 0xff))
    font       <- openFont "lazy.ttf" 50
    message    <- renderTextSolid font "Testing Frame Rate" textColor
    
    myTimer <- start defaultTimer
    
    return (AppConfig screen background message, FrameRateTest myTimer frame cap) 
 where
    cap   = True
    frame = 0
    textColor = Color 0 0 0

loop :: AppEnv ()
loop = do

--    withFPS $ modify' (liftIO . start)
    modifyFPS $ liftIO . start
    quit <- whileEvents $ \event -> do
                case event of
                    KeyDown (Keysym SDLK_RETURN _ _) -> modify toggleCap
                    _ -> return ()
    
    AppConfig screen background message <- ask    
    frame <- getFrame
    let x = (screenWidth - (surfaceGetWidth message)) `div` 2
    let y = ((screenHeight + (surfaceGetHeight message) * 2) `div` framesPerSecond) * (frame `mod` framesPerSecond) - (surfaceGetHeight message)
    applySurface' 0 0 background screen Nothing
    applySurface' x y message screen Nothing
    
    liftIO $ Graphics.UI.SDL.flip screen
    
    putFrame $ frame + 1
    
    fps <- getFPS
    cap <- getCap
    -- delay if cap is on and we have time.
    liftIO $ do
        ticks <- getTimerTicks fps
        when (cap && ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks
    
    unless quit loop
 where
    secsPerFrame = fromIntegral $ 1000 `div` framesPerSecond

    applySurface' x y src dst clip = liftIO (applySurface x y src dst clip)

    toggleCap :: FrameRateTest -> FrameRateTest
    toggleCap frt = frt { cap = not $ cap frt }

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
    result <- TTFG.init
    if not result
        then putStr "Failed to init ttf\n"
        else do
            (env, state) <- initEnv
            runLoop env state
            TTFG.quit