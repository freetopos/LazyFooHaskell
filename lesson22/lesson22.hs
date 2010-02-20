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

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL

import Graphics.UI.SDL.Image

import Timer

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data AppData = AppData {
    bgX    :: Int,
    fps    :: Timer
}

data AppConfig = AppConfig {
    screen     :: Surface,
    background :: Surface,
    dotSprite  :: Surface
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { fps = t }

modifyFPSM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPSM act = getFPS >>= act >>= putFPS

getBgX :: MonadState AppData m => m Int
getBgX = liftM bgX get

putBgX :: MonadState AppData m => Int -> m ()
putBgX t = modify $ \s -> s { bgX = t }

modifyBgX :: MonadState AppData m => (Int -> Int) -> m ()
modifyBgX fn = fn `liftM` getBgX >>= putBgX

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getBg :: MonadReader AppConfig m => m Surface
getBg = liftM background ask

getDot :: MonadReader AppConfig m => m Surface
getDot = liftM dotSprite ask

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Background Test" []
    
    background <- loadImage "bg.png" Nothing
    dot <- loadImage "dot.bmp" (Just (0x00, 0xff, 0xff))
    fps <- start defaultTimer
    
    return (AppConfig screen background dot, AppData 0 fps) 

loop :: AppEnv ()
loop = do
    
    modifyFPSM $ liftIO . start
    quit <- whileEvents $ \_ -> return ()
    
    background <- getBg
    let backgroundW = surfaceGetWidth background
    
    modifyBgX $ Prelude.flip (-) 2
    modifyBgX $ \bgX ->
        if bgX <= -backgroundW then 0 else bgX
    
    bgX    <- getBgX
    dot    <- getDot
    screen <- getScreen
    fps    <- getFPS
    
    liftIO $ do
        applySurface bgX bgY background screen Nothing
        applySurface (bgX + backgroundW) bgY background screen Nothing
        applySurface 310 230 dot screen Nothing
                
        Graphics.UI.SDL.flip screen
        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
    bgY             = 0
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond
    mapRGB'         = mapRGB . surfaceGetPixelFormat
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

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.result <- TTFG.init
    (env, state) <- initEnv
    runLoop env state