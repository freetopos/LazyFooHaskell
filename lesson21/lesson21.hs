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

dotWidth  = 20
dotHeight = 20

levelWidth  = 1280
levelHeight = 960

defaultDot = Dot (0,0) (0,0)

halfDotWidth  = dotWidth `div` 2
halfDotHeight = dotHeight `div` 2

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data Dot = Dot { pos :: (Int, Int), vel :: (Int, Int) }

handleInput :: Event -> Dot -> Dot
handleInput (KeyDown (Keysym SDLK_UP _ _)) d@Dot { vel=(dx,dy) }    = d { vel=(dx, dy - halfDotHeight) }
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx, dy + halfDotHeight) }
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx - halfDotWidth, dy) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) d@Dot { vel=(dx,dy) } = d { vel=(dx + halfDotWidth, dy) }

handleInput (KeyUp (Keysym SDLK_UP _ _)) d@Dot { vel=(dx,dy) }    = d { vel=(dx, dy + halfDotHeight) }
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx, dy - halfDotHeight) }
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx + halfDotWidth, dy) }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) d@Dot { vel=(dx,dy) } = d { vel=(dx - halfDotWidth, dy) }

handleInput _ d = d

move :: Dot -> Dot
move d@Dot { pos=(x,y), vel=(dx,dy) } = d { pos=(x'', y'') }
 where
    (x', y') = (x + dx, y + dy)
    	
    x'' = if x' < 0 || (x' + dotWidth) > levelWidth then x else x'
    
    y'' = if y' < 0 || (y' + dotHeight) > levelHeight then y else y'

type Camera = Rect

setCamera :: Dot -> Camera -> Camera
setCamera Dot { pos=(x,y) } rect@(Rect _ _ w h) = rect { rectX=x'', rectY=y'' }
 where
    x'  = (x + dotWidth `div` 2) - screenWidth `div` 2
    y'  = (y + dotWidth `div` 2) - screenHeight `div` 2
    x'' = min (levelWidth - w) $ max x' 0
    y'' = min (levelHeight - h) $ max y' 0 

showDot Dot { pos=(x,y) } (Rect cx cy _ _) = applySurface (x - cx) (y - cy)

data AppData = AppData {
    dot    :: Dot,
    camera :: Camera,
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

getDot :: MonadState AppData m => m Dot
getDot = liftM dot get

putDot :: MonadState AppData m => Dot -> m ()
putDot t = modify $ \s -> s { dot = t }

modifyDotM :: MonadState AppData m => (Dot -> m Dot) -> m ()
modifyDotM act = getDot >>= act >>= putDot

modifyDot :: MonadState AppData m => (Dot -> Dot) -> m ()
modifyDot fn = fn `liftM` getDot >>= putDot

getCamera :: MonadState AppData m => m Camera
getCamera = liftM camera get

putCamera :: MonadState AppData m => Camera -> m ()
putCamera t = modify $ \s -> s { camera = t }

modifyCameraM :: MonadState AppData m => (Camera -> m Camera) -> m ()
modifyCameraM act = getCamera >>= act >>= putCamera

modifyCamera :: MonadState AppData m => (Camera -> Camera) -> m ()
modifyCamera fn = fn `liftM` getCamera >>= putCamera

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getBg :: MonadReader AppConfig m => m Surface
getBg = liftM background ask

getDotSprite :: MonadReader AppConfig m => m Surface
getDotSprite = liftM dotSprite ask

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Move the Dot" []
    
    background <- loadImage "bg.png" Nothing
    dot        <- loadImage "dot.bmp" (Just (0x00, 0xff, 0xff))
    fps        <- start defaultTimer
    
    return (AppConfig screen background dot, AppData myDot camera fps) 
 where
    myDot  = defaultDot
    camera = Rect 0 0 screenWidth screenHeight

loop :: AppEnv ()
loop = do
    
    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifyDot . handleInput
    
    modifyDot $ move
    
    myDot <- getDot
    modifyCamera $ setCamera myDot 
    
    fps    <- getFPS
    camera <- getCamera
    dot    <- getDotSprite
    screen <- getScreen
    background <- getBg
    
    liftIO $ do
        applySurface 0 0 background screen $ Just camera
        
        showDot myDot camera dot screen Nothing
        
        Graphics.UI.SDL.flip screen
        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
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

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    (env, state) <- initEnv
    runLoop env state
