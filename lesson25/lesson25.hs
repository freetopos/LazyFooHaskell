{-
    The MIT License
    Copyright (c) 2010 Korcan Hussein

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

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

dotWidth  = 20
dotHeight = 20

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

defaultDot = Dot { pos=(0,0), vel=(0,0) }

handleInput :: Event -> Dot -> Dot

handleInput (JoyAxisMotion 0 0 value) dot@Dot { vel=(dx,dy) } = dot { vel=(xVel, dy) }
 where xVel = if value > -8000 && value < 8000 then 0
                else if value < 0 then -halfDotWidth else halfDotWidth

handleInput (JoyAxisMotion 0 1 value) dot@Dot { vel=(dx,dy) } = dot { vel=(dx, yVel) }
 where yVel = if value > -8000 && value < 8000 then 0
                else if value < 0 then -halfDotHeight else halfDotHeight

handleInput _ d = d

move :: Dot -> Dot
move dot@Dot { pos=(x,y), vel=(dx,dy) } = dot { pos=(x'', y'') } 
 where
    x'  =   x + dx
    y'  =   y + dy
    x'' =   if x' < 0 || (x' + dotWidth) > screenWidth then x else x' 
    y'' =   if y' < 0 || (y' + dotHeight) > screenHeight then y else y'

showDot Dot { pos=(x,y) } = applySurface x y 

data AppData = AppData {
    dot :: Dot,
    fps :: Timer
}

data AppConfig = AppConfig {
    screen    :: Surface,
    dotSprite :: Surface,
    stick     :: Joystick  
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

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getDotSprite :: MonadReader AppConfig m => m Surface
getDotSprite = liftM dotSprite ask

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Move the Dot" []
    
    dot <- loadImage "dot.bmp" Nothing
    fps <- start defaultTimer
    
    stick <- open joyIndex -- will throw an exception if no joysticks exists.
    
    return (AppConfig screen dot stick, AppData defaultDot fps) 
 where joyIndex = 0

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifyDot . handleInput
    
    modifyDot move
    
    fps       <- getFPS
    myDot     <- getDot
    dotSprite <- getDotSprite
    screen    <- getScreen    
    liftIO $ do
        jrect <- Just `liftM` getClipRect screen
        color <- mapRGB' screen 0xff 0xff 0xff
        
        fillRect screen jrect color 
        showDot myDot dotSprite screen Nothing
        
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

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
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
    close $ stick env
