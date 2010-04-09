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
dotVel    = 200

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = mapRGB' surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

applySurface' :: MonadIO m => Int -> Int -> Surface -> Surface -> Maybe Rect -> m Bool
applySurface' x y src dst = liftIO . applySurface x y src dst

data Dot = Dot { pos :: (Float, Float), vel :: (Float, Float) }

defaultDot = Dot { pos=(0,0), vel=(0,0) }

handleInput :: Event -> Dot -> Dot
handleInput (KeyDown (Keysym SDLK_UP _ _)) dot@Dot { vel=(dx,dy) }    = dot { vel=(dx, dy - dotVel) }
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) dot@Dot { vel=(dx,dy) }  = dot { vel=(dx, dy + dotVel) }
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) dot@Dot { vel=(dx,dy) }  = dot { vel=(dx - dotVel, dy) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) dot@Dot { vel=(dx,dy) } = dot { vel=(dx + dotVel, dy) }

handleInput (KeyUp (Keysym SDLK_UP _ _)) dot@Dot { vel=(dx,dy) }    = dot { vel=(dx, dy + dotVel) }
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) dot@Dot { vel=(dx,dy) }  = dot { vel=(dx, dy - dotVel) }
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) dot@Dot { vel=(dx,dy) }  = dot { vel=(dx + dotVel, dy) }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) dot@Dot { vel=(dx,dy) } = dot { vel=(dx - dotVel, dy) }

handleInput _ d = d

move :: Word32 -> Dot -> Dot
move deltaTicks dot@Dot { pos=(x,y), vel=(dx,dy) } = dot { pos=(x'', y'') } 
 where
    x'  = x + (dx * (fromIntegral deltaTicks / 1000.0))
    y'  = y + (dy * (fromIntegral deltaTicks / 1000.0))
    x'' = if x' < 0 then 0 else if (x' + fromIntegral dotWidth) > fromIntegral screenWidth then fromIntegral $ screenWidth - dotWidth else x'
    y'' = if y' < 0 then 0 else if (y' + fromIntegral dotHeight) > fromIntegral screenHeight then fromIntegral $ screenHeight - dotHeight else y'

showDot Dot { pos=(x,y) } = applySurface (truncate x) (truncate y) 

data AppData = AppData {
    dot   :: Dot,
    delta :: Timer
}

data AppConfig = AppConfig {
    screen    :: Surface,
    dotSprite :: Surface
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getDelta :: MonadState AppData m => m Timer
getDelta = liftM delta get

putDelta :: MonadState AppData m => Timer -> m ()
putDelta t = modify $ \s -> s { delta = t }

modifyDeltaM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyDeltaM act = getDelta >>= act >>= putDelta

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
    
    dot <- loadImage "dot.bmp" Nothing--(Just (0x00, 0xff, 0xff))
    delta <- start defaultTimer
    return (AppConfig screen dot, AppData defaultDot delta) 

loop :: AppEnv ()
loop = do
    
    quit <- whileEvents $ modifyDot . handleInput
    
    delta <- liftIO . getTimerTicks =<< getDelta
    modifyDot $ move delta
    modifyDeltaM $ liftIO . start
    
    myDot     <- getDot
    dotSprite <- getDotSprite
    screen    <- getScreen    
    liftIO $ do
        jrect <- Just `liftM` getClipRect screen
        color <- mapRGB' screen 0xff 0xff 0xff
        
        fillRect screen jrect color 
        showDot myDot dotSprite screen Nothing
        
        Graphics.UI.SDL.flip screen
    
    unless quit loop

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