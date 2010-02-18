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

import Data.List
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

dotAccel = 1

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

newtype RectArray = RectArray { rectArray ::  [Rect] }

data Dot = Dot { pos :: (Int, Int), box :: RectArray, vel :: (Int, Int) }

class Collidable a where
    intersects :: a -> a -> Bool

instance Collidable Rect where
    --intersects :: Rect -> Rect -> Bool
    intersects Rect {rectX=ax,rectY=ay,rectW=aw,rectH=ah } Rect {rectX=bx,rectY=by,rectW=bw,rectH=bh } =
        bottomA > topB && topA < bottomB && rightA > leftB && leftA < rightB
     where
        leftA   = ax
        rightA  = ax + aw
        topA    = ay
        bottomA = ay + ah
        
        leftB   = bx
        rightB  = bx + bw
        topB    = by
        bottomB = by + bh

instance Collidable RectArray where
    --intersects :: RectArray -> RectArray -> Bool
    intersects (RectArray lhs) (RectArray rhs) =  any (\x -> any (intersects x) rhs) lhs --Prelude.flip any lhs $ Prelude.flip any rhs . intersects 

shiftBoxes :: Dot -> Dot
shiftBoxes d@Dot { pos=(x,y), box=RectArray rs } = d { box=RectArray $ snd $ mapAccumL shiftBox 0 rs }
 where shiftBox r (Rect _ _ bw bh) = (r + bh, Rect (x + ((dotWidth - bw) `div` 2)) (y + r) bw bh)

makeDot x y = shiftBoxes $ Dot (x,y) newBox (0,0)
 where newBox = RectArray [Rect 0 0 6 1,  Rect 0 0 10 1, Rect 0 0 14 1, Rect 0 0 16 2, Rect 0 0 18 2, Rect 0 0 20 6,
                           Rect 0 0 18 2, Rect 0 0 16 2, Rect 0 0 14 1, Rect 0 0 10 1, Rect 0 0 6 1]

handleInput :: Event -> Dot -> Dot
handleInput (KeyDown (Keysym SDLK_UP _ _)) d@Dot { vel=(dx,dy) }    = d { vel=(dx, dy - dotAccel) }
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx, dy + dotAccel) }
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx - dotAccel, dy) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) d@Dot { vel=(dx,dy) } = d { vel=(dx + dotAccel, dy) }

handleInput (KeyUp (Keysym SDLK_UP _ _)) d@Dot { vel=(dx,dy) }    = d { vel=(dx, dy + dotAccel) }
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx, dy - dotAccel) }
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx + dotAccel, dy) }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) d@Dot { vel=(dx,dy) } = d { vel=(dx - dotAccel, dy) }

handleInput _ d = d

move :: RectArray -> Dot -> Dot
move rs d@Dot { box=b, vel=(dx,dy), pos=(x,y) } = d { box=box'', pos=(x'', y'') }
 where
    (x', y')    = (x + dx, y + dy)
    
    rs'         = box $ shiftBoxes $ d { pos=(x',y) }
    (x'', box') = if x' < 0 || (x' + dotWidth) > screenWidth || intersects rs' rs then (x, b) else (x', rs')
    
    rs''         = box $ shiftBoxes $ d { pos=(x'',y') } 
    (y'', box'') = if y' < 0 || (y' + dotHeight) > screenHeight || intersects rs'' rs then (y, box') else (y', rs'')

showDot Dot { pos=(x,y) } = applySurface x y 

data AppData = AppData {
    dot :: Dot,
    fps :: Timer
}

data AppConfig = AppConfig {
    screen    :: Surface,
    dotSprite :: Surface
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
    
    dot <- loadImage "dot.bmp" $ Just (0x00, 0xff, 0xff)
    
    fps <- start defaultTimer
        
    return (AppConfig screen dot, AppData myDot fps) 
 where myDot = makeDot 0 0

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifyDot . handleInput
    
    modifyDot $ move $ box otherDot
    
    fps       <- getFPS
    myDot     <- getDot
    dotSprite <- getDotSprite
    screen    <- getScreen    
    liftIO $ do
        jrect <- Just `liftM` getClipRect screen
        white <- mapRGB' screen 0xff 0xff 0xff
        blue  <- mapRGB' screen 0x00 0x00 0xff
        
        fillRect screen jrect white
        
        showDot myDot dotSprite screen Nothing
        showDot otherDot dotSprite screen Nothing
        
        Graphics.UI.SDL.flip screen
        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
    otherDot        = makeDot 20 20
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