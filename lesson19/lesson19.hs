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
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module Main where

import Data.List
import Data.Word

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL

import Graphics.UI.SDL.Image

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render

import Timer

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

dotWidth  = 20
dotHeight = 20

dotWidthHalf  = dotWidth `div` 2
dotHeightHalf = dotHeight `div` 2

dotAccel = 1

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data Circle = Circle { pos :: (Int, Int), radius :: Int }

newtype RectArray = RectArray { rectArray :: [Rect] }

data Dot = Dot { circle :: Circle, vel :: (Int, Int) }

class Collidable a b where
    intersects :: a -> b -> Bool

instance Collidable Rect Rect where
	--intersects :: Rect -> Rect -> Bool
    intersects Rect {rectX=ax,rectY=ay,rectW=aw,rectH=ah } Rect {rectX=bx,rectY=by,rectW=bw,rectH=bh }
        | bottomA <= topB || topA >= bottomB = False
        | rightA <= leftB || leftA >= rightB = False
        | otherwise = True
     where
        leftA   = ax
        rightA  = ax + aw
        topA    = ay
        bottomA = ay + ah
        
        leftB   = bx
        rightB  = bx + bw
        topB    = by
        bottomB = by + bh

instance (Collidable a b) => Collidable [a] [b] where
    intersects lhs rhs = any (\x -> any (intersects x) rhs) lhs

instance Collidable RectArray RectArray where
    --intersects :: Rect -> Rect -> Bool
    intersects (RectArray lhs) (RectArray rhs) = intersects lhs rhs

distance :: Int -> Int -> Int -> Int -> Float
distance x1 y1 x2 y2 = sqrt . fromIntegral $ x' + y'
 where
    x' = (x2 - x1) ^ 2
    y' = (y2 - y1) ^ 2

instance Collidable Circle Circle where
    intersects (Circle (aX,aY) aRad) (Circle (bX, bY) bRad) = distance aX aY bX bY < (fromIntegral $ aRad + bRad)

instance Collidable Circle RectArray where
    intersects (Circle (x,y) rad) (RectArray rs) =
        any (\r@(Rect bx by bw bh) ->
                let cx = if x < bx then bx else if x > (bx + bw) then bx + bw else x
                    cy = if y < by then by else if y > (by + bh) then by + bh else y
                in
                    distance x y cx cy < rad') rs				
     where
        rad'      = fromIntegral rad

defaultDot = Dot { circle=Circle (dotWidthHalf,dotWidthHalf) dotWidthHalf, vel=(0,0) }

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

move :: RectArray -> Circle -> Dot -> Dot
move rs c2 Dot { vel=v@(dx,dy), circle=c1@(Circle (x,y) rad) } = Dot { circle=Circle (x'', y'') rad, vel=v }
 where
    (x', y') = (x + dx, y + dy)
    
    c1' = Circle (x',y) rad
    x'' = if (x' - dotWidthHalf) < 0 || (x' + dotWidthHalf) > screenWidth || intersects c1' rs || intersects c1' c2 then x else x'
    
    c1'' = Circle (x'', y') rad
    y'' = if (y' - dotHeightHalf) < 0 || (y' + dotHeightHalf) > screenHeight || intersects c1'' rs || intersects c1'' c2 then y else y'

showCircle (Circle (x,y) r) = applySurface (x - r) (y - r) 
showDot Dot { circle=c } = showCircle c 

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
 where myDot = defaultDot

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifyDot . handleInput
    
    modifyDot $ move box otherDot
    
    fps       <- getFPS
    myDot     <- getDot
    dotSprite <- getDotSprite
    screen    <- getScreen    
    liftIO $ do
        jrect <- Just `liftM` getClipRect screen
        white <- mapRGB' screen 0xff 0xff 0xff
        black <- mapRGB' screen 0x00 0x00 0x00
        
        fillRect screen jrect white
        fillRect screen (Just $ head $ rectArray box) black
        
        showCircle otherDot dotSprite screen Nothing
        showDot myDot dotSprite screen Nothing
        
        Graphics.UI.SDL.flip screen
        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
    box      = RectArray [Rect 60 60 40 40]
    otherDot = Circle (30, 30) (dotWidth `div` 2)
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
