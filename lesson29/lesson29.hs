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
import Data.List

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

halfDotWidth  = dotWidth `div` 2
halfDotHeight = dotHeight `div` 2

-- Tile constants
tileWidth   = 80
tileHeight  = 80
totalTiles  = 192
tileSprites = 12

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

applySurface' :: MonadIO m => Int -> Int -> Surface -> Surface -> Maybe Rect -> m Bool
applySurface' x y src dst = liftIO . applySurface x y src dst

intersects :: Rect -> Rect -> Bool
intersects (Rect ax ay aw ah) (Rect bx by bw bh) =
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

-- The different tile sprites
data TileType =
      Red
    | Green
    | Blue
    | Center
    | Top
    | TopRight
    | Right
    | BottomRight
    | Bottom
    | BottomLeft
    | Left
    | TopLeft
 deriving (Eq, Ord, Enum, Bounded, Show)

data Tile = Tile {
    tBox     :: Rect,
    tileType :: TileType
}

tile :: Int -> Int -> TileType -> Tile
tile x y t = Tile { tBox=Rect x y tileWidth tileHeight, tileType=t }

touchesWall :: Rect -> [Tile] -> Bool
touchesWall b = any touches
 where touches (Tile tB tType) =
        let eType = fromEnum tType
        in if eType >= fromEnum Center && eType <= fromEnum TopLeft
            then intersects b tB
            else False

data Dot = Dot { dBox :: Rect, vel :: (Int, Int) }

dot = Dot (Rect 0 0 dotWidth dotHeight) (0,0)

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

move :: [Tile] -> Dot -> Dot
move tiles d@Dot { dBox=b@(Rect x y _ _), vel=(dx,dy) } = d { dBox=b { rectX=x'', rectY=y'' } }
 where
    (x', y') = (x + dx, y + dy)

    x'' = if x' < 0 || (x' + dotWidth) > levelWidth || touchesWall b {rectX=x'} tiles then x else x'

    y'' = if y' < 0 || (y' + dotHeight) > levelHeight || touchesWall b {rectX=x'',rectY=y'} tiles then y else y'

type Camera = Rect

setCamera :: Dot -> Camera -> Camera
setCamera Dot { dBox=Rect x y _ _ } rect@(Rect _ _ w h) = rect { rectX=x'', rectY=y'' }
 where
    x'  = (x + dotWidth `div` 2) - screenWidth `div` 2
    y'  = (y + dotHeight `div` 2) - screenHeight `div` 2
    x'' = min (levelWidth - w) $ max x' 0
    y'' = min (levelHeight - h) $ max y' 0 

setTiles :: IO [Tile]
setTiles = do
    tileMap <- wordsToInt `liftM` readFile "lazy.map"
    evalStateT (mapM mkTile tileMap) (0,0)
 where wordsToInt :: String -> [Int]
       wordsToInt = map read . words
       mkTile :: MonadState (Int,Int) m => Int -> m Tile
       mkTile t = do
        (x, y) <- get
        if (x + tileWidth >= levelWidth)
            then put (0, y + tileHeight)
            else put (x + tileWidth, y)
        
        if t >= 0 && t < tileSprites
            then return $ tile x y $ toEnum t
            else error "failed to parse tile"

clips :: [Rect]
clips = [
    -- red
    Rect 0 0 tileWidth tileHeight,
    -- green
    Rect 0 80 tileWidth tileHeight,
    -- Blue
    Rect 0 160 tileWidth tileHeight,
    -- Center
    Rect 160 80 tileWidth tileHeight,
    -- Top
    Rect 160 0 tileWidth tileHeight,
    -- TopRight
    Rect 240 0 tileWidth tileHeight,
    -- Right
    Rect 240 80 tileWidth tileHeight,
    -- BottomRight
    Rect 240 160 tileWidth tileHeight,
    -- Bottom
    Rect 160 160 tileWidth tileHeight,
    -- BottomLeft
    Rect 80 160 tileWidth tileHeight,
    -- Left
    Rect 80 80 tileWidth tileHeight,
    -- TopLeft
    Rect 80 0 tileWidth tileHeight]

data AppData = AppData {
    adDot  :: Dot,
    camera :: Camera,
    fps    :: Timer
}

data AppConfig = AppConfig {
    screen     :: Surface,
    dotSprite  :: Surface,
    tileSheet  :: Surface,
    tiles      :: [Tile]
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
getDot = liftM adDot get

putDot :: MonadState AppData m => Dot -> m ()
putDot t = modify $ \s -> s { adDot = t }

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

getDotSprite :: MonadReader AppConfig m => m Surface
getDotSprite = liftM dotSprite ask

getTileSheet :: MonadReader AppConfig m => m Surface
getTileSheet = liftM tileSheet ask

getTiles :: MonadReader AppConfig m => m [Tile]
getTiles = liftM tiles ask

showTile :: (MonadIO m, MonadReader AppConfig m) => Camera -> Tile -> m ()
showTile camera@(Rect cx cy _ _) (Tile tbox@(Rect tx ty _ _) tileType) =
    when (intersects tbox camera) $ do
        screen    <- getScreen
        tileSheet <- getTileSheet
        applySurface' (tx - cx) (ty - cy) tileSheet screen $ Just $ clips !! tileIndex
        return ()
 where tileIndex = fromEnum tileType

showDot :: (MonadIO m, MonadReader AppConfig m) => Camera -> Dot -> m ()
showDot (Rect cx cy _ _) (Dot (Rect dx dy _ _) _) = do
    screen    <- getScreen
    dotSprite <- getDotSprite
    applySurface' (dx - cx) (dy - cy) dotSprite screen Nothing
    return () 

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Move the Dot" []

    dotSprite <- loadImage "dot.png" $ Just (0x00, 0xff, 0xff)
    tileSheet <- loadImage "tiles.png" Nothing
    tiles     <- setTiles
    fps       <- start defaultTimer

    return (AppConfig screen dotSprite tileSheet tiles, AppData myDot camera fps) 
 where
    myDot  = dot
    camera = Rect 0 0 screenWidth screenHeight

loop :: AppEnv ()
loop = do
    
    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifyDot . handleInput
    
    tiles <- getTiles
    modifyDot $ move tiles
    
    myDot <- getDot
    modifyCamera $ setCamera myDot
    
    camera <- getCamera
    forM_ tiles $ showTile camera 
       
    showDot camera myDot
    
    screen <- getScreen
    fps    <- getFPS
    liftIO $ do                
        Graphics.UI.SDL.flip screen
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks
    
    unless quit loop
 where
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond

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
