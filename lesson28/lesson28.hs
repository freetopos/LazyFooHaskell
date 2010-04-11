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

import System.Random

import Data.Word
import Data.List

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

totalParticles = 20

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data Particle = Particle {
    offset :: (Int, Int),
    frame  :: Int,
    particleType :: Surface
}

data Dot = Dot {
    pos :: (Int, Int),
    vel :: (Int, Int),
    particles :: [Particle]
}

-- utility function to hide plumbing of random generator types.
rand :: (Random a, RandomGen g, MonadState g m) => a -> a -> m a
rand lo hi = do
    r <- get
    let (val, r') = randomR (lo, hi) r
    put r'
    return val

particle :: (RandomGen g, MonadState g m) => Int -> Int -> [Surface] -> m Particle 
particle x y colours = do
    ox <- (x - 5 +) `liftM` rand 0 24 -- we could use <$>/fmap instead.
    oy <- (y - 5 +) `liftM` rand 0 24
    frame  <- rand 0 4
    colour <- (colours !!) `liftM` rand 0 2
    return $ Particle (ox, oy) frame colour

isDead :: Particle -> Bool
isDead Particle { frame=frame } = frame > 10

dot :: (RandomGen g, MonadState g m) => [Surface] -> m Dot
dot colours = do
    particles <- replicateM totalParticles $ particle x y colours
    return $ Dot (x,y) (x,y) particles
 where (x,y) = (0,0)

handleInput :: Event -> Dot -> Dot
handleInput (KeyDown (Keysym SDLK_UP _ _)) dot@Dot { vel=(dx,dy) }    = dot { vel=(dx, dy - halfDotHeight) }
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) dot@Dot { vel=(dx,dy) }  = dot { vel=(dx, dy + halfDotHeight) }
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) dot@Dot { vel=(dx,dy) }  = dot { vel=(dx - halfDotWidth, dy) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) dot@Dot { vel=(dx,dy) } = dot { vel=(dx + halfDotWidth, dy) }

handleInput (KeyUp (Keysym SDLK_UP _ _)) dot@Dot { vel=(dx,dy) }    = dot { vel=(dx, dy + halfDotHeight) }
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) dot@Dot { vel=(dx,dy) }  = dot { vel=(dx, dy - halfDotHeight) }
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) dot@Dot { vel=(dx,dy) }  = dot { vel=(dx + halfDotWidth, dy) }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) dot@Dot { vel=(dx,dy) } = dot { vel=(dx - halfDotWidth, dy) }

handleInput _ d = d

move :: Dot -> Dot
move dot@Dot { pos=(x,y), vel=(dx,dy) } = dot { pos=(x'', y'') } 
 where
    x'  = x + dx
    y'  = y + dy
    x'' = if x' < 0 || (x' + dotWidth) > screenWidth then x else x' 
    y'' = if y' < 0 || (y' + dotHeight) > screenHeight then y else y'

data AppData = AppData {
    dotAD  :: Dot,
    acRand :: StdGen,
    fps    :: Timer
}

data AppConfig = AppConfig {
    screen    :: Surface,
    dotSprite :: Surface,
    shimmer   :: Surface,
    colours   :: [Surface]
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
getDot = liftM dotAD get

putDot :: MonadState AppData m => Dot -> m ()
putDot t = modify $ \s -> s { dotAD = t }

modifyDotM :: MonadState AppData m => (Dot -> m Dot) -> m ()
modifyDotM act = getDot >>= act >>= putDot

modifyDot :: MonadState AppData m => (Dot -> Dot) -> m ()
modifyDot fn = fn `liftM` getDot >>= putDot

getRGen :: MonadState AppData m => m StdGen
getRGen = liftM acRand get

putRGen :: MonadState AppData m => StdGen -> m ()
putRGen t = modify $ \s -> s { acRand = t }

modifyRGenM :: MonadState AppData m => (StdGen -> m (a, StdGen)) -> m a
modifyRGenM act = do
    r <- getRGen
    (d, r') <- act r
    putRGen r'
    return d

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getDotSprite :: MonadReader AppConfig m => m Surface
getDotSprite = liftM dotSprite ask

getShimmer :: MonadReader AppConfig m => m Surface
getShimmer = liftM shimmer ask

getColours :: MonadReader AppConfig m => m [Surface]
getColours = liftM colours ask

applySurface' x y src dst = liftIO . applySurface x y src dst

showParticle :: (MonadIO m, MonadReader AppConfig m) => Particle -> m Particle
showParticle p@(Particle (x,y) frame ptType) = do
    screen  <- getScreen
    shimmer <- getShimmer
    
    applySurface' x y ptType screen Nothing   
    when (frame `mod` 2 == 0) $ do
        applySurface' x y shimmer screen Nothing
        return ()
    
    return p { frame=frame + 1 }

showParticles :: (MonadIO m, RandomGen g, MonadState g m, MonadReader AppConfig m) => Dot -> m Dot
showParticles d@Dot { pos=(x,y), particles=particles } = do
    
    colours <- getColours
    
    p' <- forM particles $ \p -> do
            if isDead p 
                then particle x y colours
                else return p
    
    p'' <- mapM showParticle p'

    return d { particles=p'' }

showDot :: (MonadIO m, MonadState AppData m, MonadReader AppConfig m) => Dot -> m Dot
showDot d@Dot { pos=(x,y) } = do
    screen <- getScreen
    dot    <- getDotSprite
    
    applySurface' x y dot screen Nothing
    
    modifyRGenM $ runStateT $ showParticles d

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Particle Test" []
       
    dotSprite <- loadImage "dot.bmp" $ Just (0x00, 0xff, 0xff)
    red       <- loadImage "red.bmp" $ Just (0x00, 0xff, 0xff)
    blue      <- loadImage "blue.bmp" $ Just (0x00, 0xff, 0xff)
    green     <- loadImage "green.bmp" $ Just (0x00, 0xff, 0xff)
    shimmer   <- loadImage "shimmer.bmp" $ Just (0x00, 0xff, 0xff)
    
    setAlpha red [SrcAlpha, RLEAccel] 192
    setAlpha blue [SrcAlpha, RLEAccel] 192
    setAlpha green [SrcAlpha, RLEAccel] 192
    setAlpha shimmer [SrcAlpha, RLEAccel] 192
    
    fps <- start defaultTimer
    rg  <- (mkStdGen . fromIntegral) `liftM` getTicks
    let colours = [red,blue,green]
    let (myDot, rg') = runState (dot colours) rg
    
    return (AppConfig screen dotSprite shimmer colours, AppData myDot rg' fps) 

loop :: AppEnv ()
loop = do
    
    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifyDot . handleInput
    
    modifyDot move
    
    screen <- getScreen    
    liftIO $ do
        jrect <- Just `liftM` getClipRect screen
        color <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff        
        fillRect screen jrect color 
        
    modifyDotM showDot

    fps <- getFPS
    liftIO $ do
        Graphics.UI.SDL.flip screen        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond

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
