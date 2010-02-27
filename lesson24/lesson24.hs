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

import System.IO
import System.IO.Error

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

halfDotWidth  = dotWidth `div` 2
halfDotHeight = dotHeight `div` 2

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

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
    x'' = if x' < 0 || (x' + dotWidth) > screenWidth then x else x'    
    y'' = if y' < 0 || (y' + dotHeight) > screenHeight then y else y'

showDot Dot { pos=(x,y) } = applySurface x y 

data AppData = AppData {
    dot        :: Dot,
    background :: Pixel,
    fps        :: Timer
}

data AppConfig = AppConfig {
    screen     :: Surface,
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

getBackground :: MonadState AppData m => m Pixel
getBackground = liftM background get

putBackground :: MonadState AppData m => Pixel -> m ()
putBackground t = modify $ \s -> s { background = t }

modifyBgM :: MonadState AppData m => (Pixel -> m Pixel) -> m ()
modifyBgM act = getBackground >>= act >>= putBackground

modifyBg :: MonadState AppData m => (Pixel -> Pixel) -> m ()
modifyBg fn = fn `liftM` getBackground >>= putBackground

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getDotSprite :: MonadReader AppConfig m => m Surface
getDotSprite = liftM dotSprite ask

loadFile :: Dot -> Pixel -> Surface -> IO (Dot, Pixel)
loadFile thisDot bg screen =
    withFile "game_save" ReadMode (\fileHandle -> do
        line  <- hGetLine fileHandle
        let xs = read `map` words line :: [Int]
        when (length xs < 2) $ do
            ioError $ userError "failed to parse dot"
        
        let pos@(x, y) = (xs !! 0, xs !! 1)
        when (x < 0 || x > (screenWidth - dotWidth)) $ do
            ioError $ userError "dot-x is out of range"
        when (y < 0 || y > (screenHeight - dotHeight)) $ do
            ioError $ userError "dot-y is out of range"
        
        level <- hGetLine fileHandle
        bg' <- case level of
                "White Level" -> mapRGB' screen 0xFF 0xFF 0xFF
                "Red Level"   -> mapRGB' screen 0xFF 0x00 0x00
                "Green Level" -> mapRGB' screen 0x00 0xFF 0x00
                "Blue Level"  -> mapRGB' screen 0x00 0x00 0xFF
                _             -> return bg
        return (Dot pos (0,0), bg'))
    `catch` \e ->
        if not $ isUserError e
            then return (thisDot, bg)
            else ioError e -- rethrow exception

saveFile :: Dot -> Pixel -> Surface -> IO ()
saveFile Dot { pos=(x,y) } bg screen =
    withFile "game_save" WriteMode $ \fileHandle -> do
        hPutStrLn fileHandle dotStr
        (r,g,b) <- getRGB bg $ surfaceGetPixelFormat screen
        let level = if r == 0xFF && g == 0xFF && b == 0xFF then "White Level"
                    else if r == 0xFF then "Red Level"
                    else if g == 0xFF then "Green Level"
                    else if b == 0xFF then "Blue Level"
                    else []
        hPutStrLn fileHandle level
 where dotStr = show x ++ " " ++ show y

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Move the Dot" []
    dot <- loadImage "dot.png" (Just (0x00, 0xff, 0xff))
    fps <- start defaultTimer
    
    background <- mapRGB' screen 0xFF 0xFF 0xFF
    
    (myDot, bg) <- loadFile (Dot (0,0) (0,0)) background screen
    
    return (AppConfig screen dot, AppData myDot bg fps)

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit <- whileEvents $ \event -> do
        modifyDot $ handleInput event
        modifyBgM $ \b -> do
            screen <- getScreen
            case event of
                KeyDown (Keysym SDLK_1 _ _) -> mapRGBIO screen 0xFF 0xFF 0xFF
                KeyDown (Keysym SDLK_2 _ _) -> mapRGBIO screen 0xFF 0x00 0x00
                KeyDown (Keysym SDLK_3 _ _) -> mapRGBIO screen 0x00 0xFF 0x00
                KeyDown (Keysym SDLK_4 _ _) -> mapRGBIO screen 0x00 0x00 0xFF
                _ -> return b

    modifyDot $ move
    
    myDot  <- getDot        
    fps    <- getFPS    
    dot    <- getDotSprite
    screen <- getScreen
    background <- getBackground
    
    liftIO $ do
        clipRect <- Just `liftM` getClipRect screen
        fillRect screen clipRect background
                
        showDot myDot dot screen Nothing
        
        Graphics.UI.SDL.flip screen
        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond
    mapRGBIO s r g b = liftIO (mapRGB' s r g b)
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

runLoop :: AppConfig -> AppData -> IO AppData
runLoop = execStateT . runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    (env, state) <- initEnv
    state' <- runLoop env state
    saveFile (dot state') (background state') (screen env) 