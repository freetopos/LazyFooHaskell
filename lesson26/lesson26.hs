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

import Control.Monad.Reader
import Control.Monad.State

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

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

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

data Window = Window { screen :: Surface, windowed :: Bool }

makeWindow :: String -> Int -> Int -> Int -> IO Window
makeWindow title w h bpp = do
    screen <- setVideoMode w h bpp [SWSurface, Resizable]
    setCaption title []
    return $ Window screen True

windowGetWidth :: Window -> Int
windowGetWidth (Window s _) = surfaceGetWidth s

windowGetHeight :: Window -> Int
windowGetHeight (Window s _) = surfaceGetHeight s

windowGetBpp :: Window -> IO Word8
windowGetBpp (Window s _) = (pixelFormatGetBitsPerPixel . surfaceGetPixelFormat) s

toggleScreenMode :: Window -> IO Window
toggleScreenMode oldWindow@(Window oldScreen isWindowed) = do
    bpp    <- fromIntegral `liftM` windowGetBpp oldWindow
    screen <- setVideoMode w h bpp flags
    return $ Window screen $ not isWindowed
 where
    w = windowGetWidth oldWindow
    h = windowGetHeight oldWindow
    flags = if isWindowed then
                [SWSurface, Resizable, Fullscreen]
            else 
                [SWSurface, Resizable]

handleEvents :: Event -> Window -> IO Window
handleEvents (VideoResize w h) ow@(Window screen True) = do
    bpp     <- fromIntegral `liftM` windowGetBpp ow
    screen' <- setVideoMode w h bpp [SWSurface, Resizable]
    return $ Window screen' True

handleEvents (KeyDown (Keysym SDLK_RETURN _ _)) oldWindow = toggleScreenMode oldWindow

handleEvents (GotFocus focusList) w  = mapM_ (\_ -> setCaption "Window Event Test" []) focusList >> return w
handleEvents (LostFocus focusList) w = mapM_ setCaption' focusList >> return w
 where
    setCaption' ApplicationFocus = setCaption "Window Event Test: Iconified" []
    setCaption' InputFocus       = setCaption "Window Event Test: Keyboard focus lost" []
    setCaption' MouseFocus       = setCaption "Window Event Test: Mouse Focus Lost" []

handleEvents VideoExpose w@(Window screen _) = Graphics.UI.SDL.flip screen >> return w

handleEvents _ w = return w

type AppData   = Window
type AppConfig = Surface

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getScreen :: MonadState AppData m => m Surface
getScreen = liftM screen get

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM act = get >>= act >>= put

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    myWindow <- makeWindow "Window Event Test" screenWidth screenHeight screenBpp
    testing  <- loadImage "window.png" Nothing    
    return (testing, myWindow) 

loop :: AppEnv ()
loop = do
    quit <- whileEvents $ modifyM . handleEvents'
    testing <- ask
    screen  <- getScreen    
    liftIO $ do
        jrect <- Just `liftM` getClipRect screen
        white <- mapRGB' screen 0xff 0xff 0xff
        
        fillRect screen jrect white
        applySurface ((surfaceGetWidth screen - surfaceGetWidth testing) `div` 2)
                     ((surfaceGetHeight screen - surfaceGetHeight testing) `div` 2) testing screen Nothing
        
        Graphics.UI.SDL.flip screen

    unless quit loop
 where handleEvents' e w = liftIO $ handleEvents e w

whileEvents :: (MonadIO m) => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        KeyDown (Keysym SDLK_ESCAPE _ _)-> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    (env, state) <- initEnv
    runLoop env state
