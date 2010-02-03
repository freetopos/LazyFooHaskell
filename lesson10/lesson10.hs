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
{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Control.Monad
import Control.Monad.Reader

import Data.Word

import Foreign
import Foreign.C.Types

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.TTF
import Graphics.UI.SDL.Utilities as Util
import qualified Graphics.UI.SDL.TTF.General as TTFG

-- `SDL_GetKeyState' is not defined in Graphic.UI.SDL
foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

type KeyProc = SDLKey -> Bool
-- this function comes from mokehehe's super nario bros: http://github.com/mokehehe/monao in the file "AppUtil.hs"
getKeyState :: IO KeyProc
getKeyState = alloca $ \numkeysPtr -> do
    keysPtr <- sdlGetKeyState numkeysPtr
    return $ \k -> (/= 0) $ unsafePerformIO $ (peekByteOff keysPtr $ fromIntegral $ Util.fromEnum k :: IO Word8)

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data AppConfig = AppConfig {
    screen       :: Surface,
    background   :: Surface,
    up           :: Surface,
    down         :: Surface,
    left         :: Surface,
    right        :: Surface,
    screenWidth  :: Int,
    screenHeight :: Int,
    screenBpp    :: Int
}

type AppEnv = ReaderT AppConfig IO

initEnv :: IO AppConfig
initEnv = do
    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Press an Arrow Key" []
        
    background <- loadImage "background.png" $ Just (0x00, 0xff, 0xff)
    font       <- openFont "lazy.ttf" 72
    up         <- renderTextSolid font "Up" textColor
    down       <- renderTextSolid font "Down" textColor
    left       <- renderTextSolid font "Left" textColor
    right      <- renderTextSolid font "Right" textColor
    
    return $ AppConfig screen background up down left right screenWidth screenHeight screenBpp
    
 where
    screenWidth  = 640
    screenHeight = 480
    screenBpp    = 32
    textColor    = Color 0 0 0
    
loop :: AppEnv ()
loop = do

    quit <- whileEvents $ \_ -> return ()
    
    screen       <- fmap screen ask
    background   <- fmap background ask
    screenWidth  <- fmap screenWidth ask
    screenHeight <- fmap screenHeight ask
    
    applySurface' 0 0 background screen Nothing
    
    keyState <- liftIO getKeyState
    
    when (keyState SDLK_UP) $ do
        up <- fmap up ask
        applySurface' ((screenWidth - surfaceGetWidth up) `div` 2) ((screenHeight `div` 2 - surfaceGetHeight up) `div` 2) up screen Nothing
        return ()
    
    when (keyState SDLK_DOWN) $ do
        down <- fmap down ask
        applySurface' ((screenWidth - surfaceGetWidth down) `div` 2) ((screenHeight `div` 2 - surfaceGetHeight down) `div` 2 + (screenHeight `div` 2)) down screen Nothing
        return ()

    when (keyState SDLK_LEFT) $ do
        left <- fmap left ask
        applySurface' ((screenWidth `div` 2 - surfaceGetWidth left) `div` 2) ((screenHeight - surfaceGetHeight left) `div` 2) left screen Nothing
        return ()

    when (keyState SDLK_RIGHT) $ do
        right <- fmap right ask
        applySurface' ((screenWidth `div` 2 - surfaceGetWidth right) `div` 2 + (screenWidth `div` 2)) ((screenHeight - surfaceGetHeight right) `div` 2) right screen Nothing
        return ()
    
    liftIO $ Graphics.UI.SDL.flip screen
    
    unless quit loop

 where
    applySurface' x y src dst clip = liftIO $ applySurface x y src dst clip

whileEvents :: (Event -> AppEnv ()) -> AppEnv Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> IO ()
runLoop = runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    result <- TTFG.init
    if not result
        then putStr "Failed to init ttf\n"
        else do
            env <- initEnv
            
            runLoop env
                        
            TTFG.quit