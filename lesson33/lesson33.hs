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
module Main where

import Data.Word

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import Graphics.UI.SDL.Image
import Graphics.UI.SDL

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = mapRGB' surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

myThread :: MVar Bool -> IO ()
myThread quit = do
    quit' <- readMVar quit
    when (not quit') $ do
        -- Do the caption animation
        setCaption "Thread is running" []
        delay'
        
        setCaption "Thread is running." []
        delay'
        
        setCaption "Thread is running.." []
        delay'
        
        setCaption "Thread is running..." []
        delay'
        -- loop        
        myThread quit        
 where delay' = delay 250

loop :: MVar Bool -> IO ()
loop quit = do
    quit' <- whileEvents $ \_ -> return ()    
    if quit'
        then modifyMVar_ quit $ \_ -> return True
        else loop quit

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

main = withInit [InitEverything] $ do -- withInit calls quit for us.

    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Thread test" []
    
    quit <- newMVar False
    forkIO $ myThread quit
    
    image <- loadImage "image.png" Nothing
    
    applySurface 0 0 image screen Nothing
    
    Graphics.UI.SDL.flip screen
    
    loop quit