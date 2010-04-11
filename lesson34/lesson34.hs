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

{-
    WARNING: In this program we have video functions running in seperate threads.
    This is for demonstrative purposes only. You should never in a real application
    have video functions running in seperate threads.
-}
module Main where

import System.IO.Unsafe

import Data.Word

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import Graphics.UI.SDL.Image
import Graphics.UI.SDL

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

foldM' :: Monad m => a -> [b] -> (a -> b -> m a) -> m ()
foldM' x ys fn = foldM_ fn x ys

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = mapRGB' surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

showSurface :: QSem -> Int -> Int -> Surface -> Surface -> IO ()
showSurface videoLock x y src screen = do
    -- Lock
    waitQSem videoLock
    
    -- Blit
    applySurface x y src screen Nothing
    
    -- Update the screen
    Graphics.UI.SDL.flip screen
    
    -- Unlock
    signalQSem videoLock

blitterA, blitterB :: QSem -> [Surface] -> Surface -> IO ()
blitterA videoLock text screen =
    foldM' 10 text $ \y t -> do
        -- Wait
        delay 200
        
        -- Show surface
        showSurface videoLock (((screenWidth `div` 2 ) - surfaceGetWidth t) `div` 2) y t screen
        
        -- Move down
        return $ y + 100

blitterB videoLock text screen =
    foldM' 10 text $ \y t -> do
        -- Wait
        delay 200
        
        -- Show surface
        showSurface videoLock ((screenWidth `div` 2) + (((screenWidth `div` 2) - surfaceGetWidth t) `div` 2 )) y t screen
        
        -- Move down
        return $ y + 100

loop :: IO ()
loop = do
    quit <- whileEvents $ \_ -> return ()
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

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    result <- TTFG.init
    if (not result)
        then putStrLn "Failed to init ttf"
        else do
            screen    <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
            setCaption "Testing Threads" []
            
            videoLock <- newQSem 1
            
            background <- loadImage "background.png" (Just (0x00, 0xff, 0xff))
            font       <- openFont "lazy.ttf" 72
            text       <- sequence [renderTextSolid font "One" textColor,
                                    renderTextSolid font "Two" textColor,
                                    renderTextSolid font "Three" textColor,
                                    renderTextSolid font "Four" textColor,
                                    renderTextSolid font "Five" textColor]
            
            showSurface videoLock 0 0 background screen
            
            forkChild $ blitterA videoLock text screen
            forkChild $ blitterB videoLock text screen
            
            waitForChildren
            
            loop
 where textColor = Color 255 255 255

-- This section of code is copied from http://www.haskell.org/ghc/staging/docs/latest/html/libraries/base-4.2.0.0/Control-Concurrent.html#11
-- These helper functions allows for the main thread to wait for a child thread to finish.
children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
    cs <- takeMVar children
    case cs of
        []   -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar   <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkIO (io `finally` putMVar mvar ())