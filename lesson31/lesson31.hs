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

import Foreign

import Data.Char
import Data.Word
import Data.Maybe
import Data.List
import Data.Array

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

data FlipFlags =
      Vertical
    | Horizontal
    deriving (Eq, Ord, Enum, Show)

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

pixelFormatGetRMask :: PixelFormat -> IO Word32
pixelFormatGetRMask format = withForeignPtr format $ \hsc_ptr -> peekByteOff hsc_ptr 16

pixelFormatGetGMask :: PixelFormat -> IO Word32
pixelFormatGetGMask format = withForeignPtr format $ \hsc_ptr -> peekByteOff hsc_ptr 20

pixelFormatGetBMask :: PixelFormat -> IO Word32
pixelFormatGetBMask format = withForeignPtr format $ \hsc_ptr -> peekByteOff hsc_ptr 24

pixelFormatGetAMask :: PixelFormat -> IO Word32
pixelFormatGetAMask format = withForeignPtr format $ \hsc_ptr -> peekByteOff hsc_ptr 28

getPixel32 :: Int -> Int -> Surface -> IO Pixel
getPixel32 x y s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    Pixel `liftM` peekElemOff pixels ((y * surfaceGetWidth s) + x)

putPixel32 :: Int -> Int -> Pixel -> Surface -> IO ()
putPixel32 x y (Pixel pixel) s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    pokeElemOff pixels ((y * surfaceGetWidth s) + x) pixel

flipSurface :: [FlipFlags] -> Surface -> IO Surface
flipSurface flags s = do
    
    surfFlags  <- surfaceGetFlags s
    let isColorKeyed = SrcColorKey `elem` surfFlags
    
    bitsPerPixel <- fromIntegral `liftM` pixelFormatGetBitsPerPixel format
    rMask <- pixelFormatGetRMask format
    gMask <- pixelFormatGetGMask format
    bMask <- pixelFormatGetBMask format
    aMask <- if isColorKeyed then return 0 else pixelFormatGetAMask format
    flipped <- createRGBSurface [SWSurface] (surfaceGetWidth s) (surfaceGetHeight s) bitsPerPixel rMask gMask bMask aMask 
    
    let (flippedW, flippedH) = (surfaceGetWidth flipped, surfaceGetHeight flipped)
    let flipAxis = if flipBoth `intersect` flags == flipBoth then
                       [(rx,ry) | rx <- [flippedW - 1, flippedW - 2 .. 0], ry <- [flippedH - 1, flippedH - 2 .. 0]]
                   else if Horizontal `elem` flags then
                       [(rx,y) | rx <- [flippedW - 1, flippedW - 2 .. 0], y <- [0.. flippedH - 1]]
                   else if Vertical `elem` flags then
                       [(x,ry) | x <- [0 .. flippedW - 1], ry <- [flippedH - 1, flippedH - 2 .. 0]]
                   else []
    
    when (not $ null flipAxis) $ do
        lockSurface s
        
        forM_ (zip [(x,y) | x <- [0 .. flippedW - 1], y <- [0 .. flippedH - 1]] flipAxis) $ \((x,y),(rx,ry)) -> do
            pixel <- getPixel32 x y s
            putPixel32 rx ry pixel flipped
        
        unlockSurface s
    
    when isColorKeyed $ do
        pixelFormatGetColorKey format >>= setColorKey flipped [RLEAccel, SrcColorKey]
        return ()
    
    return flipped
 where flipBoth = [Vertical,Horizontal]
       format =  surfaceGetPixelFormat s

initEnv :: IO ()
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Flip Test" []

    topLeft     <- loadImage "corner.png" $ Just (0x00, 0xff, 0xff)
    -- Flip surfaces
    topRight    <- flipSurface [Horizontal] topLeft
    bottomLeft  <- flipSurface [Vertical] topLeft
    bottomRight <- flipSurface [Horizontal, Vertical] topLeft
    
    -- Apply the images to the screen
    applySurface 0 0 topLeft screen Nothing
    applySurface 320 0 topRight screen Nothing
    applySurface 0 240 bottomLeft screen Nothing
    applySurface 320 240 bottomRight screen Nothing
    
    Graphics.UI.SDL.flip screen

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
    initEnv
    loop