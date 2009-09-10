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
module Timer (Timer(Timer), defaultTimer, start, stop, getTimerTicks, pause, unpause, isStarted, isPaused) where
	import qualified Graphics.UI.SDL.Time as SdlTime 
	import Data.Word
	
	data Timer = Timer { startTicks :: Word32, pausedTicks :: Word32, paused :: Bool, started :: Bool }

	defaultTimer	=	Timer { startTicks=0, pausedTicks=0, paused=False, started=False }
	
	--start, stop, pause, unpause :: Timer -> Timer
	start :: Timer -> IO Timer
	start Timer { pausedTicks=pt } = SdlTime.getTicks >>= \ticks -> return $ Timer { pausedTicks=pt, startTicks=ticks, started=True,paused=False }
	
	stop :: Timer -> Timer
	stop Timer { startTicks=st, pausedTicks=pt } = Timer { startTicks=st, pausedTicks=pt, paused=False, started=False }
	
	getTimerTicks :: Timer -> IO Word32
	getTimerTicks Timer { started=False } = return 0
	getTimerTicks Timer { started=True, paused=True, pausedTicks=pausedTicks' } = return pausedTicks'
	getTimerTicks Timer { started=True, paused=False, startTicks=st } = SdlTime.getTicks >>= \currTicks -> return $ currTicks - st
	--getTimerTicks _ = return 0
		
	pause :: Timer -> IO Timer
	pause Timer { started=True, paused=False, startTicks=st } = SdlTime.getTicks >>= \currTicks ->	return $ Timer { startTicks=st, pausedTicks=(currTicks - st), paused=True, started=True }
	pause timer = return timer
	
	unpause :: Timer -> IO Timer
	unpause timer@Timer { paused=False } = return timer
	unpause timer@Timer { paused=True, pausedTicks=pausedTicks', started=started' } =
		 SdlTime.getTicks >>= \currTicks -> return $ Timer { startTicks=(currTicks - pausedTicks'), pausedTicks=0, paused=False, started=started' }
	
	isStarted :: Timer -> Bool
	isStarted Timer { started=s } = s
	
	isPaused :: Timer -> Bool
	isPaused Timer { paused=p } = p
