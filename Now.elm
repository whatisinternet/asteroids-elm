module Now (..) where

-- Taken from http://stackoverflow.com/questions/29453679/how-do-i-get-the-current-time-in-elm

import Native.Now
import Debug


loadTime : Float
loadTime =
  Native.Now.loadTime
