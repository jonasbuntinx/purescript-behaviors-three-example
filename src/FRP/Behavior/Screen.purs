module FRP.Behavior.Screen
  ( resize
  ) where

import Prelude

import FRP.Behavior (Behavior, behavior)
import FRP.Event.Screen (Screen, withDimensions)
import Graphics.Canvas (Dimensions)

resize :: Screen -> Behavior Dimensions
resize screen = behavior \e -> map
  (\{ value, dimensions } -> value dimensions)
  (withDimensions screen e)
