module FRP.Event.Screen
  ( Screen
  , getScreen
  , disposeScreen
  --, resize
  , withDimensions
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, makeEvent, subscribe)
import Graphics.Canvas (Dimensions)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window) as HTML
import Web.HTML.Window (innerHeight, innerWidth, toEventTarget)

newtype Screen = Screen
  { dimensions :: Ref.Ref Dimensions
  , dispose :: Effect Unit
  }

getScreen :: Effect Screen
getScreen = do
  window <- HTML.window
  width <- toNumber <$> innerWidth window
  height <- toNumber <$> innerHeight window
  dimensions <- Ref.new { width , height }

  resizeListener <- eventListener \_ -> do
    w <- toNumber <$> innerWidth window
    h <- toNumber <$> innerHeight window
    Ref.write { width: w , height: h } dimensions

  addEventListener (wrap "resize") resizeListener false (toEventTarget window)
  let dispose = removeEventListener (wrap "resize") resizeListener false (toEventTarget window)
  pure (Screen { dimensions, dispose })

disposeScreen :: Screen -> Effect Unit
disposeScreen (Screen { dispose }) = dispose

{--
resize :: Event Dimensions
resize = makeEvent \k -> do
  window <- HTML.window
  listener <- eventListener \_ -> do
    width <- toNumber <$> innerWidth window
    height <- toNumber <$> innerHeight window
    k { width, height }
  addEventListener (wrap "resize") listener false (toEventTarget window)
  pure $ removeEventListener (wrap "resize") listener false (toEventTarget window)
--}

withDimensions :: forall a. Screen -> Event a -> Event { value :: a, dimensions :: Dimensions }
withDimensions (Screen { dimensions }) e = makeEvent \k -> do
  canceller <- e `subscribe` \value -> do
    dimensions' <- Ref.read dimensions
    k { value, dimensions: dimensions' }
  pure canceller
