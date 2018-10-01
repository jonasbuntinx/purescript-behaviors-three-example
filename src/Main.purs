module Main where

import Prelude

import Color (Color, cssStringHSLA)
import Color.Scheme.MaterialDesign (blueGrey, green)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import FRP.Behavior (Behavior, animate)
import FRP.Behavior.Keyboard (key)
import FRP.Behavior.Screen (resize)
import FRP.Event.Keyboard (Keyboard, getKeyboard)
import FRP.Event.Screen (Screen, getScreen)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, Dimensions, Rectangle, drawImage, fillPath, getCanvasElementById, getContext2D, rect, setCanvasDimensions, setFillStyle, tryLoadImage, withContext)
import Partial.Unsafe (unsafePartial)

type Assets =
  { image :: Maybe CanvasImageSource }

type Background =
  { shape :: Rectangle
  , fill :: Color
  }

type MyScene =
  { dimensions :: Dimensions
  , background :: Background
  , foreground :: Maybe CanvasImageSource }

scene :: Screen -> Keyboard -> Assets -> Behavior MyScene
scene screen keyboard assets = ado
  bg <- background
  fg <- foreground assets
  d <- resize screen
  in { dimensions: d, background: bg, foreground: fg }

  where
    background :: Behavior Background
    background = ado
      color <- bgColor
      { width, height } <- resize screen
      in { shape: { x: 0.0, y: 0.0, width, height }
      , fill: color
      }

    bgColor :: Behavior Color
    bgColor = key keyboard "ArrowUp" <#> (
      if _
        then
          blueGrey
        else
          green
    )

    foreground :: Assets -> Behavior (Maybe CanvasImageSource)
    foreground { image } = key keyboard "ArrowUp" <#> (
      if _
        then
          image
        else
          Nothing
    )

render :: CanvasElement -> Context2D -> MyScene -> Effect Unit
render canvas ctx { dimensions, background, foreground } = do
  setCanvasDimensions canvas dimensions
  withContext ctx do
    setFillStyle ctx (cssStringHSLA background.fill)
    fillPath ctx $
    rect ctx background.shape

  for_ foreground \img -> drawImage ctx img 10.0 10.0

loadImage :: String -> Aff (Maybe CanvasImageSource)
loadImage path = Aff.makeAff \resolve -> tryLoadImage path (resolve <<< Right) *> mempty

main :: Effect Unit
main = launchAff_ do
  image <- loadImage "https://i.pinimg.com/564x/97/d5/21/97d5210058c6249539794697c8fc86bd.jpg"

  liftEffect do
    mcanvas <- getCanvasElementById "canvas"
    let canvas = unsafePartial (fromJust mcanvas)
    ctx <- getContext2D canvas
    keyboard <- getKeyboard
    screen <- getScreen
    void $ animate (scene screen keyboard { image : image }) (render canvas ctx)


-- use traverse for: Array (Maybe a) -> Maybe (Array a)

-- Rob's bad version: traverse :: forall f m a. Traversable f => Monad m => f (m a) -> m (f a)
-- use Identity for traverse

