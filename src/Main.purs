module Main where

import Prelude

import Color (toHexString)
import Color.Scheme.X11 (orangered)
import Data.Array (concat, index, mapWithIndex, modifyAtIndices, (..))
import Data.DateTime.Instant (unInstant)
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Now (now)
import FRP.Behavior (Behavior, animate, fixB)
import FRP.Behavior.Screen (resize)
import FRP.Behavior.Time as Time
import FRP.Event.Screen (Screen, getScreen)
import Graphics.Canvas (Dimensions)
import Math as Math
import ParticleEngine as PE
import Three as Three
import Web.HTML (window) as HTML
import Web.HTML.Window (innerHeight, innerWidth)

type Stage =
  { dimensions :: Dimensions
  , particles :: Array PE.Particle
  , positions :: Three.Float32BufferAttribute
  , emitter :: PE.Emitter
  , prevRenderTime :: Seconds
  }

staging :: Dimensions -> Three.Scene -> PE.ParticleEngine -> Effect Stage
staging dimensions scene engine = do
  now <- Seconds <$> (\m -> (unwrap m) / 1000.0) <$> unInstant <$> now
  --
  geometry <- Three.makeBufferGeometry
  particles <- for (0 .. round engine.particleCount) (\_ -> PE.makeParticle engine)
  positions <- Three.makeFloat32BufferAttribute (concat $ particles <#> (\p -> [ p.position.x, p.position.y, p.position.z])) 3
  Three.addAttribute "position" positions geometry
  --
  material <- Three.makePointsMaterial { color: toHexString orangered, size: 3 }
  --
  points <- Three.makePoints (unwrap geometry) material
  Three.addChild (unwrap points) (unwrap scene)
  --
  pure { dimensions: dimensions, particles, positions, emitter: { age: 0.0, state: PE.EmitterAlive }, prevRenderTime: now }

rigging :: Screen -> PE.ParticleEngine -> Stage -> Behavior Stage
rigging screen engine initialStage =
  fixB initialStage \b ->
    updateStage <$> Time.seconds <*> resize screen <*> b

  where
    updateStage :: Seconds -> Dimensions -> Stage -> Stage
    updateStage sec dimensions stage@{emitter, particles, prevRenderTime} =
      let
        dt = (unwrap sec) - (unwrap prevRenderTime) # \d ->
          if d > 0.1
            then 0.0
            else d * 0.5
      in
        stage
          { dimensions = dimensions
          , particles = mapWithIndex (\i -> updateParticle i dt emitter) (staggeredStart dt stage)
          , emitter = emitter.age + dt # \age -> emitter
              { age = age
              , state =
                if age > engine.emitterDeathAge
                  then PE.EmitterDead
                  else emitter.state
              }
          , prevRenderTime = sec
          }

    staggeredStart :: Number -> Stage -> Array PE.Particle
    staggeredStart dt { emitter, particles } =
      if emitter.age < engine.particleDeathAge
        then
          let
            start = round (engine.particlesPerSecond * (emitter.age + 0.0))
            end = round $ Math.min engine.particleCount (engine.particlesPerSecond * (emitter.age + dt))
          in
            modifyAtIndices (start .. end) (\p ->
                case p.state of
                  PE.ParticleInit -> p { state = PE.ParticleAlive }
                  _ -> p
              ) particles
        else
          particles

    updateParticle :: Int -> Number -> PE.Emitter -> PE.Particle -> PE.Particle
    updateParticle i dt emitter particle =
      case particle.state of
        PE.ParticleAlive ->
          PE.updateParticle dt particle # \p ->
            if p.age > engine.particleDeathAge
              then p { state = PE.ParticleDead }
              else p
        PE.ParticleDead ->
          case Tuple emitter.state (index initialStage.particles i) of
            Tuple PE.EmitterAlive (Just initialParticle) ->
              initialParticle { state = PE.ParticleAlive }
            Tuple _ (Just initialParticle) ->
              initialParticle { state = PE.ParticleDead }
            Tuple _ _ ->
              particle
        PE.ParticleInit ->
          particle

render :: Three.Scene -> Three.Camera -> Three.Renderer -> Stage -> Effect Unit
render scene camera renderer { dimensions, particles, positions, emitter } = do
  Three.setSize dimensions.width dimensions.height renderer
  Three.setAspect (dimensions.width / dimensions.height) camera
  Three.updateProjectionMatrix camera
  --
  Three.setArrayFloat32 (concat $ particles <#> (\p -> [ p.position.x, p.position.y, p.position.z])) positions
  Three.needsUpdate true positions
  --
  Three.render scene camera renderer

main :: Effect Unit
main = do
  let engine = PE.snowEngine
  --
  window <- HTML.window
  width <- toNumber <$> innerWidth window
  height <- toNumber <$> innerHeight window
  --
  scene <- Three.makeScene
  stage <- staging { width, height } scene engine
  --
  camera <- Three.makePerspectiveCamera 45.0 (width / height) 2.0 5000.0
  Three.setPosition 0.0 200.0 400.0 (unwrap camera)
  Three.lookAt 0.0 0.0 0.0 (unwrap camera)
  --
  renderer <- Three.makeWebGLRenderer
  Three.setSize width height renderer
  Three.addDomElement renderer
  --
  screen <- getScreen
  --
  void $ animate (rigging screen engine stage) (render scene camera renderer)

