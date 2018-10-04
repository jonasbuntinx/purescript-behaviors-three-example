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
import ParticleEngine (Emitter, EmitterState(..), Particle, ParticleEngine, ParticleState(..), makeParticle, snowEngine)
import Three as Three
import Web.HTML (window) as HTML
import Web.HTML.Window (innerHeight, innerWidth)

type Delta = Number

type Stage =
  { dimensions :: Dimensions
  , particles :: Array Particle
  , geometry :: Three.BufferGeometry
  , emitter :: Emitter
  , prevRenderTime :: Seconds
  }

staging :: Dimensions -> Three.Scene -> ParticleEngine -> Effect Stage
staging dimensions scene engine = do
  now <- Seconds <$> (\m -> (unwrap m) / 1000.0) <$> unInstant <$> now
  --
  geometry <- Three.makeBufferGeometry
  particles <- for (0 .. round engine.particleCount) (\_ -> makeParticle engine)
  positions <- Three.makeFloat32BufferAttribute (concat $ particles <#> (\p -> [ p.position.x, p.position.y, p.position.z])) 3
  Three.addAttribute "position" positions geometry
  --
  material <- Three.makePointsMaterial { color: toHexString orangered, size: 3 }
  --
  points <- Three.makePoints (unwrap geometry) material
  Three.addChild (unwrap points) (unwrap scene)
  --
  pure { dimensions: dimensions, particles, geometry, emitter: { age: 0.0, state: EmitterAlive }, prevRenderTime: now }

rigging :: Screen -> ParticleEngine -> Stage -> Behavior Stage
rigging screen engine initialStage =
  fixB initialStage \b ->
    updateStage <$> Time.seconds <*> resize screen <*> b

  where
    updateStage :: Seconds -> Dimensions -> Stage -> Stage
    updateStage sec dimensions stage@{emitter, particles, prevRenderTime} =
      let
        dt = ((unwrap sec) - (unwrap prevRenderTime)) * 0.5
      in
        stage
          { dimensions = dimensions
          , particles = mapWithIndex (\i -> updateParticle i dt emitter) (staggeredStart dt stage)
          , emitter
            { age = emitter.age + dt
            , state = if (emitter.age  + dt) > engine.emitterDeathAge then EmitterDead else emitter.state
            }
          , prevRenderTime = sec
          }

    staggeredStart :: Delta -> Stage -> Array Particle
    staggeredStart dt { emitter, particles } =
      if emitter.age < engine.particleDeathAge
        then
          let
            start = round (engine.particlesPerSecond * (emitter.age + 0.0))
            end = round $ Math.min engine.particleCount (engine.particlesPerSecond * (emitter.age + dt))
          in
            modifyAtIndices (start .. end) (\p ->
                case p.state of
                  ParticleInit ->
                    p { state = ParticleAlive }
                  _ ->
                    p
              ) particles
        else
          particles

    updateParticle :: Int -> Delta -> Emitter -> Particle -> Particle
    updateParticle i dt emitter particle =
      case particle.state of
        ParticleAlive ->
          particle
            { position = particle.position + ( particle.velocity * { x: dt, y: dt, z: dt } )
            , velocity = particle.velocity + ( particle.acceleration * { x: dt, y: dt, z: dt } )
            , angle = particle.angle + particle.angleVelocity * (Math.pi/180.0) * dt
            , angleVelocity = particle.angleVelocity + particle.angleAcceleration * (Math.pi/180.0) * dt
            , age = particle.age + dt
            , state = if (particle.age + dt) > engine.particleDeathAge then ParticleDead else particle.state
            }
        ParticleDead ->
          case Tuple emitter.state (index initialStage.particles i) of
            Tuple EmitterAlive (Just p) ->
              p { state = ParticleAlive }
            Tuple _ (Just p) ->
              p { state = ParticleDead }
            Tuple _ _ ->
              particle
        ParticleInit ->
          particle

render :: Three.Scene -> Three.Camera -> Three.Renderer -> Stage -> Effect Unit
render scene camera renderer { dimensions, particles, geometry, emitter } = do
  Three.setSize dimensions.width dimensions.height renderer
  Three.setAspect (dimensions.width / dimensions.height) camera
  Three.updateProjectionMatrix camera
  --
  positions <- Three.makeFloat32BufferAttribute (concat $ particles <#> (\p -> [ p.position.x, p.position.y, p.position.z])) 3
  Three.addAttribute "position" positions geometry
  --
  Three.render scene camera renderer

main :: Effect Unit
main = do
  let engine = snowEngine
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

