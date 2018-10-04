module ParticleEngine where

import Prelude

import Color (Color, hsl)
import Effect (Effect)
import Effect.Random (random)
import Math as Math

type Vector =
  { x :: Number
  , y :: Number
  , z :: Number }

data ParticleState = ParticleInit | ParticleAlive | ParticleDead

type Particle =
  { position :: Vector
  , velocity :: Vector
  , acceleration :: Vector
  , angle :: Number
  , angleVelocity :: Number
  , angleAcceleration :: Number
  , size :: Number
  , color :: Color
  , opacity :: Number
  , age :: Number
  , state :: ParticleState
  }

type ParticleEngine =
  { positionBase :: Vector
  , positionSpread :: Vector
  , positionRadius :: Number
  , velocityBase :: Vector
  , velocitySpread :: Vector
  , speedBase :: Number
  , speedSpread :: Number
  , accelerationBase :: Vector
  , accelerationSpread :: Vector
  , angleBase :: Number
  , angleSpread :: Number
  , angleVelocityBase :: Number
  , angleVelocitySpread :: Number
  , angleAccelerationBase :: Number
  , angleAccelerationSpread :: Number
  , sizeBase :: Number
  , sizeSpread :: Number
  , colorBase :: Vector
  , colorSpread :: Vector
  , opacityBase :: Number
  , opacitySpread :: Number
  , particlesPerSecond :: Number
  , particleDeathAge :: Number
  , emitterDeathAge :: Number
  , particleCount :: Number }

data EmitterState = EmitterAlive | EmitterDead

type Emitter =
  { age :: Number
  , state :: EmitterState }

snowEngine :: ParticleEngine
snowEngine =
  let
    particlesPerSecond  = 200.0
    particleDeathAge    = 4.0
    emitterDeathAge     = 60.0

  in
    { positionBase:             { x: 0.0, y: 200.0, z: 0.0 }
    , positionSpread:           { x: 500.0, y: 0.0, z: 500.0 }
    , positionRadius:           0.0
    , velocityBase:             { x: 0.0, y: (- 60.0), z: 0.0 }
    , velocitySpread:           { x: 50.0, y: 20.0, z: 50.0 }
    , speedBase:                0.0
    , speedSpread:              0.0
    , accelerationBase:         { x: 0.0, y: (- 10.0), z: 0.0 }
    , accelerationSpread:       { x: 0.0, y: 0.0, z: 0.0 }
    , angleBase:                0.0
    , angleSpread:              720.0
    , angleVelocityBase:        0.0
    , angleVelocitySpread:      60.0
    , angleAccelerationBase:    0.0
    , angleAccelerationSpread:  0.0
    , sizeBase:                 0.0
    , sizeSpread:               0.0
    , colorBase:                { x: 0.66, y: 1.0, z: 0.9 }
    , colorSpread:              { x: 0.0, y: 0.0, z: 0.0 }
    , opacityBase:              1.0
    , opacitySpread:            0.0
    , particlesPerSecond
    , particleDeathAge
    , emitterDeathAge
    , particleCount:            particlesPerSecond * ( Math.min particleDeathAge emitterDeathAge )
    }

makeParticle :: ParticleEngine -> Effect Particle
makeParticle engine = do
  position <- randomVector engine.positionBase engine.positionSpread
  velocity <- randomVector engine.velocityBase engine.velocitySpread
  acceleration <- randomVector engine.accelerationBase engine.accelerationSpread
  angle <- randomValue engine.angleBase engine.angleSpread
  angleVelocity <- randomValue engine.angleVelocityBase engine.angleVelocitySpread
  angleAcceleration <- randomValue engine.angleAccelerationBase engine.angleAccelerationSpread
  size <- randomValue engine.sizeBase engine.sizeSpread
  opacity <- randomValue engine.opacityBase engine.opacitySpread
  color <- randomVector engine.colorBase engine.colorSpread
  pure
    { position
    , velocity
    , acceleration
    , angle
    , angleVelocity
    , angleAcceleration
    , size
    , color:              hsl color.x color.y color.z
    , opacity
    , age:                0.0
    , state:              ParticleInit
    }

  where

    randomValue :: Number -> Number -> Effect Number
    randomValue base spread = (\r -> base + (spread * (r - 0.5))) <$> random

    randomVector :: Vector -> Vector -> Effect Vector
    randomVector base spread = (\x y z -> base + (spread * ({ x: x - 0.5, y: y - 0.5, z: z - 0.5 }))) <$> random <*> random <*> random

