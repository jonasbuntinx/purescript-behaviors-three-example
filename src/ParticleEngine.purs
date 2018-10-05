module ParticleEngine where

import Prelude

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
  --, angle :: Number
  --, angleVelocity :: Number
  --, angleAcceleration :: Number
  --, size :: Number
  --, color :: Color
  --, opacity :: Number
  , age :: Number
  , state :: ParticleState
  }

type ParticleEngine =
  { positionBase :: Vector
  , positionSpread :: Vector
  , positionRadius :: Number
  , velocityBase :: Vector
  , velocitySpread :: Vector
  --, speedBase :: Number
  --, speedSpread :: Number
  , accelerationBase :: Vector
  , accelerationSpread :: Vector
  --, angleBase :: Number
  --, angleSpread :: Number
  --, angleVelocityBase :: Number
  --, angleVelocitySpread :: Number
  --, angleAccelerationBase :: Number
  --, angleAccelerationSpread :: Number
  --, sizeBase :: Number
  --, sizeSpread :: Number
  --, colorBase :: Vector
  --, colorSpread :: Vector
  --, opacityBase :: Number
  --, opacitySpread :: Number
  , particlesPerSecond :: Number
  , particleDeathAge :: Number
  , emitterDeathAge :: Number
  , particleCount :: Number }

data EmitterState = EmitterAlive | EmitterDead

type Emitter =
  { age :: Number
  , state :: EmitterState }

makeEngine :: ParticleEngine
makeEngine =
  let
    particlesPerSecond  = 100.0
    particleDeathAge    = 1.0
    emitterDeathAge     = 60.0
    particleCount       = particlesPerSecond * ( Math.min particleDeathAge emitterDeathAge )

  in
    { positionBase:             { x: 0.0, y: 0.0, z: 0.0 }
    , positionSpread:           { x: 0.0, y: 0.0, z: 0.0 }
    , positionRadius:           0.0
    , velocityBase:             { x: 0.0, y: 0.0, z: 0.0 }
    , velocitySpread:           { x: 0.0, y: 0.0, z: 0.0 }
    --, speedBase:                0.0
    --, speedSpread:              0.0
    , accelerationBase:         { x: 0.0, y: 0.0, z: 0.0 }
    , accelerationSpread:       { x: 0.0, y: 0.0, z: 0.0 }
    --, angleBase:                0.0
    --, angleSpread:              0.0
    --, angleVelocityBase:        0.0
    --, angleVelocitySpread:      0.0
    --, angleAccelerationBase:    0.0
    --, angleAccelerationSpread:  0.0
    --, sizeBase:                 0.0
    --, sizeSpread:               0.0
    --, colorBase:                { x: 0.0, y: 1.0, z: 0.5 }
    --, colorSpread:              { x: 0.0, y: 0.0, z: 0.0 }
    --, opacityBase:              1.0
    --, opacitySpread:            0.0
    , particlesPerSecond
    , particleDeathAge
    , emitterDeathAge
    , particleCount
    }

snowEngine :: ParticleEngine
snowEngine =
  let
    particlesPerSecond  = 200.0
    particleDeathAge    = 4.0
    emitterDeathAge     = 60.0
    particleCount       = particlesPerSecond * ( Math.min particleDeathAge emitterDeathAge )

  in
    makeEngine
      { positionBase            = { x: 0.0, y: 200.0, z: 0.0 }
      , positionSpread          = { x: 500.0, y: 0.0, z: 500.0 }
      , velocityBase            = { x: 0.0, y: (- 60.0), z: 0.0 }
      , velocitySpread          = { x: 50.0, y: 20.0, z: 50.0 }
      , accelerationBase        = { x: 0.0, y: (- 10.0), z: 0.0 }
      --, angleSpread             = 720.0
      --, angleVelocitySpread     = 60.0
      --, colorBase               = { x: 0.66, y: 1.0, z: 0.9 }
      --, colorSpread             = { x: 0.0, y: 0.0, z: 0.0 }
      , particlesPerSecond      = particlesPerSecond
      , particleDeathAge        = particleDeathAge
      , emitterDeathAge         = emitterDeathAge
      , particleCount           = particleCount
      }

rainEngine :: ParticleEngine
rainEngine =
  let
    particlesPerSecond  = 1000.0
    particleDeathAge    = 1.0
    emitterDeathAge     = 60.0
    particleCount       = particlesPerSecond * ( Math.min particleDeathAge emitterDeathAge )

  in
    makeEngine
      { positionBase            = { x: 0.0, y: 200.0, z: 0.0 }
      , positionSpread          = { x: 600.0, y: 0.0, z: 600.0 }
      , velocityBase            = { x: 0.0, y: (- 400.0), z: 0.0 }
      , velocitySpread          = { x: 10.0, y: 50.0, z: 10.0 }
      , accelerationBase        = { x: 0.0, y: (- 10.0), z: 0.0 }
      --, sizeBase                = 8.0
      --, sizeSpread              = 4.0
      --, colorBase               = { x: 0.66, y: 1.0, z: 0.7 }
      --, colorSpread             = { x: 0.0, y: 0.0, z: 0.2 }
      , particlesPerSecond      = particlesPerSecond
      , particleDeathAge        = particleDeathAge
      , emitterDeathAge         = emitterDeathAge
      , particleCount           = particleCount
      }

firefliesEngine :: ParticleEngine
firefliesEngine =
  let
    particlesPerSecond  = 20.0
    particleDeathAge    = 6.0
    emitterDeathAge     = 60.0
    particleCount       = particlesPerSecond * ( Math.min particleDeathAge emitterDeathAge )

  in
    makeEngine
      { positionBase            = { x: 0.0, y: 100.0, z: 0.0 }
      , positionSpread          = { x: 400.0, y: 200.0, z: 400.0 }
      , velocitySpread          = { x: 60.0, y: 20.0, z: 60.0 }
      --, sizeBase                = 30.0
      --, sizeSpread              = 2.0
      --, colorBase               = { x: 0.3, y: 1.0, z: 0.6 }
      --, colorSpread             = { x: 0.3, y: 0.0, z: 0.0 }
      , particlesPerSecond      = particlesPerSecond
      , particleDeathAge        = particleDeathAge
      , emitterDeathAge         = emitterDeathAge
      , particleCount           = particleCount
      }

fountainEngine :: ParticleEngine
fountainEngine =
  let
    particlesPerSecond  = 200.0
    particleDeathAge    = 3.0
    emitterDeathAge     = 60.0
    particleCount       = particlesPerSecond * ( Math.min particleDeathAge emitterDeathAge )

  in
    makeEngine
      { positionBase            = { x: 0.0, y: 5.0, z: 0.0 }
      , positionSpread          = { x: 10.0, y: 0.0, z: 10.0 }
      , velocityBase            = { x: 0.0, y: 160.0, z: 0.0 }
      , velocitySpread          = { x: 100.0, y: 20.0, z: 100.0 }
      , accelerationBase        = { x: 0.0, y: (- 100.0), z: 0.0 }
      --, angleSpread             = 30.0
      --, angleVelocitySpread     = 360.0 * 4.0
      , particlesPerSecond      = particlesPerSecond
      , particleDeathAge        = particleDeathAge
      , emitterDeathAge         = emitterDeathAge
      , particleCount           = particleCount
      }

makeParticle :: ParticleEngine -> Effect Particle
makeParticle engine = do
  position <- randomVector engine.positionBase engine.positionSpread
  velocity <- randomVector engine.velocityBase engine.velocitySpread
  acceleration <- randomVector engine.accelerationBase engine.accelerationSpread
  --angle <- randomValue engine.angleBase engine.angleSpread
  --angleVelocity <- randomValue engine.angleVelocityBase engine.angleVelocitySpread
  --angleAcceleration <- randomValue engine.angleAccelerationBase engine.angleAccelerationSpread
  --size <- randomValue engine.sizeBase engine.sizeSpread
  --opacity <- randomValue engine.opacityBase engine.opacitySpread
  --color <- randomVector engine.colorBase engine.colorSpread
  pure
    { position
    , velocity
    , acceleration
    --, angle
    --, angleVelocity
    --, angleAcceleration
    --, size
    --, color:              hsl color.x color.y color.z
    --, opacity
    , age:                0.0
    , state:              ParticleInit
    }

  where

    randomValue :: Number -> Number -> Effect Number
    randomValue base spread = (\r -> base + (spread * (r - 0.5))) <$> random

    randomVector :: Vector -> Vector -> Effect Vector
    randomVector base spread = (\x y z -> base + (spread * ({ x: x - 0.5, y: y - 0.5, z: z - 0.5 }))) <$> random <*> random <*> random

updateParticle :: Number -> Particle -> Particle
updateParticle dt particle =
  particle
    { position = particle.position + ( particle.velocity * { x: dt, y: dt, z: dt } )
    , velocity = particle.velocity + ( particle.acceleration * { x: dt, y: dt, z: dt } )
    --, angle = particle.angle + particle.angleVelocity * (Math.pi/180.0) * dt
    --, angleVelocity = particle.angleVelocity + particle.angleAcceleration * (Math.pi/180.0) * dt
    , age = particle.age + dt
    }
