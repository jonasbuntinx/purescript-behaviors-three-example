module Three where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)

-- Common

type X = Number
type Y = Number
type Z = Number
type Width = Number
type Height = Number
type Depth = Number
type Fov = Number
type Aspect = Number
type Near = Number
type Far = Number


-- Vector

foreign import data Vector3 :: Type

foreign import makeVector3Impl :: EffectFn3 X Y Z Vector3

makeVector3 :: X -> Y -> Z -> Effect Vector3
makeVector3 = runEffectFn3 makeVector3Impl

-- Object3D

foreign import data Object3D :: Type

foreign import setRotationImpl :: EffectFn4 X Y Z Object3D Unit

setRotation :: X -> Y -> Z -> Object3D -> Effect Unit
setRotation = runEffectFn4 setRotationImpl

foreign import addChildImpl :: EffectFn2 Object3D Object3D Unit

addChild :: Object3D -> Object3D -> Effect Unit
addChild = runEffectFn2 addChildImpl

foreign import setPositionImpl :: EffectFn4 X Y Z Object3D Unit

setPosition :: X -> Y -> Z -> Object3D -> Effect Unit
setPosition = runEffectFn4 setPositionImpl

foreign import lookAtImpl :: EffectFn4 X Y Z Object3D Unit

lookAt :: X -> Y -> Z -> Object3D -> Effect Unit
lookAt = runEffectFn4 lookAtImpl

-- Object3D > Scene

newtype Scene = Scene Object3D

derive instance newtypeScene :: Newtype Scene _

foreign import makeScene :: Effect Scene

-- Object3D > Points

newtype Points = Points Object3D

derive instance newtypePoints :: Newtype Points _

foreign import makePointsImpl :: EffectFn2 Geometry Material Points

makePoints :: Geometry -> Material -> Effect Points
makePoints = runEffectFn2 makePointsImpl

-- Object3D > Line

newtype Line = Line Object3D

derive instance newtypeLine :: Newtype Line _

foreign import makeLineImpl :: EffectFn2 Geometry Material Line

makeLine :: Geometry -> Material -> Effect Line
makeLine = runEffectFn2 makeLineImpl

-- Object3D > Mesh

newtype Mesh = Mesh Object3D

derive instance newtypeMesh :: Newtype Mesh _

foreign import makeMeshImpl :: EffectFn2 Geometry Material Mesh

makeMesh :: Geometry -> Material -> Effect Mesh
makeMesh = runEffectFn2 makeMeshImpl

-- Object3D > Camera

newtype Camera = Camera Object3D

derive instance newtypeCamera :: Newtype Camera _

foreign import makePerspectiveCameraImpl :: EffectFn4 Fov Aspect Near Far Camera

makePerspectiveCamera :: Fov -> Aspect -> Near -> Far -> Effect Camera
makePerspectiveCamera = runEffectFn4 makePerspectiveCameraImpl

foreign import setAspectImpl :: EffectFn2 Aspect Camera Unit

setAspect :: Aspect -> Camera -> Effect Unit
setAspect = runEffectFn2 setAspectImpl

foreign import updateProjectionMatrixImpl :: EffectFn1 Camera Unit

updateProjectionMatrix :: Camera -> Effect Unit
updateProjectionMatrix = runEffectFn1 updateProjectionMatrixImpl

-- Geometry

foreign import data Geometry :: Type

foreign import makeGeometry :: Effect Geometry

foreign import makeBoxGeometryImpl :: EffectFn3 Width Height Depth Geometry

makeBoxGeometry :: Width -> Height -> Depth -> Effect Geometry
makeBoxGeometry = runEffectFn3 makeBoxGeometryImpl

-- BufferGeometry

newtype BufferGeometry = BufferGeometry Geometry

derive instance newtypeBufferGeometry :: Newtype BufferGeometry _

foreign import makeBufferGeometry :: Effect BufferGeometry

foreign import addAttributeImpl :: EffectFn3 String Float32BufferAttribute BufferGeometry Unit

addAttribute :: String -> Float32BufferAttribute -> BufferGeometry -> Effect Unit
addAttribute = runEffectFn3 addAttributeImpl

-- BufferAttribute

foreign import data Float32BufferAttribute :: Type

foreign import makeFloat32BufferAttributeImpl :: forall t. EffectFn2 (Array t) Int Float32BufferAttribute

makeFloat32BufferAttribute :: forall t. Array t -> Int -> Effect Float32BufferAttribute
makeFloat32BufferAttribute = runEffectFn2 makeFloat32BufferAttributeImpl

foreign import setArrayFloat32Impl :: forall t. EffectFn2 (Array t) Float32BufferAttribute Unit

setArrayFloat32 :: forall t. Array t -> Float32BufferAttribute -> Effect Unit
setArrayFloat32 = runEffectFn2 setArrayFloat32Impl

foreign import needsUpdateImpl :: EffectFn2 Boolean Float32BufferAttribute Unit

needsUpdate :: Boolean-> Float32BufferAttribute -> Effect Unit
needsUpdate = runEffectFn2 needsUpdateImpl

-- Material

foreign import data Material :: Type

foreign import makeMeshBasicMaterialImpl :: EffectFn1 { color :: String } Material

makeMeshBasicMaterial :: { color :: String } -> Effect Material
makeMeshBasicMaterial = runEffectFn1 makeMeshBasicMaterialImpl

foreign import makePointsMaterialImpl :: EffectFn1 { color :: String, size :: Int } Material

makePointsMaterial :: { color :: String, size :: Int  } -> Effect Material
makePointsMaterial = runEffectFn1 makePointsMaterialImpl

-- Renderer

foreign import data Renderer :: Type

foreign import makeWebGLRenderer :: Effect Renderer

foreign import setSizeImpl :: EffectFn3 Width Height Renderer Unit

setSize :: Width -> Height -> Renderer -> Effect Unit
setSize = runEffectFn3 setSizeImpl

foreign import addDomElementImpl :: EffectFn1 Renderer Unit

addDomElement :: Renderer -> Effect Unit
addDomElement = runEffectFn1 addDomElementImpl

foreign import renderImpl :: EffectFn3 Scene Camera Renderer Unit

render :: Scene -> Camera -> Renderer -> Effect Unit
render = runEffectFn3 renderImpl
