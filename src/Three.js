"use strict";

var THREE = require("three");

// Vector

exports.makeVector3Impl = function (x, y, z) {
  return new THREE.Vector3(x, y, z);
};

// Object3D

exports.setRotationImpl = function (x, y, z, object3d) {
  object3d.rotation.set(x, y, z);
  return {};
};

exports.addChildImpl = function (child, parent) {
  parent.add(child);
  return {};
};

exports.setPositionImpl = function (x, y, z, object3d) {
  object3d.position.set(x, y, z);
  return {};
};

exports.lookAtImpl = function (x, y, z, object3d) {
  object3d.lookAt(x, y, z);
  return {};
};

// Object3D > Scene

exports.makeScene = function () {
  var scene = new THREE.Scene();
  return scene;
};

// Object3D > Points

exports.makePointsImpl = function (geometry, material) {
  return new THREE.Points(geometry, material);
};

// Object3D > Points

exports.makeLineImpl = function (geometry, material) {
  return new THREE.Line(geometry, material);
};

// Object3D > Mesh

exports.makeMeshImpl = function (geometry, material) {
  return new THREE.Mesh(geometry, material);
};

// Object3D >Camera

exports.makePerspectiveCameraImpl = function (fov, aspect, near, far) {
  return new THREE.PerspectiveCamera(fov, aspect, near, far);
};

exports.setAspectImpl = function (aspect, camera) {
  camera.aspect = aspect;
  return {};
};

exports.updateProjectionMatrixImpl = function (camera) {
  camera.updateProjectionMatrix();
  return {};
};

// Geometry

exports.makeGeometry = function () {
  return new THREE.Geometry();
};

exports.makeBoxGeometryImpl = function (width, height, depth) {
  return new THREE.BoxGeometry(width, height, depth);

};

// BufferGeometry

exports.makeBufferGeometry = function () {
  return new THREE.BufferGeometry();
};

exports.addAttributeImpl = function (name, attribute, bufferGeometry) {
  bufferGeometry.addAttribute(name, attribute);
  return {};
};

// BufferAttribute

exports.makeFloat32BufferAttributeImpl = function (array, itemSize) {
  return new THREE.Float32BufferAttribute(array, itemSize).setDynamic(true);
};

exports.setArrayFloat32Impl = function (array, bufferAttribute) {
  bufferAttribute.set(new Float32Array(array));
  return {};
};

exports.needsUpdateImpl = function (boolean, bufferAttribute) {
  bufferAttribute.needsUpdate = boolean;
  return {};
};

// Material

exports.makeMeshBasicMaterialImpl = function (parameters) {
  return new THREE.MeshBasicMaterial(parameters);
};

exports.makePointsMaterialImpl = function (parameters) {
  return new THREE.PointsMaterial(parameters);
};

// Renderer

exports.makeWebGLRenderer = function () {
  return new THREE.WebGLRenderer();
};

exports.setSizeImpl = function (width, height, renderer) {
  renderer.setSize(width, height);
  return {};
};

exports.addDomElementImpl = function (renderer) {
  document.body.appendChild(renderer.domElement);
  return {};
};

exports.renderImpl = function (scene, camera, renderer) {
  renderer.render(scene, camera);
  return {};
};
