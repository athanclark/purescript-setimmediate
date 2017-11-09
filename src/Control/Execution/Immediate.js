"use strict";


exports.registerShim = function registerShim () {
  return require('setimmediate');
};


exports.run0Impl = function run0Impl (f) {
  return setImmediate(f);
};

exports.run1Impl = function run1Impl (f,a) {
  return setImmediate(f,a);
};

exports.run2Impl = function run2Impl (f,a,b) {
  return setImmediate(f,a,b);
};

exports.run3Impl = function run3Impl (f,a,b,c) {
  return setImmediate(f,a,b,c);
};

exports.run4Impl = function run4Impl (f,a,b,c,d) {
  return setImmediate(f,a,b,c,d);
};

exports.run5Impl = function run5Impl (f,a,b,c,d,e) {
  return setImmediate(f,a,b,c,d,e);
};

exports.cancelImpl = function cancelImpl (id) {
  clearImmediate(id);
};
