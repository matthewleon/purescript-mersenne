"use strict";

exports.unsafePeekSTArray = function (xs) {
  return function (i) {
    return function () {
      return xs[i];
    };
  };
};

exports.unsafePokeSTArray = function (xs) {
  return function (i) {
    return function (a) {
      return function () {
        xs[i] = a;
        return {};
      };
    };
  };
};
