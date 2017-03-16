"use strict";

exports.unsafePeekSTArray = function (xs) {
  return function (i) {
    return function () {
      return xs[i];
    };
  };
};
