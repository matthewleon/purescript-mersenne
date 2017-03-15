// module Test.Main
var execFileSync = require('child_process').execFileSync;

exports.exec = function(path) {
  return function(args) {
    return String(execFileSync(path, args));
  }
};
