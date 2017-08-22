"use strict";
/* jshint node: true */

var os = require('os');

exports.eol = os.EOL;
exports.archImpl = os.arch;
exports.cpus = os.cpus;
exports.endiannessImpl = os.endianness;
exports.freemem = os.freemem;
exports.homedir = os.homedir;
exports.hostname = os.hostname;
exports.loadavgImpl = os.loadavg;
exports.networkInterfaces = os.networkInterfaces;
exports.platformImpl = os.platform;
exports.release = os.release;
exports.tmpdir = os.tmpdir;
exports.totalmem = os.totalmem;
exports.ostype = os.type;
exports.uptime = os.uptime;
exports.userInfoImpl = function (update) {
  return function (Nothing) {
    return function (Just) {
      return function (opts) {
        return function () {
          var userInfo = os.userInfo(opts);
          if (userInfo.shell === null) {
            return update(function (x) {
              return Just(Nothing);
            })(userInfo);
          } else {
            return update(function (x) {
              return Just(Just(x));
            })(userInfo);
          }
        };
      };
    };
  };
};
