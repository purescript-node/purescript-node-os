"use strict";
/* jshint node: true */

import os from 'os';

export const eol = os.EOL;
export const archImpl = os.arch;
export const cpus = os.cpus;
export const endiannessImpl = os.endianness;
export const freemem = os.freemem;
export const homedir = os.homedir;
export const hostname = os.hostname;
export const loadavgImpl = os.loadavg;
export const networkInterfaces = os.networkInterfaces;
export const platformImpl = os.platform;
export const release = os.release;
export const tmpdir = os.tmpdir;
export const totalmem = os.totalmem;
export const ostype = os.type;
export const uptime = os.uptime;
export const userInfoImpl = function (update) {
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
