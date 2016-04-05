"use strict";

// module Node.OS

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
