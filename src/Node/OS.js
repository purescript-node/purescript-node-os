export { 
  EOL as eol, 
  arch as archImpl, 
  constants, 
  cpus, 
  devNull, 
  endianness as endiannessImpl, 
  freemem, 
  getPriority as getPriorityImpl, 
  homedir,
  hostname, 
  loadavg as loadavgImpl,
  machine,
  networkInterfaces as networkInterfacesImpl, 
  // platform, -- platform is defined in `node-process` library
  release, 
  setPriority as setPriorityImpl, 
  tmpdir, 
  totalmem, 
  type as type_, 
  uptime, 
  userInfo as userInfoImpl, 
  version 
} from "node:os";
