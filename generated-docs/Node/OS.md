## Module Node.OS

#### `NetworkInterface`

``` purescript
type NetworkInterface = { address :: String, netmask :: String, family :: String, mac :: String, internal :: Boolean }
```

#### `CPU`

``` purescript
type CPU = { model :: String, speed :: Int, times :: { user :: Milliseconds, nice :: Milliseconds, sys :: Milliseconds, idle :: Milliseconds, irq :: Milliseconds } }
```

#### `OS`

``` purescript
data OS :: Effect
```

#### `loadavg`

``` purescript
loadavg :: forall eff. Eff (os :: OS | eff) { one :: Number, five :: Number, fifteen :: Number }
```

#### `Arch`

``` purescript
data Arch
  = X64
  | ARM
  | IA32
  | UnknownArch
```

##### Instances
``` purescript
Eq Arch
Show Arch
```

#### `arch`

``` purescript
arch :: forall eff. Eff (os :: OS | eff) Arch
```

#### `Endianness`

``` purescript
data Endianness
  = LittleEndian
  | BigEndian
  | UnknownEndian
```

##### Instances
``` purescript
Eq Endianness
Show Endianness
```

#### `endianness`

``` purescript
endianness :: forall eff. Eff (os :: OS | eff) Endianness
```

#### `Platform`

``` purescript
data Platform
  = Darwin
  | FreeBSD
  | Linux
  | SunOS
  | Win32
  | UnknownPlatform
```

##### Instances
``` purescript
Eq Platform
Show Platform
```

#### `platform`

``` purescript
platform :: forall eff. Eff (os :: OS | eff) Platform
```

#### `userInfo`

``` purescript
userInfo :: forall eff. { encoding :: String } -> Eff (os :: OS | eff) UserInfo
```

#### `eol`

``` purescript
eol :: Char
```

#### `cpus`

``` purescript
cpus :: forall eff. Eff (os :: OS | eff) (Array CPU)
```

#### `freemem`

``` purescript
freemem :: forall eff. Eff (os :: OS | eff) Number
```

#### `homedir`

``` purescript
homedir :: forall eff. Eff (os :: OS | eff) String
```

#### `hostname`

``` purescript
hostname :: forall eff. Eff (os :: OS | eff) String
```

#### `release`

``` purescript
release :: forall eff. Eff (os :: OS | eff) String
```

#### `tmpdir`

``` purescript
tmpdir :: forall eff. Eff (os :: OS | eff) String
```

#### `totalmem`

``` purescript
totalmem :: forall eff. Eff (os :: OS | eff) Number
```

#### `ostype`

``` purescript
ostype :: forall eff. Eff (os :: OS | eff) String
```

#### `uptime`

``` purescript
uptime :: forall eff. Eff (os :: OS | eff) Seconds
```

#### `networkInterfaces`

``` purescript
networkInterfaces :: forall eff. Eff (os :: OS | eff) (StrMap (Array NetworkInterface))
```


