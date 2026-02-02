# Chip8 Emulator

This is a simple [Chip-8](https://en.wikipedia.org/wiki/CHIP-8) Emulator written in Clojure.

## How to run
It can be used in two ways:

### Java UI
UI is written using [quil](https://github.com/quil/quil)

``` sh
clojure
```

Use 'o' key to open another ROM file
Use 'p' key to pause the emulator

### Web UI

Everything handled on the backend server - [DataStar](https://data-star.dev/) is used to propagate server state to client and propagate user events to the server.

``` sh
clojure -M -m chip8.web
```

## Keymap:
The emulator uses the normal Chip8 keymap mapped to pyhsical keys

1 2 3 C      (Physical: 1 2 3 4)
4 5 6 D      (Physical: Q W E R)
7 8 9 E      (Physical: A S D F)
A 0 B F      (Physical: Z X C V)

## Tests

The project uses Kaocha for tests

``` sh
clojure -M:test --watch
```
