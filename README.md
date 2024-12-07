# Functional Pac-Man

![Animation](/docs/pacman.gif?raw=true "Animation")

An interpretation of the [Pac-Man](https://en.wikipedia.org/wiki/Pac-Man) game, written in approximately [1,000 lines](https://github.com/lucamug/functional-pacman/blob/master/src/Game.elm) of purely functional code. It runs both in the browser and in the terminal.

The game is written in the [Elm language](https://elm-lang.org/) and the [Gren language](https://gren-lang.org/), using the [TUI library](https://packages.gren-lang.org/package/blaix/gren-tui/version/3.0.2/overview) for the version that runs in the terminal.

## To run the game in the browser

Go to: [https://lucamug.github.io/pacman/](https://lucamug.github.io/pacman/)

## To run the game in the terminal

Execute:

```shell
git clone https://github.com/lucamug/functional-pacman/
cd pacman
node docs/gren-pacman
```

The program requires Node v20 or later.

![Screen 2](/docs/screen02.png?raw=true "Screen 2")

## Development

```shell
git clone https://github.com/lucamug/functional-pacman/
cd pacman
npm install
```

Commands are available under the `cmd` folder.

### To develop the Elm code

```shell
cmd/start-elm
```

Then point your browser to [http://localhost:8003/](http://localhost:8003/).

### To develop the Gren code

```shell
cmd/start-gren
```

This will compile and run the code in the terminal. [ESC] or [CTRL + C] to exit.

To format the Gren code

```shell
cmd/format-gren
```

### To release a new version

Run

```shell
cmd/build
```

This will update the file in `/docs/`. These files can be published via Github, for example, via "Settings > Pages > Deploy from a branch > main /docs".
