# Functional Pac-Man

An interpretation of the [Pac-Man](https://en.wikipedia.org/wiki/Pac-Man) game written in around [1k lines](https://github.com/lucamug/pacman/blob/master/src/Game.elm) of purely functional code that runs in the browser and in the terminal.

The game is written in the [Elm language](https://elm-lang.org/) and [Gren language](https://gren-lang.org/) using the [TUI library](https://packages.gren-lang.org/package/blaix/gren-tui/version/3.0.2/overview) for the terminal part.

## In the browser

Demo: [https://lucamug.github.io/pacman/](https://lucamug.github.io/pacman/)

## In the terminal

To run the game in the terminal:

```shell
git clone https://github.com/lucamug/pacman/
cd pacman
node docs/gren-pacman
```

The program requires Node v20 or later.

![Screen 1](/docs/screen01.png?raw=true "Screen 1")

![Screen 2](/docs/screen02.png?raw=true "Screen 2")

![Animation](/docs/pacman.gif?raw=true "Animation")
