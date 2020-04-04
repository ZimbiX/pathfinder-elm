# PathFinder Elm

<!-- MarkdownTOC autolink=true -->

- [Play](#play)
- [Elm](#elm)
- [History](#history)
- [Development](#development)
    - [Dependencies](#dependencies)
    - [Run](#run)
    - [Build](#build)
    - [Todo](#todo)

<!-- /MarkdownTOC -->

## Play

This app hardly does anything yet, but you can check it out at:

https://pathfinder-elm.netlify.com/

## Elm

I'm using this an opportunity to learn Functional Programming! :D Elm seems like a good choice, and I'm finding the [Elm Guide](https://guide.elm-lang.org/) very approachable.

My hope is for this modern rewrite to eventually be able to run on smartphones as an installable app.

## History

This is my fourth attempt at writing an implementation of the board game, PathFinder. See my original version on my old website for more info on the game: http://www.zimbico.net/games/pathfinder/

Versions:

- GameMaker (the most complete)
- GameMaker Studio (Android spike)
- Pascal using SwinGame - for a uni project
- This one!

## Development

### Dependencies

To install development dependencies (only supports Arch Linux, currently):

```bash
$ ./scripts/setup
```

### Run

To run the app with live reload on code changes:

```bash
$ ./scripts/start
```

### Build

To build the app to the `dist` folder:

```bash
$ ./scripts/build
```

### Todo

- Interpolate wall placement points to handle line being drawn quickly
