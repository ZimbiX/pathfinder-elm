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

Currently only same-device multiplayer is supported. You can check it out at:

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

- Once player 2 finishes drawing in same-device mode, it should be player 1's turn to move first (on the maze player 2 just drew)
- Fix popup dismiss button's right half being unclickable
- Allow two players to draw their maze simultaneously on each device
- Resume as the right player upon a page reload - some form of login?
    + Generate UUID to identify the user, and store in local storage - only when missing
- Instantly replay game history upon page reload part-way through a game (no animation), but preserve how receiving an event from the opponent applies it as normal interaction (with animation)
- Disable local interaction while it's the opponent's move
- Generate UUID to identify the game on init - if not provided in URL
- Read game id from URL
- Display link for sharing with a friend to play against
- Refactor to provide an event for the popup dismiss button
- Fix board flipping stretching sideways when widescreen
- Call `switchMazes` from somewhere better
- Count moves and wall hits and show this in the winning message
- Prevent touchscreen button lag
- Verify maze is possible to win before leaving drawing stage
- Allow changing grid size
- Saving a maze to be able to reuse it
- Fade in new sections of traversed path
- Make wall removal hitbox rectangular, matching orientation
- Unit tests
- Switch from `List.concat` to `(::)` operator
- Prevent line drawing craziness on multitouch - check pointer ID is 1?
- Automatically resize to fit viewport - probably requires using a custom HTML page template
- Hold with a second finger to remove
- Split out to multiple files
- Deduplicate walls when finishing drawing
- Deduplicate golds
- Deduplicate path travelled
- Drag player to reposition
- Different background colour for each maze within a game
- Compensate for overshooting the point of collision in `returnToOriginIfPathUnclear`
