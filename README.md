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

- Sync game state with backend
    + Deal with sending/receiving events out of order
        * Make backend only reply with contiguous events
        * **When receiving events, only apply events if they are the next expected version**, ~~and sort the events queued for application by version upon receiving more~~
    + Instantly replay game history upon page reload part-way through a game (no animation)
        * Figure out how to resume as the right player - some form of login?
    + Receive an event from the opponent and apply it as normal interaction (with animation), while local interaction for the opponent is disabled
    + Generate UUID to identify the game on init
- Refactor to provide an event for the popup dismiss button
- Fix board flipping stretching sideways when widescreen
- Switch mazes from being a tuple to `mazes.active` and `mazes.inactive`
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
