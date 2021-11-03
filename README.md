# UU-INFOFP-Game

[Click here to browse all build artifacts.](https://s3.cerbus.nl/minio/uu-infofp-game/)

## Setup development environment (Debian & friends)

```sh
# Install dependencies
sudo apt-get install freeglut3 libsdl2-ttf-dev

# Setup stack
stack setup

# Build and run the game
stack run

# Automatically build game when source changes
stack build --file-watch
```

## Setup development environment (Windows)

```sh
# Install dependencies
stack exec -- pacman -Syu
stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_ttf
# Download and place them next to UU-INFOFP-Game-exe.exe
# - https://www.libsdl.org/release/SDL2-devel-2.0.16-mingw.tar.gz
# - https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-devel-2.0.15-mingw.tar.gz

# Setup stack
stack setup

# Build and run the game
stack run

# Automatically build game when source changes
stack build --file-watch
```

## Optional stuff

```sh
# Install hlint
stack install hlint

# Install stuff needed for debugging
stack install haskell-dap ghci-dap haskell-debug-adapter

# Show FPS counter in console
LIBGL_SHOW_FPS=1 stack exec UU-INFOFP-Game-exe

# Profile the game performance
# Make sure to exit the game from the main menu by navigating to Quit. Exiting from the console, or by closing the window does NOT work.
stack --profile run --rts-options -p
```

## How to use the map builder

### Setup

Install the [Tiled](https://www.mapeditor.org/) software.

_Note: map and level are used interchangeably throughout this document. They mean the same thing._

### How to create a level

Open the Tiled Map Editor and click on "New" -> "New Map". Pick the following settings and then save your map in the `design\tiled` folder as `YourMapName.tmx`.

```
Orientation: Orthogonal
Tile layer format: CSV
Tile render order: Right Down

Map Size: Fixed
Width: 72 tiles
Height: 42 tiles
Width/Height: 8 px
```

Next you should click on "Map" -> "Map Properties" and add the `Name` property. Here you can enter the display name of your map.
To export your level click "File" -> "Export As" and save it as a `YourMapName.xml` file inside the `assets/levels` folder.

### Level Specification

A level consists of zero or more tile layers, and at most one object layer. The tile layers should be named `Foreground` or `Background` to show up in the game.
Tile layers with another name will not be rendered. The name of the object layer does not matter. See the (Level Objects)[] section for more info about the objects.

### Map Properties

Properties are case sensitive and optional, unless noted otherwise.

- Name: Level name, required!
- Foreground: Level foreground image, should be the name of a file inside the `assets/images` directory (without the file extension).
- Background: Level background image, same rules as above.

### Level Objects

#### Collision Object

- Name: Collision
- Behavior: The player and the enemies collide with this object.

#### Enemy Collision Object

- Name: EnemyCollision
- Behavior: Only enemies collide with this object.

#### Death Object

- Name: Death
- Behavior: The player and enemies will die when they touch this object. Not yet implemented.

#### Enemy Spawner Object

- Name: EnemySpawner
- Properties:
  - Type: Enemy type. Possible values: `Crab`. Not yet implemented.
- Behavior: Will spawn an enemy once (or infinitely idk yet).

## Credits

- #haskell on irc.libera.chat
- [Space War Man: platform shmup set](https://opengameart.org/content/space-war-man-platform-shmup-set)
- [Denzi explosion for Space War Man](https://opengameart.org/content/denzi-explosion-for-space-war-man)
- [Cethiel's Desert Redux as a 16-bit space background](https://opengameart.org/content/cethiels-desert-redux-as-a-16-bit-space-background)
- [Sci-fi User Interface Elements](https://opengameart.org/content/sci-fi-user-interface-elements)
