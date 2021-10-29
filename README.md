# UU-INFOFP-Game

[Click here to download the game.](https://s3.cerbus.nl/minio/uu-infofp-game/)

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

## Credits

- #haskell on irc.libera.chat
- [Space War Man: platform shmup set](https://opengameart.org/content/space-war-man-platform-shmup-set)
- [Denzi explosion for Space War Man](https://opengameart.org/content/denzi-explosion-for-space-war-man)
- [Cethiel's Desert Redux as a 16-bit space background](https://opengameart.org/content/cethiels-desert-redux-as-a-16-bit-space-background)
- [Sci-fi User Interface Elements](https://opengameart.org/content/sci-fi-user-interface-elements)
