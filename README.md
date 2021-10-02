# UU-INFOFP-Game

## Setup development environment

```sh
# Install dependencies (Ubuntu)
sudo apt-get install freeglut3 libsdl2-ttf-dev   

# Setup stack
stack setup

# Install hlint
stack install hlint

# Build and run the game
stack run

# Automatically build game when source changes
stack exec uu-infofp-game

# Install stuff needed for debugging
stack install haskell-dap ghci-dap haskell-debug-adapter
```

## Todo

- Maybe use ghcid instead of stack build (for speed)
