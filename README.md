# UU-INFOFP-Game

## Setup development environment

```sh
# Install dependencies (Ubuntu)
sudo apt-get install freeglut3

# Setup stack
stack setup

# Install hlint
stack install hlint

# Build and run the game
stack run

# Automatically build game when source changes
stack exec uu-infofp-game
```

## Todo

- Maybe use ghcid instead of stack build (for speed)
