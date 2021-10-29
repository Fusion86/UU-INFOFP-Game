Write-Output "Running stack setup..."
stack setup

Write-Output "Updating pacman..."
stack exec -- pacman -Syu --noconfirm

Write-Output "Installing pkg-config, SDL2, and SDL2_ttf"
stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_ttf --noconfirm
