Write-Output "Running stack build..."
stack build --ghc-options -O2 --copy-bins --local-bin-path "./dist"

Write-Output "Copy assets..."
Copy-Item -Path "./assets" -Destination "./dist" -Recurse -Force -Verbose

Write-Output "Copy dependencies..."
Copy-Item -Path "./deps/win64/*" -Destination "./dist" -Force -Verbose
