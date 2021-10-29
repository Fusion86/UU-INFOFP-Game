Write-Output "Running stack build..."
stack build --ghc-options -O2 --copy-bins --local-bin-path "./dist"

Write-Output "Copy assets..."
Copy-Item -Path "./assets" -Destination "./dist" -Recurse -Force -Verbose

Write-Output "Copy libs..."
Copy-Item -Path "./lib/*" -Destination "./dist" -Force -Verbose
