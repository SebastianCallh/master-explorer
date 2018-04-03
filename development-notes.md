# Development Notes

This markdown contains some scattered notes of problems and eventual solutions I've experienced during development.

## Nix
Wrestling with Nix took quite a while. To get Dante to work the .emacs needed to be properly configured, which is currently hard coded to the project subpath of the reflex project, which provides the enironment and Reflex dependencies. The `default.nix` needs to contains all projects, and so does the `cabal.project`.

## JSaddle
When running the app in jsaddle-warp the program spontaneously crashes with `Error : Unexpected Duplicate. syncCallbacks=True nBatch=35 nExpected=37` (example numbers) which is awful. This turned out to be a bug in Firefox. Running the app in chromium solved the issue.