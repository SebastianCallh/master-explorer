# Development Notes

This markdown contains some scattered notes of problems and eventual solutions I've experienced during development.

## Nix
Wrestling with Nix took quite a while. To get Dante to work the .emacs needed to be properly configured, which is currently hard coded to the project subpath of the reflex project, which provides the enironment and Reflex dependencies. The `default.nix` needs to contains all projects, and so does the `cabal.project`.

## JSaddle
When running the app in jsaddle-warp the program spontaneously crashes with `Error : Unexpected Duplicate. syncCallbacks=True nBatch=35 nExpected=37` (example numbers) which is awful. This turned out to be a bug in Firefox. Running the app in chromium solved the issue.

### Filtering dynamic
Something very odd happened when I tried to filter a list inside a dynamic. Instead of displaying the filtered items it displayed the n first items, where n is the length of the filtered list. It had something to do with the usage of `sample . current` and filtering several items in a frame. It seems that the built in functions to generate DOM elements only gets you so far. I had to resort to creating them manually using `elDynAttr'`, which worked brilliantly.

### The Scraper
The scraper does not parse all course pages successfully so the selection of courses is not complete. However I have decided to continue developing the web application firstly and then fix this issue when time permits.


### Missing URLS
Some courses does not have urls in the course list, such as Avancerad programvarudesign, Maskininlärning, planering och reglering fär autonoma farkoster, and Avancerad programvarudesign. 