# endlessSkyShips

This is a free time project to parse out data about ships and ship components for a specific video game (Endless Sky), and then write that data out as json.  It expects to find the game installed at /Applications/Endless Sky.app, and it was only ever run on a Mac.  As a free time project, this wasn't written to a professional standard (cough no tests at all), but it should at least show that I have some knowledge of Haskell.

## To Run:

stack build
stack exec endlessSkyShips-exe
