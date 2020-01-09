# Codam Clustermap
Map displaying who's active inside the clusters.

## Elm
The front-end is written in Elm. The elm code compiles to javascript. More info
on elm: elm-lang.org

### Installing Elm
You can install Elm from the website: https://guide.elm-lang.org/install/elm.html
or using brew.
```
brew install elm
```

### Compiling projects
After installation you can compile the projects by going into the projects folder
and running something like this.
```
elm make src/Main.elm --output=../public_html/js/interactive_maps.js
```
But it's easier to just use the Makefile commands.

## Running localy
You can run the projects localy by starting a webserver in the public_html
folder.
```
python3 -m http.server
OR
php -S localhost:8000
```
After that you can go to the .html file corresponding with the project, 
for example: http://0.0.0.0:8000/interactive.html
