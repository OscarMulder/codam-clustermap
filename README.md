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

## Using the dragmap
The dragmap is a tool which allows you to position the icons in the correct place
on the map. You can run it localy and it doesn't require any configuration.

After starting the http server it should be available at 
http://0.0.0.0:8000/interactive.html. The dragmap needs 2 uploads to work:
- A svg map image
- A list of hosts

You can upload these by simply dragging them on the upload button, just make sure
to drag them separately and not together. If both files are valid it will display
the map and the icons, and you can drag them in place. After dragging you can
download a json file which you can use with the clustermap.

The hostfile can be in different formats:
```
[
    {
        "hostname": "f1r3s2.codam.nl"
    },
    {
        "hostname": "f1r1s11.codam.nl"
    },
    {
        "hostname": "f1r3s4.codam.nl"
    }
]
```
```
[
    {
        "hostname": "f1r1s10.codam.nl",
        "left": 899,
        "top": 696
    },
    {
        "hostname": "f1r4s19.codam.nl",
        "left": 516,
        "top": 376
    },
    {
        "hostname": "f1r4s20.codam.nl",
        "left": 468,
        "top": 333
    }
]
```
```
{
    "hosts":
    [
        {
        "hostname": "f1r1s10.codam.nl",
        "left": 899,
        "top": 696
        },
        {
        "hostname": "f1r4s19.codam.nl",
        "left": 516,
        "top": 376
        },
        {
        "hostname": "f1r4s20.codam.nl",
        "left": 468,
        "top": 333
        }
    ]
}

```
Or, like the output format:
```
{
  "mapsettings": {
    "heigth": 1325,
    "width": 1026,
    "active-size": 60,
    "empty-size": 20
  },
  "hosts":
  [
    {
      "hostname": "f1r1s10.codam.nl",
      "left": 899,
      "top": 696
    },
    {
      "hostname": "f1r4s19.codam.nl",
      "left": 516,
      "top": 376
    },
    {
      "hostname": "f1r4s20.codam.nl",
      "left": 468,
      "top": 333
    }
  ]
}
```
And any other combination of these formats should work (like mapsettings and a 
hostlist with only hostnames).

If you input the map in the output format, the dragmap will use the provided mapsettings 
for the display of the map. In the other formats it will use some default settings, 
which may not be optimal depending on the map svg. 
If the hosts already have a top and left value, make sure that they are set on 
the same mapsettings as in the file.