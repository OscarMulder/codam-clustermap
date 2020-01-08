interactive:
	cd _elm-clustermap-interactive && elm make src/Main.elm --optimize --output=../public_html/js/interactive_maps.js

interactive-debug:
	cd _elm-clustermap-interactive && elm make src/Main.elm --output=../public_html/js/interactive_maps.js

interactive-uglify: interactive
	uglifyjs public_html/js/interactive_maps.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=public_html/js/interactive_maps.js && uglifyjs public_html/js/interactive_maps.js --mangle --output=public_html/js/interactive_maps.js

dragmap:
	cd _elm-dragmap && elm make src/Main.elm --output=../public_html/js/dragmap.js