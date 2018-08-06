
all:
	dune build bin/awale.exe
	mv -f ./_build/default/bin/awale.exe ./awale.exe

js:
	dune build bin/awale_js.bc.js
	mv -f ./_build/default/bin/awale_js.bc.js ./awale.js

clean:
	dune clean
	rm -f ./awale.exe ./awale.js