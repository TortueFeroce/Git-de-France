all:
	dune build ./bin/gdf.exe
	mv bin/gdf.exe bin/gdf
	export PATH="${PATH}:bin"