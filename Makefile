all:
	dune build ./bin/gdf.exe
	mv bin/gdf.exe bin/gdf

clean:
	dune clean
	rm ./bin/gdf