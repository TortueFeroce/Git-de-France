main:
	ocamlfind ocamlopt -linkpkg -package cmdliner -o git-de-france gdf.ml
	./git-de-france