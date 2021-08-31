all:
	dune build bin/Synduce.exe

doc:
	dune build @doc
	cp -r _build/default/_doc/_html/* docs/