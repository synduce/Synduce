all:
	dune build bin/Synduce.exe

doc:
	dune build @doc
	cp -rf _build/default/_doc/_html/* docs/