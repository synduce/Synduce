all:
	dune build bin/ReFunS.exe
	ln -s _build/default/bin/ReFunS.exe ReFunS