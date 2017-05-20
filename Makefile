all: runtime.class bayesc.native

runtime.class:
	javac runtime/Runtime.java

bayesc.native:
	ocamlbuild bayesc.native -lib unix -I extlib-1.5.3

clean:
	rm -Rf _build
	rm runtime/*.class
	rm bayesc.native
