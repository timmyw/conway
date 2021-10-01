
.phone: clean

all: conway

SRC=conway.ml

conway: $(SRC)
	ocamlopt -o $@ $<

clean:
	rm -f *.o *.cmi *.cmx
	rm -f conway
