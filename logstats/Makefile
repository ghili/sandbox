#cf http://caml.inria.fr/pub/old_caml_site/FAQ/Makefile_ocaml-fra.html

OCAMLDEP = ocamldep
OCAMLC =ocamlc
OCAMLOPT=ocamlopt

OPTIONS = -g -o
OCAMLFLAGS = 
OCAMLOPTFLAGS = $(OCAMLFLAGS)

RESULT= logstats
LIBS= str.cma unix.cma graphics.cma
INTERFACES = 
OBJECTS= logParsers.cmo graphLog.cmo main.cmo

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: %.ml
	$(OCAMLC) $(LIBS) $(OCAMLFLAGS)  -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS)  -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<


all: $(INTERFACES) $(OBJECTS)
	$(OCAMLC) $(LIBS) $(OCAMLFLAGS) $(OPTIONS)  $(RESULT) $(OBJECTS) 

clean:
	rm -f *.cm[iox]
	rm -f $(RESULT) 

depend: 
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend
