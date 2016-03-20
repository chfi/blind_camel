#
# Compiles all lessons
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = 		corebuild $(OCB_FLAGS)
SOURCES = lesson1 lesson2

all: 		native # profile debug

clean:
			$(OCB) -clean

native: 	#sanity
			$(OCB) $(SOURCES:=.native)

byte:		sanity
			$(OCB) $(SOURCES:=.byte)

profile: 	sanity
			$(OCB) -tag profile $(SOURCES:=.native)

debug: 		sanity
			$(OCB) -tag debug $(SOURCES:=.byte)

sanity:
# check that packages can be found
			ocamlfind query core
			ocamlfind query tsdl

.PHONY: 	all clean byte native profile debug sanity test
