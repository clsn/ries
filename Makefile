# This setup is all kind of dumb.  Sorry.
GSLFLAGS=-DRIES_GSL `gsl-config --libs`
M64FLAGS=-DRIES_USE_SA_M64
LDBLFLAGS=-DRIES_WANT_LDBL
ifdef RIES_GSL
XTRAFLAGS=$(GSLFLAGS)
else ifdef RIES_USE_SA_M64
XTRAFLAGS=$(M64FLAGS)
else ifdef RIES_WANT_LDBL
XTRAFLAGS=$(LDBLFLAGS)
else
XTRAFLAGS=
endif
ries: ries.c
	cc -O2 -o $@ $< $(XTRAFLAGS) -lm

riesw: ries.c msal_math64.c
	cc -O2 -o $@ $< $(M64FLAGS) -lm

riesgsl: ries.c
	cc -O2 -o $@ $< $(GSLFLAGS) -lm

riesldbl: ries.c
	cc -O2 -o $@ $< $(LDBLFLAGS) -lm

ries.man.txt: ./ries.1
	nroff -man -t $< > $@

ries.ps: ries.1
	groff -man -t -e $< > $@

.PHONY:	clean allries
clean:
	rm -f ries riesw riesgsl riesldbl ries.man.txt ries.ps
allries: ries riesw riesgsl riesldbl
