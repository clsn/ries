GSLFLAGS=-DRIES_GSL `gsl-config --libs`
M64FLAGS=-DRIES_USE_SA_M64
ifdef RIES_GSL
XTRAFLAGS=$(GSLFLAGS)
else ifdef RIES_USE_SA_M64
XTRAFLAGS=$(M64FLAGS)
else
XTRAFLAGS=
endif
ries: ries.c
	cc -O2 -o $@ $< $(XTRAFLAGS) -lm

riesw: ries.c msal_math64.c
	cc -O2 -o $@ $< $(M64FLAGS) -lm

riesgsl: ries.c
	cc -O2 -o $@ $< $(GSLFLAGS) -lm

ries.man.txt: ries.1
	nroff -man $< > $@

ries.ps: ries.1
	groff -man -t -e $< > $@

.PHONY:
	clean
clean:
	rm -f ries riesw riesgsl ries.man.txt ries.ps
