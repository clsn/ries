ries: ries.c
	cc -O2 -o $@ $< -DRIES_GSL `gsl-config --libs`

riesw: ries.c msal_math64.c
	cc -o $@ ries.c -DRIES_USE_SA_M64 -lm

ries.man.txt: ries.1
	nroff -man $< > $@

ries.ps: ries.1
	groff -man -t -e $< > $@

.PHONY:
	clean
clean:
	rm -f ries riesw ries.man.txt ries.ps
