################################################################################
F77=gfortran
AR=ar
FLAGS+=-g -fno-range-check -fno-automatic -std=legacy -Iinclude
################################################################################
all:      jobs nastran nasthelp nastplot chkfil ff
nasinfo:  NASINFO
nastran:  obj bin nasinfo bin/nastran.x
nasthelp: obj bin bin/nasthelp.x
nastplot: obj bin bin/nastplot.x
chkfil:   obj bin bin/chkfil.x
ff:       obj bin bin/ff.x
OUTPUT:
	mkdir -p OUTPUT
obj:
	mkdir -p obj
bin:
	mkdir -p bin
	cp um/*.TXT bin
clean:
	rm -rf bin
	rm -rf obj
	rm -rf OUTPUT
	rm -f testnas.*
	rm -f *.mod
	rm -f dsnames.*
	rm -f NASINFO
	rm -f COS*
	rm -f RSCARDS
	rm -f fort.*
################################################################################
OBJ+=$(patsubst mis/%.f,obj/%.o,$(wildcard mis/*.f))
OBJ+=$(patsubst mds/%.f,obj/%.o,$(wildcard mds/*.f))
OBJ+=$(patsubst bd/%.f,obj/%.o,$(wildcard bd/*.f))
################################################################################
bin/nastran.x: obj/nastrn.o $(OBJ)
	$(F77) $(FLAGS) $^ -o $@
bin/nasthelp.x: obj/nasthelp.o
	$(F77) $(FLAGS) $^ -o $@
bin/nastplot.x: obj/nastplot.o
	$(F77) $(FLAGS) $^ -o $@
bin/ff.x: obj/ff.o $(OBJ)
	$(F77) $(FLAGS) $^ -o $@
bin/chkfil.x: obj/chkfil.o
	$(F77) $(FLAGS) $^ -o $@
NASINFO: rf/NASINFO
	cp $^ $@
################################################################################
obj/%.o : bd/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : mds/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : mis/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : src/%.f
	$(F77) $(FLAGS) -c $< -o $@
################################################################################
JOBS+=$(patsubst inp/%.inp,OUTPUT/%.f06,$(wildcard inp/*.inp))
################################################################################
COS=COSDBCL COSDDAM COSDFVA COSHYD1 COSHYD2 COSMFVA
jobs: nastran OUTPUT $(COS) $(JOBS)
################################################################################
COSDBCL: alt/COSDBCL
	ln -s $^ $@
COSDDAM: alt/COSDDAM
	ln -s $^ $@
COSDFVA: alt/COSDFVA
	ln -s $^ $@
COSHYD1: alt/COSHYD1
	ln -s $^ $@
COSHYD2: alt/COSHYD2
	ln -s $^ $@
COSMFVA: alt/COSMFVA
	ln -s $^ $@
################################################################################
OUTPUT/%.f06 : inp/%.inp
	./sbin/nastran.py -o OUTPUT $<
################################################################################
OUTPUT/d01011b.f06: inp/d01011b.inp OUTPUT/d01011a.f06
	rm -f RSCARDS
	ln -s OUTPUT/d01011a.dict RSCARDS
	./sbin/nastran.py -o OUTPUT --OPTPNM OUTPUT/d01011a.nptp $<
OUTPUT/d01011c.f06: inp/d01011c.inp OUTPUT/d01011a.f06
	rm -f RSCARDS
	ln -s OUTPUT/d01011a.dict RSCARDS
	./sbin/nastran.py -o OUTPUT --OPTPNM OUTPUT/d01011a.nptp $<
OUTPUT/d01021b.f06: inp/d01021b.inp OUTPUT/d01021a.f06
	rm -f RSCARDS
	ln -s OUTPUT/d01021a.dict RSCARDS
	./sbin/nastran.py -o OUTPUT --OPTPNM OUTPUT/d01021a.nptp $<
OUTPUT/d11011b.f06: inp/d11011b.inp OUTPUT/d11011a.f06
	rm -f RSCARDS
	ln -s OUTPUT/d11011a.dict RSCARDS
	./sbin/nastran.py -o OUTPUT --OPTPNM OUTPUT/d11011a.nptp $<
OUTPUT/t00001a.f06: inp/t00001a.inp
	./sbin/nastran.py -o OUTPUT --FTN15 inp/t00001a.inp1 --FTN16 inp/t00001a.inp2 $<
OUTPUT/t03111b.f06: inp/t03111b.inp OUTPUT/t03111a.f06
	rm -f RSCARDS
	ln -s OUTPUT/t03111a.dict RSCARDS
	./sbin/nastran.py -o OUTPUT --OPTPNM OUTPUT/t03111a.nptp $<
OUTPUT/t03121b.f06: inp/t03121b.inp OUTPUT/t03121a.f06
	rm -f RSCARDS
	ln -s OUTPUT/t03121a.dict RSCARDS
	./sbin/nastran.py -o OUTPUT --OPTPNM OUTPUT/t03121a.nptp $<
OUTPUT/t03121c.f06: inp/t03121c.inp OUTPUT/t03121a.f06
	rm -f RSCARDS
	ln -s OUTPUT/t03121a.dict RSCARDS
	./sbin/nastran.py -o OUTPUT --OPTPNM OUTPUT/t03121a.nptp $<
OUTPUT/t04021b.f06: inp/t04021b.inp OUTPUT/t04021a.f06
	rm -f RSCARDS
	ln -s OUTPUT/t04021a.dict RSCARDS
	./sbin/nastran.py -o OUTPUT --OPTPNM OUTPUT/t04021a.nptp $<
################################################################################
