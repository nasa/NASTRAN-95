################################################################################
F77=gfortran
AR=ar
FLAGS+=-g -fno-range-check -fno-automatic -std=legacy -Iinclude -Llib
################################################################################
all:       jobs nastran nasthelp nastplot chkfil ff
nasinfo:   NASINFO
nastran:   obj bin libnas nasinfo bin/nastran.x
libnas:    lib libnasmis libnasmds libnasbd libnassys lib/libnas.a
libnasmis: lib lib/libnasmis.a
libnasmds: lib lib/libnasmds.a
libnasbd:  lib lib/libnasbd.a
libnassys: lib lib/libnassys.a
nasthelp:  obj bin bin/nasthelp.x
nastplot:  obj bin bin/nastplot.x
chkfil:    obj bin bin/chkfil.x
ff:        obj bin bin/ff.x
OUTPUT:
	mkdir -p OUTPUT
lib:
	mkdir -p lib
obj:
	mkdir -p obj
bin:
	mkdir -p bin
	cp um/*.TXT bin
clean:
	rm -rf bin obj lib
	rm -rf OUTPUT
	rm -f testnas.*
	rm -f dsnames.*
	rm -f NASINFO COS* RSCARDS
	rm -f fort.*
################################################################################
MISOBJ+=$(patsubst mis/%.f,obj/%.o,$(wildcard mis/*.f))
MDSOBJ+=$(patsubst mds/%.f,obj/%.o,$(wildcard mds/*.f))
BDOBJ+=$(patsubst bd/%.f,obj/%.o,$(wildcard bd/*.f))
################################################################################
lib/libnassys.a:
	make -C system LIBDIR=../lib OBJDIR=../obj INCDIR=../include
lib/libnasmis.a: $(MISOBJ)
	$(AR) cr $@ $^
lib/libnasmds.a: $(MDSOBJ)
	$(AR) cr $@ $^
lib/libnasbd.a: $(BDOBJ)
	$(AR) cr $@ $^
lib/libnas.a: lib/libnasmis.a lib/libnasmds.a lib/libnasbd.a lib/libnassys.a
	$(AR) crT $@ $^
bin/nastran.x: obj/nastrn.o
	$(F77) $(FLAGS) $^ -lnas -o $@    # Note that "-lnas" is after "$^"!
bin/nasthelp.x: obj/nasthelp.o
	$(F77) $(FLAGS) $^ -o $@
bin/nastplot.x: obj/nastplot.o
	$(F77) $(FLAGS) $^ -o $@
bin/ff.x: obj/ff.o 
	$(F77) $(FLAGS) $^ -lnas -o $@
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
