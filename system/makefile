################################################################################
# System module makefile
# D. Everhart
# 02 JAN 2017
################################################################################
# The MIT License (MIT)
# 
# Copyright (c) 2017 Daniel Everhart
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the 
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
################################################################################
F77=gfortran
AR=ar
ARFLAGS=rcs
################################################################################
ifndef OBJDIR
#$(error OBJDIR not set)
OBJDIR=obj
endif
ifndef LIBDIR
#$(error LIBDIR not set)
LIBDIR=lib
endif
#ifndef INCDIR
#$(error INCDIR not set)
#endif

SRCDIR=src

FLAGS+=-g -J$(OBJDIR)
ifdef INCDIR
FLAGS+=-I$(INCDIR)
endif

system: $(OBJDIR) $(LIBDIR) $(LIBDIR)/libsystem.a

$(OBJDIR):
	mkdir -p $(OBJDIR)
$(LIBDIR):
	mkdir -p $(LIBDIR)
clean:
	rm -rf $(LIBDIR)
	rm -rf $(OBJDIR)
################################################################################
SRC=$(wildcard $(SRCDIR)/*.f)
F90=$(wildcard $(SRCDIR)/*.f90)
OBJ=$(patsubst $(SRCDIR)/%.f,$(OBJDIR)/%.o,$(SRC))
OBJ+=$(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(F90))
################################################################################
$(LIBDIR)/libsystem.a: $(OBJ)
	$(AR) $(ARFLAGS) $@ $^
################################################################################
$(OBJDIR)/%.o : src/%.f
	$(F77) $(FLAGS) -c $< -o $@
$(OBJDIR)/%.o : src/%.f90
	$(F77) $(FLAGS) -c $< -o $@
################################################################################
