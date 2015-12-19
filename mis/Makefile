
# GNU Fortran

FC = gfortran

FFLAGS += -g -w
FFLAGS += -fno-range-check
FFLAGS += -fno-automatic
FFLAGS += -fcray-pointer

# Objects

SRCF = $(wildcard *.f)
OBJS = $(patsubst %.f,%.o,$(SRCF))

# Default target

default:	libnastran.a

all:	libnastran.a

# Specific targets

libnastran.a:	$(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# Template rules

%.o:	%.f
	$(FC) $(FFLAGS) -c -o $@ $<

# Phony rules

.PHONY:	clean distclean

distclean:	clean
	-rm libnastran.a

clean:
	-rm *.o
