# compiler selection
FC = gfortran

# compiler option: Common Options
FFLAGS_COMMON = -ffree-line-length-none -fimplicit-none -pedantic -std=f2008 -Wall -Werror -Wextra

# compiler option: Release mode
FFLAGS_RELEASE = ${FFLAGS_COMMON} -O2

# compiler option: Debug mode
FFLAGS_DEBUG = ${FFLAGS_COMMON} -O0 -s -fbacktrace -fbounds-check -g

# target of compilation
TARGET = ./melg607-64.exe

# object codes
OBJS = \
	./melg_64_utility.o \
	./melg_64_test.o \
	./melg607_64.o \
	./main.o

# suffix rule
.SUFFIXES:
	.o .f90

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) -o $@ $(OBJS)

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

%.o: ../../src/%.f90
	$(FC) $(FFLAGS) -c $<

clean:
	rm ./*.exe ./*.mod ./*.o ./*.smod

debug_mode:
	make FFLAGS="$(FFLAGS_DEBUG)"

release_mode:
	make FFLAGS="$(FFLAGS_RELEASE)"
