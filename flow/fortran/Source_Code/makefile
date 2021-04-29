# Start of the makefile
# Defining Variables

objects = Global.o Indat.o Boundary.o Staticprop.o Postpro.o Resulator.o \
	Relperm.o Wellprop.o

# funcobj = reader.o

SIMULATOR: $(objects)
	gfortran -o SIMULATOR $(objects)

Global.o: Global.f90
	gfortran -c Global.f90

Indat.o: Indat.f90
	gfortran -c Indat.f90

Boundary.o: Boundary.f90
	gfortran -c Boundary.f90

Staticprop.o: Staticprop.f90
	gfortran -c Staticprop.f90

Relperm.o: Relperm.f90
	gfortran -c Relperm.f90

Wellprop.o: Wellprop.f90
	gfortran -c Wellprop.f90

Postpro.o: Postpro.f90
	gfortran -c Postpro.f90

Resulator.o: Resulator.f90
	gfortran -c Resulator.f90

# Cleaning everything

clean:
	rm global.mod
	rm $(objects)
	rm SIMULATOR
	rm *~

# End of the makefile
