# makefile: makes the Axial program

#Axial: GlobalVarbls.o INDAT.o PostProc.o AxialNMM.o
#	gfortran -o Axial GlobalVarbls.o Indat.o \
#		 PostProc.o AxialNMM.o
Axial:  AxialNMM.o
	gfortran -o Axial AxialNMM.o
# Mirt

#PostProc.o: LibraryPGE/PostProc.f90
#	gfortran -c LibraryPGE/PostProc.f90

#GlobalVarbls.o: GlobalVarbls.f90
#	gfortran -c GlobalVarbls.f90

#INDAT.o: Indat.f90
#	gfortran -c Indat.f90

AxialNMM.o: AxialNMM.f90
	gfortran -c AxialNMM.f90
