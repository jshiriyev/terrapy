#include "resControl.h"
#include <iostream>

void resControl::setXlength(double x){xLength = x;}
void resControl::setYlength(double x){yLength = x;}
void resControl::setZlength(double x){zLength = x;}
void resControl::setPorosity(double x){porosity = x;}
void resControl::setTotComp(double x){totCompressibility = x;}
void resControl::setFluidVisc(double x){fluidViscosity = x;}
void resControl::setXperm(double x){xPermeability = x;}
void resControl::setYperm(double x){yPermeability = x;}
void resControl::setZperm(double x){zPermeability = x;}
void resControl::setInitPress(double x){initPressure = x;}

void resControl::calculateProps()
{
	poreComp = porosity*totCompressibility;
	fluidTime = poreComp*fluidViscosity;
}

double resControl::getXlength(){return xLength;}
double resControl::getYlength(){return yLength;}
double resControl::getZlength(){return zLength;}
double resControl::getPorosity(){return porosity;}
double resControl::getTotComp(){return totCompressibility;}
double resControl::getFluidVisc(){return fluidViscosity;}
double resControl::getXperm(){return xPermeability;}
double resControl::getYperm(){return yPermeability;}
double resControl::getZperm(){return zPermeability;}
double resControl::getInitPress(){return initPressure;}
double resControl::getXdiffusivity(){return xPermeability/fluidTime;}
double resControl::getYdiffusivity(){return yPermeability/fluidTime;}
double resControl::getZdiffusivity(){return zPermeability/fluidTime;}
double resControl::getPoreSpaceComp(){return 1/(4*poreComp*xLength*yLength);}
