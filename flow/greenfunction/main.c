#include "resControl.h"
#include <iostream>

int main()
{
	resControl res;

	res.setXlength(100.0);
	res.setYlength(100.0);
	res.setZlength(15.24);
	res.setPorosity(0.1);
	res.setTotComp(3e-6);
	res.setFluidVisc(0.6);
	res.setXperm(1e-3);
	res.setYperm(1e-3);
	res.setZperm(1e-3);
	res.setInitPress(4200.0);
	res.calculateProps();

	std::cout << res.getXlength() << std::endl;
	std::cout << res.getYlength() << std::endl;
	std::cout << res.getZlength() << std::endl;
	std::cout << res.getPorosity() << std::endl;
	std::cout << res.getTotComp() << std::endl;
	std::cout << res.getFluidVisc() << std::endl;
	std::cout << res.getXperm() << std::endl;
	std::cout << res.getYperm() << std::endl;
	std::cout << res.getZperm() << std::endl;
	std::cout << res.getInitPress() << std::endl;
	std::cout << res.getXdiffusivity() << std::endl;
	std::cout << res.getYdiffusivity() << std::endl;
	std::cout << res.getZdiffusivity() << std::endl;
	std::cout << res.getPoreSpaceComp() << std::endl;

	return 0;
}
