class resControl
{
	public:
		void setXlength(double);
		void setYlength(double);
		void setZlength(double);
		void setPorosity(double);
		void setTotComp(double);
		void setFluidVisc(double);
		void setXperm(double);
		void setYperm(double);
		void setZperm(double);
		void setInitPress(double);
		void calculateProps();
		double getXlength();
		double getYlength();
		double getZlength();
		double getPorosity();
		double getTotComp();
		double getFluidVisc();
		double getXperm();
		double getYperm();
		double getZperm();
		double getInitPress();
		double getXdiffusivity();
		double getYdiffusivity();
		double getZdiffusivity();
		double getPoreSpaceComp();
	private:
		double xLength;
		double yLength;
        	double zLength;
        	double porosity;
		double totCompressibility;
        	double fluidViscosity;
        	double xPermeability;
        	double yPermeability;
        	double zPermeability;
        	double initPressure;
		double xDiffusivity;
		double yDiffusivity;
		double zDiffusivity;
		double poreComp;
		double fluidTime;
		double poreSpaceComp;

};
