// Analytical solution of reservoir to fracture flow
// ref: Thambynayagam 2011 The Diffusion Handbook.
//
// Green array is structure as [observer]x[source]x[time steps]
// Elliptic Theta Function of Third Kind, ref: page 6.
// Integral of Elliptic Theta Function of Third Kind, ref: page 7
//
// Two form of theta functions are given. Both forms are valid over 
// the whole range of time, but convergence is more rapid in the 
// specified regions of the argument exp(-pi^2*t)

#include "resControl.H"
#include "fracControl.H"
#include "numControl.H"
#include <iostream>

class greenFunc
{
	public:
		double array(const struct frac&, res&);
	private:
		double Xx1;
		double Xx2;
		double Xy1;
		double Xy2;
		double Xy3;
		double Xy4;
		double Xz1;
		double Xz2;
	
	public:

	double array(const frac& frac, res& res)
	{
		Xx1 = (frac.centerXcoord-frac.p1Xcoord);
		Xx2 = (frac.centerXcoord+frac.p1Xcoord);

		Xy1 = (frac.centerYcoord-frac.p1Ycoord);
		Xy2 = (frac.centerYcoord+frac.p1Ycoord);
		Xy3 = (frac.centerYcoord-frac.p2Ycoord);
		Xy4 = (frac.centerYcoord+frac.p2Ycoord);

		Xz1 = (frac.centerZcoord-res.zLength);
		Xz2 = (frac.centerZcoord+res.zLength);

		return (Xx1+Xx2)*(Xy1-Xy2-Xy3+Xy4)*(-Xz1+Xz2);

	}
};


