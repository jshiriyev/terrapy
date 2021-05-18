# bhospy

"/bhospy/flow/" contains:

    -analytical solutions
	common single-phase solutions:
	- flow of a falling film
	- flow between parallel plates
	- flow through a circular tube
	- flow through an annulus
	- flow from a tank
	- flow in a packed bed
	- flow through porous media
	- flow through fractured porous media
	analytical solution of diffusivity equation in all possible scenrarios:
	- 1D cartesian solution (transient, steady, different boundary conditions)
	- 1D radial solution (transient, steady, different boundary conditions)
	- 3D gaussian solution (transient, different boundary conditions)
	common multi-phase solutions:
	- liquid-gas, liquid-liquid, liquid-solid flow
	- flow through porous media: Buckley-Leverett Solution
    -numerical(computational)
	regular mesh generation
	- 3D cartesian grids
	- 3D cylindrical grids
	- 3D spherical grids
	finite difference solution
	- 3D central difference solution for differential order of one and two
	finite element solution
	
"/bhospy/statistics/" contains:

    -nominal analysis
	heterogeneity measures
	correlation measures
	principal component analysis
	uncertainty estimation
    -spatial analysis
	spatial continuity measures
	 - variogram
	spatial estimation
	 - kriging
	 - gaussian simulation
    -temporal analysis
    -spatiotemporal analysis
	
"/bhospy/logging/" contains:

    -analytical solutions
    -numerical (computational)
	axial hybrid method
	surface integral equations
	volume integral equations
	
"/bhospy/optimize/" contains:

    -simulated annealing
