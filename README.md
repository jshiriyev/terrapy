# bhospy

# All codes work only in SI units

"/bhospy/flow/" contains:

    -- single-phase common solutions:
    ---- flow on plates, falling film and slit flow
    ---- flow through a circular tube, pipe flow and annular flow
    ---- flow from a tank through nozzle
    ---- flow in a packed bed
    ---- flow through porous media including fractured porous media
    -- single-phase diffusivity equation analytical solution:
    ---- 1D cartesian solution (transient, steady, different boundary conditions)
    ---- 1D radial solution (transient, steady, different boundary conditions)
    ---- 3D gaussian solution (transient, different boundary conditions)
    -- multi-phase common solutions:
    ---- liquid-gas, liquid-liquid, liquid-solid flow
    ---- flow through porous media: Buckley-Leverett Solution
    
    numerical (computational) solutions
    -- finite difference solution
    ---- 3D cartesian regular mesh generation
    ---- 3D cylindrical rectangular mesh generation
    ---- 3D spherical rectangular mesh generation
    ---- 3D central difference solution for differential order of one and two

"/bhospy/geomech/" contains:

    numerical (computational) solutions
    -- finite element solution
    ---- 2D triangular mesh in rectangular domain
	
"/bhospy/statistics/" contains:

    nominal study
    -- uncertainty
    -- heterogeneity
    -- multivariate
    ---- correlation
    ---- linear regression
    ---- monte-carlo simulation
    
    spatiotemporal study
    -- connectivity
    ---- variogram
    -- spatial estimation
    ---- interpolation
    ---- moving average
    ---- kriging simple
    ---- kriging ordinary
    ---- gaussian simulation
	
"/bhospy/logging/" contains:

    analytical solutions
    
    numerical (computational)
    -- axial hybrid method
    -- surface integral equations
    -- volume integral equations
	
"/bhospy/optimize/" contains:

    simulated annealing
