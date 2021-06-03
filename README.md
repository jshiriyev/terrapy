# bhospy

All codes work only in SI units

"/bhospy/flow/" contains:

    1) flow including plates
    -- falling film
    -- parallel plates with stationary plates
    -- parallel plates with one moving plate
    2) flow through cylinders:
    -- pipe
    -- annulus
    3) flow through hole:
    -- orifice
    -- nozzle
    4) flow around solid sphere
    5) flow through packed bed
    6) flow through porous media
    -- core sample
    ---- 1D cartesian analytical solution of single-phase diffusivity equation (transient, steady, different boundary conditions)
    ---- both incompressible multi-phase Buckley-Leverett Solution
    -- reservoir model
    ---- 1D radial analytical solution of single-phase diffusivity equation (transient, steady, different boundary conditions)
    ---- 3D gaussian analytical line-solution of single-phase diffusivity equation (transient, different boundary conditions)
    ---- 3D finite difference solution, cenrtral difference, implicit solution
    -- reservoir with complex fractures
    ---- 3D gaussian analytical plane-solution of single-phase diffusivity equation (transient, different boundary conditions)

"/bhospy/geomech/" contains:

    numerical (computational) solutions
    -- finite element solution
    ---- 2D triangular mesh in rectangular domain
	
"/bhospy/statistics/" contains:

    1) nominal study
    -- visualization models
    -- learning models
    ---- heterogeneity measurements
    ---- correlation coefficients
    ---- principal component analysis
    ---- clustering algorithms (k-mean)
    -- inferential models
    ---- hypothesis testing (z,t,f,chi-squared)
    -- estimation models
    ---- regression (linear+ridge)
    ---- k-nearest-neighbors
    ---- artificial neural network
    ---- support vector machine
    -- prediction models
    ---- jacknife
    ---- bootstrap
    ---- monte-carlo simulation
    2) spatiotemporal study
    -- learning connectivity
    ---- variogram
    -- spatial estimation models
    ---- 3D-interpolation
    ---- kriging simple
    ---- kriging ordinary
    -- spatial prediction models
    ---- gaussian simulation
    -- temporal foreasting models
    ---- 1D-interpolation
    ---- moving average
    
"/bhospy/logging/" contains:

    analytical solutions
    
    numerical (computational)
    -- axial hybrid method
    -- surface integral equations
    -- volume integral equations
	
"/bhospy/optimize/" contains:

    simulated annealing
