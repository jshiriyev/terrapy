# bhospy

All codes work only in SI units

"/bhospy/flow/" contains:

    cartesian flow
    -- falling film
    -- through slit (parallel plates)
    -- through orifice (tank flow)
    
    cylindrical flow
    -- through pipe
    -- through annulus
    -- through nozzle
    
    spherical flow
    -- around solid sphere
    -- through packed bed
    
    porous media flow
    -- core
    ---- 1D cartesian analytical solution of single-phase diffusivity equation (transient, steady, different boundary conditions)
    ---- both incompressible multi-phase Buckley-Leverett Solution
    -- reservoir
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

    nominal study
    -- heterogeneity measurements
    ---- variation  coefficients
    -- hypothesis testing
    -- univariate prediction
    ---- jacknife
    ---- bootstrap
    ---- monte-carlo simulation
    -- mutlivariate analysis
    ---- correlation coefficients
    -- multivariate estimation
    ---- regression (linear + ridge)
    ---- k-nearest-neighbors
    ---- artificial neural network
    ---- support vector machine
    
    spatiotemporal study
    -- connectivity measurements
    ---- variogram
    -- spatial estimation
    ---- 3D-interpolation
    ---- kriging simple
    ---- kriging ordinary
    -- spatial prediction
    ---- gaussian simulation
    -- temporal foreasting
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
