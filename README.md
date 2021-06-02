# bhospy

All codes work only in SI units

"/bhospy/flow/" contains:

    plate flow
    -- falling film
    -- parallel plates (flow through slit)
    
    cylindrical flow
    -- through pipe
    ---- liquid-gas interaction
    ---- liquid-liquid interaction
    -- through annulus
    -- through nozzle
    
    spherical flow
    -- creeping flow around solid sphere
    
    tank flow
    -- through hole
    -- through nozzle
    
    packed bed flow
    
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
    -- uncertainty estimation
    ---- jacknife
    ---- bootstrap
    ---- monte-carlo simulation
    -- heterogeneity
    ---- coefficient of variation
    ---- coefficient of correlation
    ---- hypothesis testing
    -- multivariate prediction
    ---- linear regression
    ---- ridge regression
    ---- k-nearest-neighbors
    ---- artificial neural network
    ---- support vector machine
    
    spatiotemporal study
    -- connectivity
    ---- variogram
    -- spatial estimation
    ---- interpolation 3D
    ---- kriging simple
    ---- kriging ordinary
    ---- gaussian simulation
    -- temporal foreasting
    ---- interpolation 1D
    ---- moving average
    
"/bhospy/logging/" contains:

    analytical solutions
    
    numerical (computational)
    -- axial hybrid method
    -- surface integral equations
    -- volume integral equations
	
"/bhospy/optimize/" contains:

    simulated annealing
