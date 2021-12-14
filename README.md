# BhosPy

All codes work only in SI units

Flow analysis contains:

- **Plates:** flow including plates, falling film, parallel plates with stationary plates, parallel plates with one moving plate
- **Pipes:** flow through cylinders, pipes and annulus
- **Holes:** flow through orifice, nozzle
- **Spheres:**
- **PackBed:** flow through packed bed
- **PorMed:** flow through porous media: core sample, reservoir model, reservoir with complex fractures

GeoMech contains:

- **Pore Pressure Estimation:**

Interfaces:

- **2D Graphical View:**
- **3D Graphical View:**
- **Tabular View:**
- **Data Reading Packages:**

MathBox:

- **Unit Conversion:**

Numerics:

- **FinDiff:** Finite Difference Packaga
- **FinElm:** Finite Element Package

Optimize contains:

- **Simulated Annealing:**

ResLog contains:

- **AHM:** 2D wellbore conductivity simulation with axial hybrid method
- **SIE:** 2D plane simulation with surface integral equations
- **VIE:** 3D layer simulation volume integral equations

Statistics contains:

1) Nominal study
- **analytics:** heterogeneity measurements, correlation coefficients, principal component analysis, clustering algorithms (k-mean)
- **inference:** hypothesis testing (z,t,f,chi-squared)
- **estimation:** regression (linear+ridge), k-nearest-neighbors, artificial neural network, support vector machine
- **prediction:** jacknife, bootstrap, monte-carlo simulation

2) Spatiotemporal study
- **connectivity:** variogram
- **spatial estimation models:** 3D-interpolation, kriging simple, kriging ordinary
- **spatial prediction models:** gaussian simulation
- **temporal foreasting models:** 1D-interpolation, moving average
