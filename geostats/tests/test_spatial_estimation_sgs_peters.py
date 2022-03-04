##Exercise 4.14 Peters, page 206 Volume 1 Sequential GAUSSIAN SIMULATION

x = np.array([1,2,4,5,6])
y = np.array([2,10,4,6,14])
z = np.array([15,30,25,18,30])

V = variogram({'porosity': z},X=x,Y=y)

V.set_distance(V)

V.type = 'spherical'
V.nugget = 0
V.sill = 60
V.range = 8

V.set_theoretical()

X = np.array([2,4])#([1,2,2,4,4,5,6])
Y = np.array([6,12])#([2,6,10,12,4,6,14])

E = simulation(V,X=X,Y=Y)

E.sequential_gaussian("porosity")
