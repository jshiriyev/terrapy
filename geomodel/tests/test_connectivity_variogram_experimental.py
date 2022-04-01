## Exercise: Experimental Variogram

z = np.array([[32,24,20,10],
              [28,20,17,12],
              [12,16,10,9],
              [18,12,7,8]])

V = variogram({"porosity": z},dX=10,dY=10)

V.set_distance(V)

V.set_experimental("porosity",10,30,azimuth=-90,azimuth_tol=2)
print(V.experimental)
V.set_experimental("porosity",20*np.sqrt(2),20*np.sqrt(2),azimuth=-135,azimuth_tol=2)
print(V.experimental)
