##Exercise: Theoretical Semi-Variogram Models

V = variogram(None)

V.type = 'spherical'

V.nugget = 0
V.sill = 10
V.range = 10

x = np.linspace(0,20,100)

V.set_theoretical(bins=x)

plt.plot(x,V.theoretical)

plt.show()
