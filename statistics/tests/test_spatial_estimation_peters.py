## Example 4.2 (Kriging) and 4.3 (Simulation) page 187, Peters Volume 1

x = np.array([2,4,6])
y = np.array([30,50,20])

plt.grid(alpha=0.2)
plt.xlabel('x-axis',fontsize=14)
plt.ylabel('property',fontsize=14)

plt.xlim([0,9])
plt.ylim([0,70])

V = variogram({"porosity": y},X=x)

V.set_distance(V)

V.type = 'exponential'
V.nugget = 0
V.sill = 100
V.range = 10

V.set_theoretical()

X = np.array([1,2,3,4,5,6,7,8])

xe = np.linspace(1,8,701)

E = spatial_estimation(V,X=xe)

E.ordinary_kriging("porosity")

plt.plot(E.x,E.property,c='k')

E.ordinary_kriging("porosity",perc=0.975)

y1 = E.property

E.ordinary_kriging("porosity",perc=0.025)

y2 = E.property

plt.fill_between(E.x,y1,y2,fc='lightgrey')

xe = np.linspace(1,8,71)

E = spatial_estimation(V,X=xe)

E.gaussian_simulation("porosity")

plt.scatter(E.x,E.property,s=4,c='r')

plt.scatter(x,y,marker='X',c='k')

plt.show()
