## Class Exercise 2

x = np.array([600,400,800])
y = np.array([800,700,100])

z = np.array([0.25,0.43,0.56])

V = variogram({"porosity": z},X=x,Y=y)

V.type = 'spherical'
V.nugget = 0
V.sill = 0.0025
V.range = 700

V.set_connection()

V.set_theoretical()

plt.figure(1)

plt.scatter(V.x,V.y,s=20,c=V.porosity,alpha=0.5)
plt.colorbar()

plt.xlim([0,1000])
plt.ylim([0,1000])

plt.xlabel('x-axis')
plt.ylabel('y-axis')

##    plt.show()

##    x = np.array([500])
##    y = np.array([500])

##    E = kriging(V,X=x,Y=y)

##    E.simple_kriging()

xlin = np.linspace(0,1000,50)
ylin = np.linspace(0,1000,50)

[Xmesh,Ymesh] = np.meshgrid(xlin,ylin)

E = spatial_estimation(V,X=Xmesh.flatten(),Y=Ymesh.flatten())

##    E.mean = 0.38

##    E.simple_kriging("porosity")

E.gaussian_simulation("porosity")

plt.figure(2)

plt.contourf(Xmesh,Ymesh,E.property.reshape(50,50));
plt.colorbar()

plt.xlabel('x-axis')
plt.ylabel('y-axis')

plt.xlim([0,1000])
plt.ylim([0,1000])

plt.show()
