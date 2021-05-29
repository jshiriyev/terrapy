## Exercise: Heterogeneity Measures
    
parf = os.path.dirname(os.getcwd())

data = np.loadtxt(str(parf)+"\\"+"sample.txt",skiprows=1)

p = data[:,0]
k = data[:,1]
t = np.ones_like(k)

pm = heterogeneity({"porosity":p,
                    "permeability":k,
                    "thickness":t
                    })

print(pm.standard("permeability"))
print(pm.dykstraparson("permeability"))
print(pm.lorenz())

##w1 = np.loadtxt("well1.txt",skiprows=1)
##w2 = np.loadtxt("well2.txt",skiprows=1)
##
##d1 = w1[:,0]
##d2 = w2[:,0]
##
##t1 = d1-np.append(np.array([0]),d1[:-1])
##t2 = d2-np.append(np.array([0]),d2[:-1])
##
##hw1 = heterogeneity(permeability=w1[:,2],porosity=w1[:,1],thickness=t1)
##hw2 = heterogeneity(permeability=w2[:,2],porosity=w2[:,1],thickness=t2)
