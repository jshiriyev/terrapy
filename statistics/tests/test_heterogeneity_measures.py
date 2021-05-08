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
