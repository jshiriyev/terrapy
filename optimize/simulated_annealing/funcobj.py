import numpy as np

def objective(m):
    
    nom = m.shape[0]
    
    inprod = np.sign(np.sinc(m))*np.power(np.abs(np.sinc(m)),1./4)
    energy = np.power(1.-np.prod(inprod,1),2.)
    
#   energy.shape = (nom,1)
    
    return energy
