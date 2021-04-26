import numpy as np

def gauss(R1,R2,R3):
    xw = np.array([1./3,1./3,1.])
    
    N1 = 1.-xw[1]-xw[0]
    N2 = xw[1]
    N3 = xw[0]
    
    Rn = R1*N1+R2*N2+R3*N3
    return Rn

R1 = np.array([0,0,0])
R2 = np.array([1,0,0])
R3 = np.array([0,1,0])

Rn = gauss(R1,R2,R3)

print Rn

	
