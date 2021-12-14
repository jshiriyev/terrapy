import numpy as np

def incident(k1,R1,R2):

    # R1 is the observation point
    # R2 is the source point
    # orientation of source is on the z direction
    
    r[:][0] = R1[:][0]-R2[0]
    r[:][1] = R1[:][1]-R2[1]
    r[:][2] = R1[:][2]-R2[2]
    
    R = np.sqrt(np.sum(r.*r,2))
    
    return R

R1 = np.array([1,0,0];[])
R2 = np.array([0,1,0])

Rn = gauss(R1,R2,R3)

print Rn

	
