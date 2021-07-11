import numpy as np

class objective:
    
    def __init__(self,m):
        
        self.m = m
        
    def calculate(self):
                
        inprod = np.sign(np.sinc(self.m))*np.power(np.abs(np.sinc(self.m)),1./4)
        energy = np.power(1.-np.prod(inprod,1),2.)
        
#       energy.shape = (nom,1)
        
        return energy

if __name__ == "__main__":

    m = np.array([[0.2,0.]])

    print(objective(m).calculate())
