# Standard library imports

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

#Local application imports


class diffusivity():

    def __init__(self,hydraulic_diffusivity):
        
        self.eta = hydraulic_diffusivity

    def cartesian_1D(self,boundary_points,boundary_pressures,time,x=None,N=50):

        xL = boundary_points[0]
        xU = boundary_points[1]

        PL = boundary_pressures[0]
        PU = boundary_pressures[1]

        L = xU-xL

        self.time = time.reshape((-1,1,1))

        if x is None:
            self.x = np.linspace(xL,xU).reshape((1,-1,1))
        else:
            self.x = x.reshape((1,-1,1))

        n = np.arange(1,N).reshape((1,1,-1))

        sin_ = np.sin(n*np.pi*self.x/L)
        exp_ = np.exp(-n**2*np.pi**2/L**2*self.eta*self.time)

        sum_ = np.sum(1/n*exp_*sin_,axis=2)

        self.pressure = PL+(PU-PL)*((self.x/L).reshape((1,-1))+2/np.pi*sum_)

    def radial_1D(self):
        pass

class poisson():

    def __init__(self):
        pass

    def onedimensional(self,boundary_points,boundary_conditions,beta):
        
        """
        lower_boundary is a list of boundary conditions specified for xL
        upper_boundary is a list of boundary conditions specified for xU

        they contain following entries

        x  : x-value of boundary line
        d  : Dirichlet boundary condition coefficient
        n  : Neumann boundary condition coefficient
        f  : scalar function known for the boundary    
        """

        xL = boundary_points[0,0]
        dL = boundary_conditions[0][0]
        nL = boundary_conditions[0][1]
        fL = boundary_conditions[0][2]

        xU = boundary_points[1,0]    
        dU = boundary_conditions[1][0]
        nU = boundary_conditions[1][1]
        fU = boundary_conditions[1][2]

        rhs = np.zeros((2,1))

        rhs[0,0] = fL-dL*beta/2*xL**2-nL*beta*xL
        rhs[1,0] = fU-dU*beta/2*xU**2-nU*beta*xU

        mat = np.zeros((2,2))

        mat[0,0] = dL*xL+nL
        mat[0,1] = dL
        mat[1,0] = dU*xU+nU
        mat[1,1] = dU

        coeff = np.linalg.solve(mat,rhs)

        c1 = coeff[0]
        c2 = coeff[1]

        dx = (xU-xL)/1000
        
        self.x = np.arange(xL,xU+dx/2,dx)

        self.u = beta/2*self.x**2+c1*self.x+c2

if __name__ == "__main__":

##    k = 50 #mD
##    k *= 0.986923e-15
##
##    phi = 0.15
##    mu = 1e-3
##    c = 2e-9
##    
##    sp = diffusivity(k/(phi*mu*c))
##
##    PL = 2000 #psi
##    PU = 3500 #psi
##
##    PL *= 6894.76
##    PU *= 6894.76
##
##    sp.cartesian_1D((0,50),(PL,PU),np.array([60,600]),x=np.array([12,27,41]),N=4)
##
##    print(sp.pressure/6894.76)

    beta = 10

    bound_points = np.array([[0],[7]])

    bound_conditions = np.array([[1,0,300],[0,1,0]])

    analytical = poisson()
    
    analytical.onedimensional(bound_points,bound_conditions,beta)
    
    plt.plot(analytical.x,analytical.u,'k',label='Analytical Solution')

    plt.tight_layout()

    plt.show()
