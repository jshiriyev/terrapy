import numpy as np

from scipy.special import erf

if __name__ == "__main__":
    import setup

class kind3():

    # ------------------------------------------------------------------------ #
    # 
    #   Ref: Thambynayagam 2011 The Diffusion Handbook.
    #   Elliptic theta function of the third kind, page 6 and 7
    # 
    #   Two forms of the theta function and their integrals are given. Both
    #   forms are valid over the whole range of time, but convergence is more
    #   rapid in the specified regions of the argument exp(-pi^2*t).
    # 
    # ------------------------------------------------------------------------ #

    def __init__(self,distance,time_steps,tol=1e-4,Nmax=100):

        #   tol:	tolerance criteria to follow in convergnece condition
        #   Nmax:	truncation of summation if convergence is not achieved

        self.distance	= distance
        self.steps		= time_steps
        self.tol		= tol
        self.nmax		= Nmax

    def function(self):

        for step in self.steps:

            argument = np.exp(-2*np.pi**2*step)

            if argument<1/np.pi:

                n1 = 0
                s1 = 0

                while True:

                    n1 += 1

                    newterm = np.exp(-n1**2*np.pi**2*step)*np.cos(2*n1*np.pi*self.distance)

                    s1 += newterm

                    if not np.any(np.abs(newterm)>self.tol*np.abs(s1)):
                        # print("Convergence of elliptic theta function is obtained after {} iterations".format(n1))
                        break

                    if n1>self.nmax:
                        print("Elliptic theta function could not converge ...")
                        break

                yield 1+2*s1

            else:

                n1 = 0
                n2 = 0

                s2 = np.exp(-(self.distance)**2/step)

                while True:

                    n1 += 1
                    n2 -= 1

                    newterm1 = np.exp(-(self.distance+n1)**2/step)
                    newterm2 = np.exp(-(self.distance+n2)**2/step)

                    newterm = newterm1+newterm2

                    s2 += newterm

                    if not np.any(np.abs(newterm)>self.tol*np.abs(s2)):
                        # print("Convergence of elliptic theta function is obtained after {} iterations".format(n1))
                        break

                    if n1>self.nmax:
                        print("Elliptic theta function could not converge ...")
                        break

                yield 1./np.sqrt(np.pi*step)*s2

    def integral(self):

        for step in self.steps:

            argument = np.exp(-np.pi**2*step)

            if argument<1/np.pi:

                n1 = 0
                s1 = 0

                while True:

                    n1 += 1

                    newterm = 1/n1*np.exp(-n1**2*np.pi**2*step)*np.sin(2*n1*np.pi*self.distance)

                    s1 += newterm

                    if not np.any(np.abs(newterm)>self.tol*np.abs(s1)):
                        # print("Convergence of elliptic theta function integral is obtained after {} iterations".format(n1))
                        break

                    if n1>self.nmax:
                        print("Elliptic theta function integral could not converge ...")
                        break

                yield self.distance+1/np.pi*s1

            else:

                n1 = 0
                n2 = 0

                s2 = erf(self.distance/np.sqrt(step))

                while True:

                    n1 += 1
                    n2 -= 1

                    newterm1 = erf((self.distance+n1)/(np.sqrt(step)))-erf(n1/np.sqrt(step))
                    newterm2 = erf((self.distance+n2)/(np.sqrt(step)))-erf(n2/np.sqrt(step))

                    newterm = newterm1+newterm2

                    s2 += newterm

                    if not np.any(np.abs(newterm)>self.tol*np.abs(s2)):
                        # print("Convergence of elliptic theta function integral is obtained after {} iterations".format(n1))
                        break

                    if n1>self.nmax:
                        print("Elliptic theta function integral could not converge ...")
                        break

                yield 1/2*s2

if __name__ == "__main__":

    import matplotlib.pyplot as plt

    X = np.linspace(0,10,10000,dtype=np.float64)
    T = np.linspace(0,0.2,20,dtype=np.float64)[1:]

    theta = kind3(X,T,Nmax=1000)

    iterator1 = theta.function()
    iterator2 = theta.integral()

    for t in T:

        print("Current time step is {}".format(t))

        func = next(iterator1)
        intg = next(iterator2)

    plt.plot(X,func,label="function")
    plt.plot(X,intg,label="integral")

    plt.legend()

    plt.show()
