import matplotlib.pyplot as plt

import numpy as np

def theta3(distance,time_steps,tol=1e-4,Nmax=100):

	# ------------------------------------------------------------------------ #
	# 
	#   Ref: Thambynayagam 2011 The Diffusion Handbook.
	#   Elliptic theta function of the third kind, page 6 and 7
	# 
	#   Two forms of the theta function and their integrals are given. Both
	#   forms are valid over the whole range of time, but convergence is more
	#   rapid in the specified regions of the argument exp(-pi^2*t).
	# 
	#   order = 1 -> elliptic theta function of third kind
	#   order = 2 -> integral of elliptic theta function of third kind
	# 
	#   N: truncation of summation
	# 
	# ------------------------------------------------------------------------ #

	for step in time_steps:

		argument = np.exp(-np.pi**2*step)

		if argument>1/np.pi:

			n1 = 0
			s1 = 0

			while True:

				n1 += 1

				newterm = np.exp(-n1**2*np.pi**2*step)*np.cos(2*n1*np.pi*distance)

				s1 += newterm

				if not np.any(np.abs(newterm)>tol*np.abs(s1)):
					print("Convergence of elliptic theta function is obtained after {} iterations".format(n1))
					break

				if n1>Nmax:
					print("Elliptic theta function could not converge ...")
					break

			yield 1+2*s1

		else:

			n1 = 0
			n2 = 0

			s2 = np.exp(-(distance)**2/step)

			while True:

				n1 += 1
				n2 -= 1

				newterm1 = np.exp(-(distance+n1)**2/step)
				newterm2 = np.exp(-(distance+n2)**2/step)

				newterm = newterm1+newterm2

				s2 += newterm

				if not np.any(np.abs(newterm)>tol*np.abs(s2)):
					print("Convergence of elliptic theta function is obtained after {} iterations".format(n1))
					break

				if n1>Nmax:
					print("Elliptic theta function could not converge ...")
					break

			yield 1./np.sqrt(np.pi*step)*s2

if __name__ == "__main__":

	X = np.linspace(0,10,1000,dtype=np.float64)
	T = np.linspace(0,1,100,dtype=np.float64)[1:]

	iterator = theta3(X,T,Nmax=1000)

	for t in T:

		print("Current time step is {}".format(t))

		theta = next(iterator)

	plt.plot(X,theta)

	plt.show()


