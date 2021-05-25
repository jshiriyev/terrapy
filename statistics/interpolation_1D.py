import numpy as np

import matplotlib.pyplot as plt

import scipy.sparse as sps

from geostatistics import csvreader

class data():
    pass

data = csvreader(data,'xydata.csv')

plt.scatter(data.x,data.y)

plt.show()

##import csv
##
##fid1 = open('Data_all.bin')
##Y1 = np.fromfile(fid1,dtype=np.float32)
##
##fid2 = open('Data_in.bin')
##Y2 = np.fromfile(fid2,dtype=np.float32)
##
##N = Y1.size
##
##X1 = np.linspace(1,N,N)
##
##Y = Y2[Y2!=0]
##X = X1[Y2!=0]
##
##np.savetxt("x.csv", X, delimiter=",")
##np.savetxt("y.csv", Y, delimiter=",")


##
##L = Y.size
##
##d0 = np.array(list(range(N-1)))
##d1 = np.array(list(range(1,N)))
##
##d2 = np.array(list(range(N,N+L)))
##
##row = np.concatenate(((d0[0],d1[-1]),d0,d1,d0[1:],d2))
##col = np.concatenate(((d0[0],d1[-1]),d1,d0,d0[1:],X-1))
##
##Mone = np.ones(2)*(-1)
##Pone = np.ones(2*(N-1))
##Mtwo = np.ones((N-2))*(-2)
##Eone = np.ones(L)
##
##data = np.concatenate((Mone,Pone,Mtwo,Eone))
###m = sps.csr_matrix(([3,5],([2,1],[1,3])));
##
##G = sps.csr_matrix((data,(row,col)),shape=(N+L,N))
##d = sps.csr_matrix((Y,(d2,np.zeros(L))),shape=(N+L,1))
##
##A = np.dot(G.todense().transpose(),G.todense())
##b = np.dot(G.todense().transpose(),d.todense())
##
##x = np.linalg.solve(A,b)
##
####print(G.todense())
##
####plt.scatter(X1,Y1)
##
##plt.scatter(X,Y,label='actual data')
####plt.plot(X1,x,c='r',label='interpolated data')
##
####plt.legend(fontsize=12)
##
##plt.show()
