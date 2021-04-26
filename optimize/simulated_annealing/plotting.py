import numpy as np
import matplotlib.pyplot as plt

class historyPlot:
    
    def __init__(self,m_min,m_max,models,energy):
        
        self.m_min = m_min
        self.m_max = m_max
        
        self.nog = models.shape[0]
        self.nom = models.shape[2]
        self.nop = models.shape[1]

        self.models = models
        self.energy = energy

        self.min_energy = energy.min(1)
        self.idx_of_min = energy.argmin(axis=1)
        
        self.min_param = np.zeros([self.nog,self.nop])

        for i in range(self.nog):
            self.min_param[i,:] = self.models[i,:,self.idx_of_min[i]]
        
    def plot_error(self):
        
        plt.figure(num='Error Evolution',figsize=[6.72,5.04])
        
        l1 = plt.semilogy(range(1,self.nog+1),self.min_energy, \
                          'o',ms=4,mec='r',mfc='r')
        
        for i in range(0,self.nom,5):
            l2 = plt.semilogy(range(1,self.nog+1),self.energy[:,i], \
                          'o',ms=4,mec='k',mfc='none')
        
        plt.legend(['best model','other models'],loc='lower center', \
                   numpoints=1,ncol=2,prop={'size':11})
        
        plt.xlabel('iteration number')
        plt.ylabel('objective function')

        plt.xlim([0,self.nog])

        plt.show()
        
        return
        
#    def map_1D(self):
        
#        plt.figure(num='1D Model Inversion',figsize=[5.6,4.2])

#       plt.show()

#        return

#    def map_2D(self):
        
#        plt.figure(num='2D Model Inversion',figsize=[5.6,4.2])
        
#        plt.show()
        
#        return
