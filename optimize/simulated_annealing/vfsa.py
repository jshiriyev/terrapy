import sys
import numpy as np

from funcobj import objective

class annealing:
    
    def __init__(self,m_min,m_max,nog,nom,nop):
        
        self.m_min = m_min
        self.m_max = m_max
        
        self.nog = nog
        self.nom = nom
        self.nop = nop
        
    def temperature(self,tmp_type):
        
        Tinit = 1.
        Tfnal = 1.e-2
        
        M = self.nog
        e = np.array(range(1,M+1))
        
        if tmp_type == 'straight':
           tmp = Tinit+(Tfnal-Tinit)/(M-1.)*(e-1.)
        elif tmp_type ==  'geometric':
           tmp = Tinit*(Tfnal/Tinit)**((e-1.)/(M-1.))
        elif tmp_type == 'reciprocal':
           tmp = Tinit*Tfnal*(M-1)/(Tfnal*M-Tinit+(Tinit-Tfnal)*e)
        elif tmp_type == 'logarithmic':
           tmp = Tinit*Tfnal*np.log((M+1)/2)/(Tfnal*np.log(M+1)- \
                 Tinit*np.log(2)+(Tinit-Tfnal)*np.log(e+1))
        
        return tmp
        
    def iterating(self,tmp):
        
        models = np.zeros([self.nom,self.nop,self.nog])
        energy = np.zeros([self.nog,self.nom]) 
        
        #np.random.seed(1)
        #R1 = np.random.rand(self.nom*self.nop).reshape(self.nop,self.nom).T
        R1 = np.random.rand(self.nom,self.nop)
        
        m1 = self.m_min+R1*(self.m_max-self.m_min)
        E1 = objective(m1)
        
        models[:,:,0] = m1
        energy[0,:] = E1
        
        m1,E1 = self.selection(m1,E1)
        
        for i in range(1,self.nog):
           
           m2 = self.offspring(m1,tmp[i])
           E2 = objective(m2)
           
           models[:,:,i] = m2
           energy[i,:] = E2
            
           dE = E2-E1
           
           #np.random.seed(2)
           R2 = np.random.rand(self.nom)
           
           pA = np.logical_or(dE<=0,tmp[i]>R2)
            
           m1[pA,:] = m2[pA,:]
           E1[pA] = E2[pA]
        
           m1,E1 = self.selection(m1,E1)
        
        return np.transpose(models,(2,1,0)),energy
        
    def selection(self,m,E):
        
        for n in range(self.nom/2+1):
        
           id_min = np.argmin(E)
           id_max = np.argmax(E)
        
           m[id_max,:] = m[id_min,:]
           E[id_max] = E[id_min]
           
        return m,E
        
    def offspring(self,m_old,tmp_i):
        
        deltam = np.tile(self.m_max-self.m_min,[self.nom,1])
        
        m_new = m_old.copy()
        
        out = np.ones((self.nom,self.nop),dtype=bool)
        
        for ntry in range(100):
           
           #np.random.seed(ntry+1)
           #R3 = np.random.rand(self.nom*self.nop).reshape(self.nop,self.nom).T
           R3 = np.random.rand(self.nom,self.nop)
           pole = np.sign(R3-0.5)
           
           #np.random.seed(ntry+1)
           #R4 = np.random.rand(self.nom*self.nop).reshape(self.nop,self.nom).T
           R4 = np.random.rand(self.nom,self.nop)
           jump = R4*tmp_i

           m_new[out] = m_old[out]+pole[out]*jump[out]*deltam[out]
 
           out = np.logical_or(m_new<self.m_min,m_new>self.m_max)

           if np.all(~out):
              break
           
        if ntry == 99:
           sys.exit("ERROR: could not find search point")
           
        return m_new
