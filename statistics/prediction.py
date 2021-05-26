"""Standard library imports"""
import os
import sys

"""Third party imports"""
import matplotlib.pyplot as plt
import numpy as np

from scipy.stats import norm

class dataReader(object):
    
    def __init__(self, filename, header_lines=0):
        
        self.filename = filename
        self.header_lines = header_lines
        
        self.get_labels()
        self.format_data()
        
        return
    
    def read_file(self):
        
        with open(self.filename) as f:
            
            data = f.readlines()[self.header_lines:]
        
        return [data[i:i+4] for i in range(0, len(data), 4)]
    
    
    def get_labels(self):
        
        with open(self.filename) as f:
            
            header = f.readlines()[:self.header_lines]
            
        occurance_count = 0
        self.labels = []
        for line_number in range(len(header)):
            if 'LINE' in header[line_number].strip():
                line1 = header[line_number + 1]
                line2 = header[line_number + 2]
                self.labels.extend((line1 + line2).strip().split())
                occurance_count += 1
            if occurance_count == 4:
                break
                
        return
    
    def format_sample(self, sample_data):
        
        clean_data = []
        
        for data in sample_data:
            clean_data.extend(data.strip().split())
        
        return clean_data
    
    def format_data(self):
        
        data = self.read_file()
        
        data =  pd.DataFrame([self.format_sample(sample_data) for sample_data in data], columns=self.labels)
        
        #Clean up data using Panda's dataframe functions
        data = data.replace('.', np.nan)
        data = data.apply(pd.to_numeric, errors='ignore')
        
        #Drop the rows where KLH is NaN because these are our training values
        self.data = data.drop(data[data['KLH'].isna()].index)

    
    def get_dataframe(self):
        return self.data

class item():
    
    """
    statistical item with a spatial property

    ...
    Attributes
    ----------
    
    Methods
    -------
    set_property():
        assigns input properties to the self
    """

    def __init__(self,prop,**kwargs):
        
        self.set_property(prop,**kwargs)

    def set_property(self,prop,X=None,Y=None,Z=None,dX=1,dY=1,dZ=1):
        """it creates best x,y,z values for the given property"""

        if prop is not None:
            ones = np.ones(prop.shape)
            self.property = prop.ravel()
        elif X is not None:
            ones = np.ones(X.shape)
        elif Y is not None:
            ones = np.ones(Y.shape)
        elif Z is not None:
            ones = np.ones(Z.shape)
        else:
            return
        
        if X is None:
            try:
                self.x = (np.cumsum(ones,0)-1).ravel()*dX
            except:
                self.x = ones.ravel()
        else:
            self.x = X.ravel()

        if Y is None:
            try:
                self.y = (np.cumsum(ones,1)-1).ravel()*dY
            except:
                self.y = ones.ravel()
        else:
            self.y = Y.ravel()

        if Z is None:
            try:
                self.z = (np.cumsum(ones,2)-1).ravel()*dZ
            except:
                self.z = ones.ravel()
        else:
            self.z = Z.ravel()

class estimation(item):

    """
    estimation class used to represent estimated points

    ...

    Attributes
    ----------
    var:
    type:
    nugget:
    sill:
    range:
    
    distance:
    theoretical:
    covariance:
    lambdas:
    property:
    variance:
    mean:
    beta:
    
    Methods
    -------
    kriging_simple()
        calculates simple kriging values at estimation points
    kriging_ordinary()
        calculates ordinary kriging values at estimation points
    simulation_gaussian()
        calculates gaussian simulation values at estimation points
    
    """

    def __init__(self,var,**kwargs):
        """var must be theoretical variogram at observation points"""
        self.var = var

        self.type = self.var.type
        self.nugget = self.var.nugget
        self.sill = self.var.sill
        self.range = self.var.range
        
        self.set_property(None,**kwargs)

    def set_distance(self):
        """here distance is calculated in 2-dimensional array form"""
        self.dx = self.x-self.var.x.reshape((-1,1))
        self.dy = self.y-self.var.y.reshape((-1,1))
        self.dz = self.z-self.var.z.reshape((-1,1))

        self.distance = np.sqrt(self.dx**2+self.dy**2+self.dz**2)

    def interpolation(self,X,Y,x):
        """1D"""

        """
        X are the locations where Y values are given
        x are the locations where we want to calculate y
        based on the interpolation of Y values
        """
        
        xadded = X[x.max()<X]
        
        x = np.append(x,xadded)
        
        N = x.size
        L = X.size

        d1 = np.array(list(range(N)))
        d2 = np.array(list(range(N,N+L)))
        
        row = np.concatenate(((d1[0],d1[-1]),d1[:-1],d1[1:],d1[1:-1]))
        col = np.concatenate(((d1[0],d1[-1]),d1[1:],d1[:-1],d1[1:-1]))

        Mone = np.ones(2)*(-1)
        Pone = np.ones(2*(N-1))
        Mtwo = np.ones((N-2))*(-2)

        data = np.concatenate((Mone,Pone,Mtwo))

        G = sps.csr_matrix((data,(row,col)),shape=(N,N))
        d = sps.csr_matrix((Y,(d2,np.zeros(L))),shape=(N+L,1))

        x = x.reshape((1,-1))
        X = X.reshape((-1,1))
        
        Glow = np.zeros((L,N))

        dmat = np.abs(x-X)              # distance matrix for the given x and X vectors
        
        colID = dmat.argsort()[:,:2]    # column indices of two minimum row values in dmat
        rowID = np.tile(np.arange(L).reshape((-1,1)),2)
                                        # row indices of two minimum row values in dmat
        
        dmin = dmat[rowID,colID]        # two minimum distance values of each row in dmat

        Glow[rowID,colID] = 1-dmin/np.sum(dmin,axis=1,keepdims=True)

        G = sps.vstack([G,sps.csr_matrix(Glow)])

        A = G.transpose()*G
        b = G.transpose()*d

        y = sps.linalg.spsolve(A,b)

        if xadded.size:
            return y[:-xadded.size]
        else:
            return y

    """
    MATLAB interpolation
    
    fid=fopen('Data_all.bin');
    allData=fread(fid,'float');

    fid=fopen('Data_in.bin');
    d=fread(fid,'float');

    L=length(allData);
    idx=(1:L)';

    nonZero=allData(d~=0);
    nonZeroID=idx(d~=0);

    l=length(nonZero);

    %% Generation of the G matrix and d vector

    epslon=1;

    G=spalloc(L,L,(L-2)*(L-1));
    G=G+sparse(2:L-1,2:L-1,-2*epslon,L,L);
    G=G+sparse(1:L-1,2:L,epslon,L,L);
    G=G+sparse(2:L,1:L-1,epslon,L,L);
    G=G+sparse(1,1,-epslon,L,L);
    G=G+sparse(L,L,-epslon,L,L);

    G=[sparse(1:l,nonZeroID,1); G];
    d=[nonZero; spalloc(L,1,0)];

    m=(G'*G)\(G'*d);

    %% Plot of the Results

    figure(1)
    spy(G)
    title('Structure of the G matrix');
    ylabel('Dimensions of G is 125x100');

    figure(2)
    scatter(nonZeroID,nonZero,'r'); hold on
    plot(idx,allData,'b',idx,m,'m')
    title('Interpolation');
    legend('Dumped Data','Full Data','Interpolation',3);
    grid on
    """

    def moving_average(self):
        pass

    def kriging_simple(self,perc=0.5):
        
        "perc -> percentile, perc=0.5 gives mean values"

        self.set_distance()
        
        variogram.set_theoretical(self)
        
        self.covariance = self.sill-self.theoretical

        self.lambdas = np.linalg.solve(self.var.covariance,self.covariance)
        
        calc_diff_arr = np.reshape(self.var.property,(-1,1))-self.mean
        calc_prop_mat = self.lambdas*(calc_diff_arr)
        calc_vars_mat = self.lambdas*self.covariance
        
        self.property = self.mean+calc_prop_mat.sum(axis=0)
        self.variance = self.sill-calc_vars_mat.sum(axis=0)

        self.property = self.property+norm.ppf(perc)*np.sqrt(self.variance)

    def kriging_ordinary(self,perc=0.5):
        
        "perc -> percentile, perc=0.5 gives mean values"

        self.set_distance()
        
        variogram.set_theoretical(self)

        self.covariance = self.sill-self.theoretical
        
        Am = self.var.covariance
        Ar = np.ones(Am.shape[0]).reshape((-1,1))
        Ab = np.ones(Am.shape[0]).reshape((1,-1))
        Ab = np.append(Ab,np.array([[0]]),axis=1)
        Am = np.append(Am,Ar,axis=1)
        Am = np.append(Am,Ab,axis=0)

        bm = self.covariance
        bb = np.ones(bm.shape[1]).reshape((1,-1))
        bm = np.append(bm,bb,axis=0)

        xm = np.linalg.solve(Am,bm)
        
        self.lambdas = xm[:-1,:]
        self.beta = xm[-1,:]

        calc_prop_mat = self.lambdas*np.reshape(self.var.property,(-1,1))
        calc_vars_mat = self.lambdas*self.covariance

        self.property = calc_prop_mat.sum(axis=0)
        self.variance = self.sill-self.beta-calc_vars_mat.sum(axis=0)
        
        self.property = self.property+norm.ppf(perc)*np.sqrt(self.variance)

    def simulation_gaussian(self):

        perc = np.random.rand(self.x.size)

        self.ordinary_kriging(perc=perc)

if __name__ == "__main__":

    pass
