## SOME PYTHON TIPS ON THE IDLE USE:
##
## PYTHON IS INDENTATION AND CASE SENSITIVE,
## DO NOT LEAVE UNNECESSARY SPACES IN THE BEGINNING OF LINE
## TO DEDENT, SELECT THE LINES YOU WANT TO DEDENT AND CLICK CTRL+[
## TO INDENT, SELECT THE LINES YOU WANT TO INDENT AND CLICK CTRL+]
## PAY ATTENTION TO THE UPPER AND LOWER CASES YOU ARE USING FOR THE VARIABLE NAMES
##
## COMMENT OUT MEANS ADDING HASHTAGS IN THE BEGINNING OF LINE
## TO COMMENT OUT, SELECT THE LINES YOU WANT TO COMMENT OUT AND CLICK ALT+3
## UNCOMMENT OUT MEANS REMOVING HASHTAGS FROM THE BEGINNING OF LINE
## TO UNCOMMENT OUT, SELECT THE LINES YOU WANT TO UNCOMMENT OUT AND CLICK ALT+4
## 
## FOR THE SECTION STARTING AFTER [if __name__ == "__main__":],
## EVERY LINE SHOULD BE INDENTED FOUR SPACES

import io

import numpy as np
from scipy.sparse import csr_matrix

import matplotlib.pyplot as plt
## if you want to generate animation, install PIL and uncomment out the line 25 and animation part in the bottom
## for the installation, go to the command prompt and type pip install Pillow
##from PIL import Image

class buckleyleverett():

    def __init__(self,Sor,Swr,muo,muw):

        self.Sor = Sor
        self.Swr = Swr
        self.muo = muo
        self.muw = muw

    def k_model(self):

        N = 1000

        self.Sw = np.linspace(self.Swr,1-self.Sor,N)

        self.kro = 2*(1-self.Sw-self.Sor)**2
        self.krw = (self.Sw-self.Swr)**3

    def coreymodel(self,koro,korw,m,n):

        N = 1000

        self.Sw = np.linspace(self.Swr,1-self.Sor,N)

        S = (self.Sw-self.Swr)/(1-self.Swr-self.Sor)

        self.kro = koro*(1-S)**m
        self.krw = korw*S**n

        ## end-point mobility ratio calculation
        self.Mo = (korw/self.muw)/(koro/self.muo)

    def fractionalflow(self):

        self.fw = (self.krw*self.muo)/(self.krw*self.muo+self.kro*self.muw)
        
        N = self.fw.size

        one = np.ones(N-1)

        idx = np.array(list(range(N)))

        row = np.concatenate(((idx[0],idx[-1]),idx[:-1],idx[1:]))
        col = np.concatenate(((idx[0],idx[-1]),idx[1:],idx[:-1]))

        val = np.concatenate(((-1,1),one,-one))

        G = csr_matrix((val,(row,col)),shape=(N,N))

        fw_diff = G*self.fw
        Sw_diff = G*self.Sw

        self.fw_der = fw_diff/Sw_diff
        
    def shockfront(self,Swi):

        self.Swi = Swi

        IC = self.Sw>=self.Swi

        ## loosing some data in fw, Sw and fw_der for the saturations below the initial value
        self.fw_IC = self.fw[IC]
        self.Sw_IC = self.Sw[IC]
        ## fw_der_IC is the fw_der corrected for the shock front as well
        self.fw_der_IC = self.fw_der[IC]
        
        self.fwi = self.fw_IC[0]
        
        idx = np.argmax(self.fw_der_IC)

        fw_dh = self.fw_IC[idx:]
        Sw_dh = self.Sw_IC[idx:]

        pseudo_fw_der = (fw_dh-self.fwi)/(Sw_dh-self.Swi)

        self.fwf = fw_dh[np.argmin(np.abs(self.fw_der_IC[idx:]-pseudo_fw_der))]
        self.Swf = Sw_dh[np.argmin(np.abs(self.fw_der_IC[idx:]-pseudo_fw_der))]

        self.Sw_avg = self.Swi+(1-self.fwi)/(self.fwf-self.fwi)*(self.Swf-self.Swi)

        fw_der_corrected = np.empty_like(self.fw_der_IC)

        fw_der_corrected[self.Sw_IC>=self.Swf] = self.fw_der_IC[self.Sw_IC>=self.Swf]
        fw_der_corrected[self.Sw_IC<self.Swf] = self.fw_der_IC[self.Sw_IC==self.Swf]

        self.fw_der_IC = fw_der_corrected

        self.fwf_der = self.fw_der_IC[0]

    def production(self,q,A,L,phi):

        """
        given that L is "ft", A is "ft2", and q is "ft3/day", tp will be in days.
        phi is porosity and is a dimensionless value.
        calculated volumes are normalized with respect to pore volume Vp,
        both produced oil Np and injected water Wi
        """

        v = q/(phi*A)

        self.v = v

        self.Vp = A*L*phi
        
        self.tbt = L/(v*self.fwf_der)

        self.Nbt = v/L*(1-self.fwi)*self.tbt

        self.tp = L/(v*self.fw_der_IC)
        self.Np = (1-self.fw_IC)/(self.fw_der_IC)+self.Sw_IC-self.Swi

        idx = self.tp<=self.tbt

        N = np.count_nonzero(idx)

        self.tp[idx] = np.linspace(0,self.tbt,N)
        self.Np[idx] = v/L*(1-self.fwi)*self.tp[idx]
        
        self.Wi = v/L*self.tp

    def profile(self,q,A,phi,t):

        """
        time is in days assuming that L is "ft", A is "ft2", and q is "ft3/day".
        phi is porosity and is a dimensionless value.
        calculated x_Sw for a saturation profile will be in "ft".
        """

        v = q/(phi*A)        

        self.x_Sw = v*t*self.fw_der_IC
    
if __name__ == "__main__":

## ALL CALCULATIONS ARE CARRIED IN BUCKLEYLEVERETT CLASS ABOVE
## BASED ON THE INPUT PROVIDED BELOW

    Sor = 0.25
    Swi = 0.15
    Swr = 0.15
##    koro = 1.0
##    korw = 0.78
##    m = 2.6
##    n = 3.7
    muo = 8.
    muw = 1.
    
    q = 1000*5.615
    A = 2500
    L = 1000
    phi = 0.2
    
    BL = buckleyleverett(Sor,Swr,muo,muw)
    
##    BL.coreymodel(koro,korw,m,n)
    BL.k_model()
    BL.fractionalflow()
    BL.shockfront(Swi)

    BL.production(q,A,L,phi)
    BL.profile(q,A,phi,BL.tbt*0.3)#0.02 is for the time

    Np = BL.Np*BL.Vp/5.615          # oil produced, bbl

    qo = np.empty_like(Np)
    qo[0] = Np[1]/BL.tp[1]
    qo[1:] = Np[1:]/BL.tp[1:]       # bbl/day
    
    qw = q/5.615-qo                 # bbl/day
    
    WOR = qw/qo

    WOR[WOR<1e-15] = 0

## INPUTS END HERE

## THE SECTION BELOW IS ONLY FOR PLOTTING AND ANIMATION
## FOR SHOWING A PLOT YOU WANT, UNCOMMENT OUT THE LINES ACCORDINGLY

## RELATIVE PERMEABILITY PLOT

##    plt.plot(BL.Sw,BL.kro)
##    plt.plot(BL.Sw,BL.krw)
##    plt.xlim([0,1])
##    plt.ylim(bottom=0)
##    plt.xlabel('water saturation',fontsize=14)
##    plt.ylabel('relative permeability',fontsize=14)
##    plt.legend(('oil','water'),fontsize=14)
##    plt.show()

## FRACTIONAL FLOW PLOT

##    plt.plot(BL.Sw,BL.fw,c='k')
##    plt.xlabel('water saturation',fontsize=14)
##    plt.ylabel('fractional water flow',fontsize=14)
##    plt.xlim((0,1))
##    plt.ylim((0,1))
####    plt.xticks([0,1])
####    plt.yticks([0,1])
##    plt.show()

## FRACTIONAL FLOW DERIVATIVE PLOT

####    A = (BL.Sw-BL.Swr)**3*BL.muo
####    B = A+2*(1-BL.Sw-BL.Sor)**2*BL.muw
####
####    C = 3*(BL.Sw-BL.Swr)**2*BL.muo
####    D = C-4*(1-BL.Sw-BL.Sor)*BL.muw
####
####    F = C/B-A/B**2*D
##
##    plt.plot(BL.Sw,BL.fw_der,c='k')
####    plt.plot(BL.Sw,F,'r--')
##    plt.xlabel('water saturation',fontsize=14)
##    plt.ylabel('derivative of water fractional flow',fontsize=14)
##    plt.xlim((0,1))
##    plt.ylim(bottom=0)
####    plt.xticks([0,1])
####    plt.yticks([])
##    plt.show()

## FRACTIONAL FLOW AND ITS DERIVATIVE TOGETHER PLOT

##    fig, ax1 = plt.subplots()
##
##    color = 'tab:blue'
##    ax1.plot(BL.Sw,BL.fw,color=color)
##    ax1.set_ylabel('water fractional flow',color=color)
##    ax1.set_xlabel('water saturation')
##    ax1.tick_params(axis='y', labelcolor=color)
##
##    ax2 = ax1.twinx()
##
##    color = 'tab:red'
##    ax2.plot(BL.Sw,BL.fw_der,color=color)
##    ax2.set_ylabel('water fractional flow derivative',color=color)
##    ax2.tick_params(axis='y', labelcolor=color)
##
##    fig.tight_layout()
##
##    plt.xlim((0,1))
##
##    plt.show()

## FRACTIONAL FLOW PLOT FOR THE SHOCK FRONT DETERMINATION

##    plt.plot(BL.Sw,BL.fw,c='k')
##    plt.plot((BL.Swi,BL.Sw_avg),(BL.fwi,1),c='r')
##    plt.xlabel('water saturation',fontsize=14)
##    plt.ylabel('fractional water flow',fontsize=14)
##    plt.xlim((0,1))
##    plt.ylim((0,1))
####    plt.xticks([0,1])
####    plt.yticks([0,1])
##    plt.show()

## WATER SATURATION PORFILE WITH RESPECT TO DISTANCE
    
##    x = np.insert(BL.x_Sw,0,2*L)
##    y = np.insert(BL.Sw_IC,0,BL.Swi)
##    plt.plot(x,y,'k')
##    plt.xlim((0,L))
##    plt.ylim((0,1))
##    plt.xlabel('x-direction',fontsize=14)
##    plt.ylabel('water saturation',fontsize=14)
##    plt.tight_layout()
##    plt.show()

## OIL PRODUCTION RATE VS INJECTED WATER
    
##    plt.plot(BL.Wi,BL.Np*BL.Vp/5.615/BL.tp)
##    plt.xlim([0,3])
##    plt.ylim(bottom=0)
##    plt.xlabel('Pore Volume of Water injected',fontsize=14)
##    plt.ylabel('Oil Production Rate [bbl/day]',fontsize=14)
##    plt.tight_layout()
##    plt.grid()
##    plt.show()

## WATER OIL RATIO VS CUMULATIVE OIL PRODUCTION

##    plt.semilogx(WOR,Np)
##    plt.ylim(bottom=90000)
##    plt.xlim(left=0.00001)
##    plt.xlabel('water oil ratio',fontsize=14)
##    plt.ylabel('total oil production [bbl]',fontsize=14)
##    plt.tight_layout()
##    plt.grid()
##    plt.show()

## CUMULATIVE OIL RECOVERY VS INJECTED WATER
    
##    plt.plot(BL.Wi,BL.Np)
##    plt.xlim([0,10])
##    plt.ylim(bottom=0)
##    plt.xlabel('Injected Pore Volume of Water',fontsize=14)
##    plt.ylabel('Produced Pore Volume of Oil',fontsize=14)
##    plt.tight_layout()
##    plt.grid()
##    plt.show()

## ANIMATION OF WATER SATURATION WITH RESPECT TO DISTANCE CHANGING WITH TIME WHILE SWEEPING OIL

##    ax = plt.subplot(1,1,1)
##
##    ax.set_xlim((0,L))
##    ax.set_ylim((0,1))
##    ax.set_xlabel('x-direction',fontsize=14)
##    ax.set_ylabel('water saturation',fontsize=14)
##    
##    plt.tight_layout()
##
##    N_tn = 100
##    
##    t_n = np.linspace(0,0.035,N_tn)
##
##    frames = []
##    
##    for i,t in enumerate(t_n):
##        
##        BL.profile(q,A,phi,t)
##        
##        x = np.insert(BL.x_Sw,0,2*L)
##        y = np.insert(BL.Sw_IC,0,BL.Swi)
##
##        line, = ax.plot(x,y,'k')
##
##        #below the image is created in the memory and appended to frames
##        buf = io.BytesIO()
##        plt.savefig(buf)
##        img = Image.open(buf)
##        frames.append(img)
##
##        line.remove()
##        
##    plt.close()
##    
##    #generates the animation from the images created
##    frames[0].save('buckley.gif',
##                   format='GIF',
##                   append_images=frames[1:],
##                   save_all=True,
##                   duration=50,
##                   loop=0)
