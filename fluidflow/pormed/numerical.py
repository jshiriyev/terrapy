import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse import csr_matrix as csr
from scipy.sparse import diags

from scipy.sparse.linalg import spsolve as sps

if __name__ == "__main__":
    import setup

from stream.items import Fluids

from stream.items import get_PorRock
from stream.items import get_Wells

class singlephase():
    
    """
    This class is supposed to generate regular grids in cartesian and
    radial coordinates and perform the given first or second order central
    derivative operation by generating A matrix and implementing boundary
    condition and solving for the given right hand side.
    """
    
    def __init__(self):
        
        self.PorRock = get_PorRock("rectangle")()

        # There can be two slightly compressible fluids where the
        # second one is at irreducible saturation, not mobile

        self.Fluids = Fluids(number=2)

        self.Wells = get_Wells()()

    def initialize(self,pressure0,Swirr=0,ctotal=None):

        self.pressure0 = pressure0

        self.Sw = Swirr
        self.So = 1-self.Sw

        if ctotal is None:
            coSo = self.Fluids.compressibility[0]*self.So
            cwSw = self.Fluids.compressibility[1]*self.Sw
            ctotal = coSo+cwSw+self.PorRock.compressibility

        self.compressibility = ctotal

        cons = self.PorRock.porosity*self.Fluids.viscosity[0]*self.compressibility

        self.diffusivity = (self.PorRock.permeability)/(np.reshape(cons,(-1,1)))

    def set_times(self,step,total):

        self.time_step = float(step)
        self.time_total = float(total)

        self.times = np.arange(step,total+step,step)

        self.pressure = np.zeros((self.PorRock.grid_numtot,self.times.size))

    def transmissibility(self):

        sizes = self.PorRock.grid_sizes

        indices = self.PorRock.grid_indices

        permeability = self.PorRock.permeability
        
        dx_m = (sizes[:,0]+sizes[indices[:,1],0])/2
        dx_p = (sizes[:,0]+sizes[indices[:,2],0])/2
        dy_m = (sizes[:,1]+sizes[indices[:,3],1])/2
        dy_p = (sizes[:,1]+sizes[indices[:,4],1])/2
        dz_m = (sizes[:,2]+sizes[indices[:,5],2])/2
        dz_p = (sizes[:,2]+sizes[indices[:,6],2])/2

        kx_m = (2*dx_m)/(sizes[:,0]/permeability[:,0]+
                         sizes[indices[:,1],0]/permeability[indices[:,1],0])
        kx_p = (2*dx_p)/(sizes[:,0]/permeability[:,0]+
                         sizes[indices[:,2],0]/permeability[indices[:,2],0])
        ky_m = (2*dy_m)/(sizes[:,1]/permeability[:,1]+
                         sizes[indices[:,3],1]/permeability[indices[:,3],1])
        ky_p = (2*dy_p)/(sizes[:,1]/permeability[:,1]+
                         sizes[indices[:,4],1]/permeability[indices[:,4],1])
        kz_m = (2*dz_m)/(sizes[:,2]/permeability[:,2]+
                         sizes[indices[:,5],2]/permeability[indices[:,5],2])
        kz_p = (2*dz_p)/(sizes[:,2]/permeability[:,2]+
                         sizes[indices[:,6],2]/permeability[indices[:,6],2])

        porosity = self.PorRock.porosity

        viscosity = self.Fluids.viscosity[0]

        etax_m = kx_m/(porosity*viscosity*self.compressibility)
        etax_p = kx_p/(porosity*viscosity*self.compressibility)
        etay_m = ky_m/(porosity*viscosity*self.compressibility)
        etay_p = ky_p/(porosity*viscosity*self.compressibility)
        etaz_m = kz_m/(porosity*viscosity*self.compressibility)
        etaz_p = kz_p/(porosity*viscosity*self.compressibility)

        self.transmissibility = np.zeros((self.PorRock.grid_numtot,6))

        self.transmissibility[:,0] = (2*etax_m)/(dx_m*(dx_m+dx_p))
        self.transmissibility[:,1] = (2*etax_p)/(dx_p*(dx_m+dx_p))
        self.transmissibility[:,2] = (2*etay_m)/(dy_m*(dy_m+dy_p))
        self.transmissibility[:,3] = (2*etay_p)/(dy_p*(dy_m+dy_p))
        self.transmissibility[:,4] = (2*etaz_m)/(dz_m*(dz_m+dz_p))
        self.transmissibility[:,5] = (2*etaz_p)/(dz_p*(dz_m+dz_p))

    def central(self,order=2):

        indices = self.PorRock.grid_indices

        noxmin = ~(indices[:,0]==indices[:,1])
        noxmax = ~(indices[:,0]==indices[:,2])
        noymin = ~(indices[:,0]==indices[:,3])
        noymax = ~(indices[:,0]==indices[:,4])
        nozmin = ~(indices[:,0]==indices[:,5])
        nozmax = ~(indices[:,0]==indices[:,6])

        id_noxmin = indices[noxmin,0] #id of grids not at xmin boundary
        id_noxmax = indices[noxmax,0] #id of grids not at xmax boundary
        id_noymin = indices[noymin,0] #id of grids not at ymin boundary
        id_noymax = indices[noymax,0] #id of grids not at ymax boundary
        id_nozmin = indices[nozmin,0] #id of grids not at zmin boundary
        id_nozmax = indices[nozmax,0] #id of grids not at zmax boundary

        idNnoxmin = indices[noxmin,1] #id of xmin neighbors for id_noxmin grids
        idNnoxmax = indices[noxmax,2] #id of xmax neighbors for id_noxmax grids
        idNnoymin = indices[noymin,3] #id of ymin neighbors for id_noymin grids
        idNnoymax = indices[noymax,4] #id of ymax neighbors for id_noymax grids
        idNnozmin = indices[nozmin,5] #id of zmin neighbors for id_nozmin grids
        idNnozmax = indices[nozmax,6] #id of zmax neighbors for id_nozmax grids

        cx_m = self.transmissibility[noxmin,0]
        cx_p = self.transmissibility[noxmax,1]
        cy_m = self.transmissibility[noymin,2]
        cy_p = self.transmissibility[noymax,3]
        cz_m = self.transmissibility[nozmin,4]
        cz_p = self.transmissibility[nozmax,5]

        shape = (self.PorRock.grid_numtot,self.PorRock.grid_numtot)

        self.Amatrix = csr(shape)

        self.Amatrix += csr((cx_m,(id_noxmin,idNnoxmin)),shape=shape)
        self.Amatrix -= csr((cx_m,(id_noxmin,id_noxmin)),shape=shape)
        
        self.Amatrix += csr((cx_p,(id_noxmax,idNnoxmax)),shape=shape)
        self.Amatrix -= csr((cx_p,(id_noxmax,id_noxmax)),shape=shape)
        
        self.Amatrix += csr((cy_m,(id_noymin,idNnoymin)),shape=shape)
        self.Amatrix -= csr((cy_m,(id_noymin,id_noymin)),shape=shape)
        
        self.Amatrix += csr((cy_p,(id_noymax,idNnoymax)),shape=shape)
        self.Amatrix -= csr((cy_p,(id_noymax,id_noymax)),shape=shape)
        
        self.Amatrix += csr((cz_m,(id_nozmin,idNnozmin)),shape=shape)
        self.Amatrix -= csr((cz_m,(id_nozmin,id_nozmin)),shape=shape)

        self.Amatrix += csr((cz_p,(id_nozmax,idNnozmax)),shape=shape)
        self.Amatrix -= csr((cz_p,(id_nozmax,id_nozmax)),shape=shape)
        
    def implement_bc(self,b_xmin=(0,1,0),b_xmax=(0,1,0),b_ymin=(0,1,0),b_ymax=(0,1,0),b_zmin=(0,1,0),b_zmax=(0,1,0)):

        """
        b_xmin,b_xmax,b_ymin,b_ymax,b_zmin and b_zmax have three entries:
        - dirichlet boundary condition coefficient,
        - neumann boundary condition coefficient,
        - function value of boundary condition
        Default is no flow boundary conditions at the exterior boundaries
        """

        sizes = self.PorRock.grid_sizes

        indices = self.PorRock.grid_indices

        self.b_correction = np.zeros(self.PorRock.grid_numtot)

        questbound = lambda x: True if x>1 else False

        if questbound(self.PorRock.grid_num[0]):

            xmin = indices[:,0]==indices[:,1]
            xmax = indices[:,0]==indices[:,2]

            id_xmin = indices[xmin,0]
            id_xmax = indices[xmax,0]

            dx_xmin = sizes[id_xmin,0]
            dx_xmax = sizes[id_xmax,0]

            tx_xmin = self.transmissibility[id_xmin,0]
            tx_xmax = self.transmissibility[id_xmax,1]

            bc_xmin = (2*tx_xmin*dx_xmin)/(b_xmin[0]*dx_xmin-2*b_xmin[1])
            bc_xmax = (2*tx_xmax*dx_xmax)/(b_xmax[0]*dx_xmax+2*b_xmax[1])
            
            self.Amatrix[id_xmin,id_xmin] -= bc_xmin*b_xmin[0]
            self.Amatrix[id_xmax,id_xmax] -= bc_xmax*b_xmax[0]
            
            self.b_correction[id_xmin] -= bc_xmin*b_xmin[2]
            self.b_correction[id_xmax] -= bc_xmax*b_xmax[2]

        if questbound(self.PorRock.grid_num[1]):

            ymin = indices[:,0]==indices[:,3]
            ymax = indices[:,0]==indices[:,4]

            id_ymin = indices[ymin,0]
            id_ymax = indices[ymax,0]

            dy_ymin = sizes[id_ymin,1]
            dy_ymax = sizes[id_ymax,1]

            ty_ymin = self.transmissibility[id_ymin,2]
            ty_ymax = self.transmissibility[id_ymax,3]

            bc_ymin = (2*ty_ymin*dy_ymin)/(b_ymin[0]*dy_ymin-2*b_ymin[1])
            bc_ymax = (2*ty_ymax*dy_ymax)/(b_ymax[0]*dy_ymax+2*b_ymax[1])
            
            self.Amatrix[id_ymin,id_ymin] -= bc_ymin*b_ymin[0]
            self.Amatrix[id_ymax,id_ymax] -= bc_ymax*b_ymax[0]
            
            self.b_correction[id_ymin] -= bc_ymin*b_ymin[2]
            self.b_correction[id_ymax] -= bc_ymax*b_ymax[2]

        try:
            grid_num_x = self.PorRock.grid_num[2]
        except IndexError:
            grid_num_x = 1
            
        if questbound(grid_num_x):

            zmin = indices[:,0]==indices[:,5]
            zmax = indices[:,0]==indices[:,6]

            id_zmin = indices[zmin,0]
            id_zmax = indices[zmax,0]

            dz_zmin = sizes[id_zmin,2]
            dz_zmax = sizes[id_zmax,2]

            tz_zmin = self.transmissibility[id_zmin,4]
            tz_zmax = self.transmissibility[id_zmax,5]

            bc_zmin = (2*tz_zmin*dz_zmin)/(b_zmin[0]*dz_zmin-2*b_zmin[1])
            bc_zmax = (2*tz_zmax*dz_zmax)/(b_zmax[0]*dz_zmax+2*b_zmax[1])

            self.Amatrix[id_zmin,id_zmin] -= bc_zmin*b_zmin[0]
            self.Amatrix[id_zmax,id_zmax] -= bc_zmax*b_zmax[0]
            
            self.b_correction[id_zmin] -= bc_zmin*b_zmin[2]
            self.b_correction[id_zmax] -= bc_zmax*b_zmax[2]
            
    def solve(self):

        indices = self.PorRock.grid_indices

        shape = (self.PorRock.grid_numtot,self.PorRock.grid_numtot)

        oneperdtime = np.ones(self.PorRock.grid_numtot)/self.time_step
        
        t_correction = csr((oneperdtime,(indices[:,0],indices[:,0])),shape=shape)
        
        A = self.Amatrix-t_correction
        
        for i in range(1,self.times.size):

            if i == 0:
                pressurePrev = self.pressure0
            else:
                pressurePrev = self.pressure[:,i-1]
            
            b = -pressurePrev/self.time_step+self.b_correction
            
            self.pressure[:,i] = sps(A,b)

class IMPES():
    
    def __init__(self,res,fluids,wells,relperm):

        self.res    = res 
        self.fluids = fluids
        self.wells  = wells
        self.rp     = relperm

    def set_transmissibility(self):

        dx0 = self.res.grid_sizes[:,0]
        dy0 = self.res.grid_sizes[:,1]
        dz0 = self.res.grid_sizes[:,2]

        dxm = self.res.grid_sizes[self.res.grid_indices[:,1],0]
        dxp = self.res.grid_sizes[self.res.grid_indices[:,2],0]
        dym = self.res.grid_sizes[self.res.grid_indices[:,3],1]
        dyp = self.res.grid_sizes[self.res.grid_indices[:,4],1]
        dzm = self.res.grid_sizes[self.res.grid_indices[:,5],2]
        dzp = self.res.grid_sizes[self.res.grid_indices[:,6],2]

        Ax = self.res.grid_areas[:,0]
        Ay = self.res.grid_areas[:,1]
        Az = self.res.grid_areas[:,2]

        kx0 = self.res.permeability[:,0]
        ky0 = self.res.permeability[:,1]
        kz0 = self.res.permeability[:,2]

        kxm = self.res.permeability[self.res.grid_indices[:,1],0]
        kxp = self.res.permeability[self.res.grid_indices[:,2],0]
        kym = self.res.permeability[self.res.grid_indices[:,3],1]
        kyp = self.res.permeability[self.res.grid_indices[:,4],1]
        kzm = self.res.permeability[self.res.grid_indices[:,5],2]
        kzp = self.res.permeability[self.res.grid_indices[:,6],2]

        dxm_mean = (dx0+dxm)/2
        dxp_mean = (dx0+dxp)/2
        dym_mean = (dy0+dym)/2
        dyp_mean = (dy0+dyp)/2
        dzm_mean = (dz0+dzm)/2
        dzp_mean = (dz0+dzp)/2

        kxm_mean = (2*dxm_mean)/(dx0/kx0+dxm/kxm)
        kxp_mean = (2*dxp_mean)/(dx0/kx0+dxp/kxp)
        kym_mean = (2*dym_mean)/(dy0/ky0+dym/kym)
        kyp_mean = (2*dyp_mean)/(dy0/ky0+dyp/kyp)
        kzm_mean = (2*dzm_mean)/(dz0/kz0+dzm/kzm)
        kzp_mean = (2*dzp_mean)/(dz0/kz0+dzp/kzp)

        self.TXM = (Ax*kxm_mean)/(dxm_mean)
        self.TYM = (Ay*kym_mean)/(dym_mean)
        self.TZM = (Az*kzm_mean)/(dzm_mean)

        self.TXP = (Ax*kxp_mean)/(dxp_mean)
        self.TYP = (Ay*kyp_mean)/(dyp_mean)
        self.TZP = (Az*kzp_mean)/(dzp_mean)

    def set_wells(self):

        self.well_grids      = np.array([],dtype=int)
        self.well_indices    = np.array([],dtype=int)
        self.well_bhpflags   = np.array([],dtype=bool)
        self.well_limits     = np.array([],dtype=float)
        self.well_waterflags = np.array([],dtype=bool)
        self.well_oilflags   = np.array([],dtype=bool)

        wells = zip(self.wells.tracks,self.wells.consbhp,self.wells.limits,self.wells.water,self.wells.oil)

        for index,(track,flag,limit,water,oil) in enumerate(wells):

            ttrack = np.transpose(track[:,:,np.newaxis],(2,1,0))

            vector = self.res.grid_centers[:,:,np.newaxis]-ttrack

            distance = np.sqrt(np.sum(vector**2,axis=1))

            well_grid = np.unique(np.argmin(distance,axis=0))

            well_index = np.full(well_grid.size,index,dtype=int)
    
            well_bhpflag = np.full(well_grid.size,flag,dtype=bool)

            well_limit = np.full(well_grid.size,limit,dtype=float)

            well_waterflag = np.full(well_grid.size,water,dtype=bool)
            
            well_oilflag = np.full(well_grid.size,oil,dtype=bool)

            self.well_grids = np.append(self.well_grids,well_grid)

            self.well_indices = np.append(self.well_indices,well_index)

            self.well_bhpflags = np.append(self.well_bhpflags,well_bhpflag)

            self.well_limits = np.append(self.well_limits,well_limit)

            self.well_waterflags = np.append(self.well_waterflags,well_waterflag)

            self.well_oilflags = np.append(self.well_oilflags,well_oilflag)

        dx = self.res.grid_sizes[self.well_grids,0]
        dy = self.res.grid_sizes[self.well_grids,1]
        dz = self.res.grid_sizes[self.well_grids,2]

        req = 0.14*np.sqrt(dx**2+dy**2)

        rw = self.wells.radii[self.well_indices]

        skin = self.wells.skinfactors[self.well_indices]

        kx = self.res.permeability[:,0][self.well_grids]
        ky = self.res.permeability[:,1][self.well_grids]

        dz = self.res.grid_sizes[:,2][self.well_grids]

        self.JR = (2*np.pi*dz*np.sqrt(kx*ky))/(np.log(req/rw)+skin)

    def set_time(self,step,total):

        self.time_step = step
        self.time_total = total

        self.time_array = np.arange(self.time_step,self.time_total+self.time_step,self.time_step)

    def initialize(self,pressure,saturation):

        N = self.time_array.size

        self.pressure = np.empty((self.res.grid_numtot,N+1))
        self.pressure[:,0] = pressure

        self.Sw = np.empty((self.res.grid_numtot,N+1))
        self.Sw[:,0] = saturation

        # for index,(p,sw) in enumerate(zip(self.pressure[:,-1],self.Sw[:,-1])):
        #   print("{:d}\tP\t{:3.0f}\tSw\t{:.4f}".format(index,p,sw))

    def solve(self):

        Vp = self.res.grid_volumes*self.res.porosity

        muw = self.fluids.viscosity[0]
        muo = self.fluids.viscosity[1]
        
        fvfw = self.fluids.fvf[0]
        fvfo = self.fluids.fvf[1]
        
        cr = self.res.compressibility

        cw = self.fluids.compressibility[0]
        co = self.fluids.compressibility[1]

        wflow_1phase = ~np.logical_and(self.well_waterflags,self.well_oilflags)

        sp_wf = np.logical_and(self.well_waterflags,wflow_1phase)
        sp_of = np.logical_and(self.well_oilflags,wflow_1phase)

        cpw = np.logical_and(self.well_bhpflags,self.well_waterflags)
        cpo = np.logical_and(self.well_bhpflags,self.well_oilflags)

        cqw = np.logical_and(~self.well_bhpflags,self.well_waterflags)
        cqo = np.logical_and(~self.well_bhpflags,self.well_oilflags)

        cxm_0 = self.res.grid_indices[self.res.grid_hasxmin,0]
        cxm_m = self.res.grid_indices[self.res.grid_hasxmin,1]

        cxp_0 = self.res.grid_indices[self.res.grid_hasxmax,0]
        cxp_p = self.res.grid_indices[self.res.grid_hasxmax,2]

        cym_0 = self.res.grid_indices[self.res.grid_hasymin,0]
        cym_m = self.res.grid_indices[self.res.grid_hasymin,3]

        cyp_0 = self.res.grid_indices[self.res.grid_hasymax,0]
        cyp_p = self.res.grid_indices[self.res.grid_hasymax,4]

        czm_0 = self.res.grid_indices[self.res.grid_haszmin,0]
        czm_m = self.res.grid_indices[self.res.grid_haszmin,5]

        czp_0 = self.res.grid_indices[self.res.grid_haszmax,0]
        czp_p = self.res.grid_indices[self.res.grid_haszmax,6]

        mshape = (self.res.grid_numtot,self.res.grid_numtot)
        vshape = (self.res.grid_numtot,1)

        vzeros = np.zeros(len(self.wells.itemnames),dtype=int)

        krw,kro = self.rp.water_oil(Sw=self.Sw[:,0])

        krw_xm = krw[cxm_0]
        krw_xp = krw[cxp_0]
        krw_ym = krw[cym_0]
        krw_yp = krw[cyp_0]
        krw_zm = krw[czm_0]
        krw_zp = krw[czp_0]

        kro_xm = kro[cxm_0]
        kro_xp = kro[cxp_0]
        kro_ym = kro[cym_0]
        kro_yp = kro[cyp_0]
        kro_zm = kro[czm_0]
        kro_zp = kro[czp_0]

        for index,time in enumerate(self.time_array):

            print("@{:5.1f}th time step".format(time))

            d11 = (Vp*self.Sw[:,index])/(self.time_step*fvfw)*(cr+cw)
            d12 = (Vp)/(self.time_step*fvfw)
            d21 = (Vp*(1-self.Sw[:,index]))/(self.time_step*fvfo)*(cr+co)
            d22 = (Vp)/(self.time_step*fvfo)*-1

            self.D = diags(-d22/d12*d11+d21)

            Uxm = self.pressure[:,index][cxm_0]>self.pressure[:,index][cxm_m]
            Uxp = self.pressure[:,index][cxp_0]>self.pressure[:,index][cxp_p]
            Uym = self.pressure[:,index][cym_0]>self.pressure[:,index][cym_m]
            Uyp = self.pressure[:,index][cyp_0]>self.pressure[:,index][cyp_p]
            Uzm = self.pressure[:,index][czm_0]>self.pressure[:,index][czm_m]
            Uzp = self.pressure[:,index][czp_0]>self.pressure[:,index][czp_p]

            krw,kro = self.rp.water_oil(Sw=self.Sw[:,index])

            krw_xm[Uxm] = krw[cxm_0][Uxm]
            krw_xp[Uxp] = krw[cxp_0][Uxp]
            krw_ym[Uym] = krw[cym_0][Uym]
            krw_yp[Uyp] = krw[cyp_0][Uyp]
            krw_zm[Uzm] = krw[czm_0][Uzm]
            krw_zp[Uzp] = krw[czp_0][Uzp]

            krw_xm[~Uxm] = krw[cxm_m][~Uxm]
            krw_xp[~Uxp] = krw[cxp_p][~Uxp]
            krw_ym[~Uym] = krw[cym_m][~Uym]
            krw_yp[~Uyp] = krw[cyp_p][~Uyp]
            krw_zm[~Uzm] = krw[czm_m][~Uzm]
            krw_zp[~Uzp] = krw[czp_p][~Uzp]

            kro_xm[Uxm] = kro[cxm_0][Uxm]
            kro_xp[Uxp] = kro[cxp_0][Uxp]
            kro_ym[Uym] = kro[cym_0][Uym]
            kro_yp[Uyp] = kro[cyp_0][Uyp]
            kro_zm[Uzm] = kro[czm_0][Uzm]
            kro_zp[Uzp] = kro[czp_0][Uzp]

            kro_xm[~Uxm] = kro[cxm_m][~Uxm]
            kro_xp[~Uxp] = kro[cxp_p][~Uxp]
            kro_ym[~Uym] = kro[cym_m][~Uym]
            kro_yp[~Uyp] = kro[cyp_p][~Uyp]
            kro_zm[~Uzm] = kro[czm_m][~Uzm]
            kro_zp[~Uzp] = kro[czp_p][~Uzp]

            TXMw = (self.TXM[self.res.grid_hasxmin]*krw_xm)/(muw*fvfw)*6.33e-3 # unit conversion
            TYMw = (self.TYM[self.res.grid_hasymin]*krw_ym)/(muw*fvfw)*6.33e-3 # unit conversion
            TZMw = (self.TZM[self.res.grid_haszmin]*krw_zm)/(muw*fvfw)*6.33e-3 # unit conversion

            TXPw = (self.TXP[self.res.grid_hasxmax]*krw_xp)/(muw*fvfw)*6.33e-3 # unit conversion
            TYPw = (self.TYP[self.res.grid_hasymax]*krw_yp)/(muw*fvfw)*6.33e-3 # unit conversion
            TZPw = (self.TZP[self.res.grid_haszmax]*krw_zp)/(muw*fvfw)*6.33e-3 # unit conversion

            TXMn = (self.TXM[self.res.grid_hasxmin]*kro_xm)/(muo*fvfo)*6.33e-3 # unit conversion
            TYMn = (self.TYM[self.res.grid_hasymin]*kro_ym)/(muo*fvfo)*6.33e-3 # unit conversion
            TZMn = (self.TZM[self.res.grid_haszmin]*kro_zm)/(muo*fvfo)*6.33e-3 # unit conversion

            TXPn = (self.TXP[self.res.grid_hasxmax]*kro_xp)/(muo*fvfo)*6.33e-3 # unit conversion
            TYPn = (self.TYP[self.res.grid_hasymax]*kro_yp)/(muo*fvfo)*6.33e-3 # unit conversion
            TZPn = (self.TZP[self.res.grid_haszmax]*kro_zp)/(muo*fvfo)*6.33e-3 # unit conversion

            self.Tw = csr(mshape)

            self.Tw -= csr((TXMw,(cxm_0,cxm_m)),shape=mshape)
            self.Tw += csr((TXMw,(cxm_0,cxm_0)),shape=mshape)

            self.Tw -= csr((TXPw,(cxp_0,cxp_p)),shape=mshape)
            self.Tw += csr((TXPw,(cxp_0,cxp_0)),shape=mshape)

            self.Tw -= csr((TYMw,(cym_0,cym_m)),shape=mshape)
            self.Tw += csr((TYMw,(cym_0,cym_0)),shape=mshape)

            self.Tw -= csr((TYPw,(cyp_0,cyp_p)),shape=mshape)
            self.Tw += csr((TYPw,(cyp_0,cyp_0)),shape=mshape)

            self.Tw -= csr((TZMw,(czm_0,czm_m)),shape=mshape)
            self.Tw += csr((TZMw,(czm_0,czm_0)),shape=mshape)

            self.Tw -= csr((TZPw,(czp_0,czp_p)),shape=mshape)
            self.Tw += csr((TZPw,(czp_0,czp_0)),shape=mshape)

            self.Tn = csr(mshape)

            self.Tn -= csr((TXMn,(cxm_0,cxm_m)),shape=mshape)
            self.Tn += csr((TXMn,(cxm_0,cxm_0)),shape=mshape)

            self.Tn -= csr((TXPn,(cxp_0,cxp_p)),shape=mshape)
            self.Tn += csr((TXPn,(cxp_0,cxp_0)),shape=mshape)

            self.Tn -= csr((TYMn,(cym_0,cym_m)),shape=mshape)
            self.Tn += csr((TYMn,(cym_0,cym_0)),shape=mshape)

            self.Tn -= csr((TYPn,(cyp_0,cyp_p)),shape=mshape)
            self.Tn += csr((TYPn,(cyp_0,cyp_0)),shape=mshape)

            self.Tn -= csr((TZMn,(czm_0,czm_m)),shape=mshape)
            self.Tn += csr((TZMn,(czm_0,czm_0)),shape=mshape)

            self.Tn -= csr((TZPn,(czp_0,czp_p)),shape=mshape)
            self.Tn += csr((TZPn,(czp_0,czp_0)),shape=mshape)

            self.T = diags(-d22/d12)*self.Tw+self.Tn

            krw,kro = self.rp.water_oil(Sw=self.Sw[:,index][self.well_grids])

            Jw_v = (self.JR*krw)/(muw*fvfw)*6.33e-3 # unit conversion
            Jn_v = (self.JR*kro)/(muo*fvfo)*6.33e-3 # unit conversion

            self.Jw = csr((Jw_v[cpw],(self.well_grids[cpw],self.well_grids[cpw])),shape=mshape)
            self.Jn = csr((Jn_v[cpo],(self.well_grids[cpo],self.well_grids[cpo])),shape=mshape)

            self.J = diags(-d22/d12)*self.Jw+self.Jn
            
            self.Qw = csr(vshape)
            self.Qn = csr(vshape)

            qw_cp = self.well_limits[cpw]*Jw_v[cpw]
            qo_cp = self.well_limits[cpo]*Jn_v[cpo]

            qw_cr = self.well_limits[cqw]*(krw[cqw]*muo)/(krw[cqw]*muo+kro[cqw]*muw)
            qo_cr = self.well_limits[cqo]*(kro[cqo]*muw)/(krw[cqo]*muo+kro[cqo]*muw)

            qw_cr[sp_wf[cqw]] = self.well_limits[sp_wf]
            qo_cr[sp_of[cqo]] = self.well_limits[sp_of]

            self.Qw += csr((qw_cp,(self.well_grids[cpw],vzeros[cpw])),shape=vshape)
            self.Qn += csr((qo_cp,(self.well_grids[cpo],vzeros[cpo])),shape=vshape)

            self.Qw += csr((qw_cr*5.61,(self.well_grids[cqw],vzeros[cqw])),shape=vshape) # unit conversion
            self.Qn += csr((qo_cr*5.61,(self.well_grids[cqo],vzeros[cqo])),shape=vshape) # unit conversion

            self.Qw = self.Qw.toarray().flatten()
            self.Qn = self.Qn.toarray().flatten()

            self.Q = -d22/d12*self.Qw+self.Qn

            self.pressure[:,index+1] = sps(self.T+self.J+self.D,self.D.dot(self.pressure[:,index])+self.Q)

            delta_p = (self.pressure[:,index+1]-self.pressure[:,index])
            
            tjp = csr.dot(self.Tw+self.Jw,self.pressure[:,index+1])

            self.Sw[:,index+1] = self.Sw[:,index]-(d11*delta_p-self.Qw+tjp)/d12

        for index,(p,sw) in enumerate(zip(self.pressure[:,-1],self.Sw[:,-1])):
            print("{:d}\tP\t{:4.1f}\tSw\t{:.5f}".format(index,p,sw))

class SS():
    
    def __init__(self):

        pass

    def set_time(self,step,total):

        self.timesteps = np.arange(0,total+step,step)

    def solve():

        pass

class Green():

    pass
    
    # properties
    #     currentTimeStep
    #     wellpressure
    #     wellflowrate
    #     pressure
    #     fracflux
    # end
    
    # methods (Static)
        
    #     function obj = solver(res,frac,well,time,Gplane)
            
    #         % for now only constant fracture properties is modeled
            
    #         # T = frac.permeability.*frac.areatofracture/...
    #         #      res.oilViscosity/res.oilFVF./frac.Length;

    #         req = 0.14*sqrt(frac.Length(well.wellID).^2+res.zLength^2);
            
    #         J = 2*pi*frac.width(well.wellID).*frac.permeability(well.wellID)./...
    #                 (res.oilViscosity*res.oilFVF.*log(req/well.radius));
                
    #         if ~isempty(well.consPressure)
    #             Q = J.*well.consPressure;
    #             Jmat = sparse(well.wellID,well.wellID,J,frac.numAfrac,frac.numAfrac);
    #         elseif ~isempty(well.consFlowrate)
    #             Q = -well.consFlowrate;
    #             Jmat = sparse(frac.numAfrac,frac.numAfrac);
    #         end
            
    #         Qvec = sparse(frac.numAfrac,1);
    #         Qvec(well.wellID) = Qvec(well.wellID)+Q;
            
    #         Tmat = solver.transmissibility(res,frac);

    #         obj.pressure = zeros(frac.numAfrac,time.numTimeStep);
    #         obj.fracflux = zeros(frac.numAfrac,time.numTimeStep);
            
    #         Af = diag(frac.areatoreservoir);
            
    #         for N = 1:time.numTimeStep
                
    #             Amat = (Tmat+Jmat)*Gplane(:,:,1)*time.deltaTime+Af/N;
                
    #             green = solver.convolution(Gplane,obj.fracflux,2,N)*time.deltaTime;
    #             fluxx = sum(obj.fracflux(:,1:N-1),2);
            
    #             bvec = (Tmat+Jmat)*(res.initPressure-green)-Af/N*fluxx-Qvec;

    #             obj.fracflux(:,N) = Amat\bvec;
    #            #obj.pressure(:,N+1) = (Tmat+Jmat)\(Af*obj.fracflux(:,N+1)+Qvec);
    #             obj.pressure(:,N) = res.initPressure-Gplane(:,:,1)*obj.fracflux(:,N)*time.deltaTime-green;
                
    #             disp(['Time iteration... ',num2str(N/time.numTimeStep*100),'% is complete'])

    #         end
            
    #         if ~isempty(well.consPressure)
    #             obj.wellpressure = zeros(1,time.numTimeStep)+well.consPressure;
    #             obj.wellflowrate = -Q+J.*obj.pressure(well.wellID,:);
    #         elseif ~isempty(well.consFlowrate)
    #             obj.wellpressure = obj.pressure(well.wellID,:)+Q./J;
    #             obj.wellflowrate = zeros(1,time.numTimeStep)-Q;
    #         end
            
    #     end
        
    #     function Tmat = transmissibility(res,frac)
            
    #         % construction of transmissibility matrix
            
    #         Tmat = zeros(frac.numAfrac,frac.numAfrac);
            
    #         for host = 1:frac.numAfrac
                
    #             [neighbor,~] = find(sum(permute(frac.map,[1,3,2])==frac.map(host,:),3));
    
    #             neighbor(neighbor==host)=[];

    #             if length(neighbor)~=length(unique(neighbor))
    #                 error('there is an error in the fracture map')
    #             end
                
    #             deltax = (frac.Length(host)+frac.Length(neighbor))/2;
                
    #             perm = deltax./(frac.Length(host)/frac.permeability(host)/2+...
    #                 frac.Length(neighbor)./frac.permeability(neighbor)/2);
                
    #             trans = perm.*frac.areatofracture(host)/...
    #                     res.oilViscosity/res.oilFVF./deltax;

    #             Tmat(host,host) = sum(trans);
    #             Tmat(host,neighbor) = -trans;

    #         end
            
    #     end
        
    #     function array = convolution(matrix,vector,timeidx1,timeidx2)

    #         switch nargin
    #             case 2
    #                 timeidx1 = 1;
    #                 timeidx2 = size(vector,2);
    #         end
            
    #         n1 = timeidx1:timeidx2;
    #         n2 = timeidx2-n1+1;
            
    #         G = matrix(:,:,n1);
    #         Q = permute(vector(:,n2),[3,1,2]);
            
    #         array = sum(sum(G.*Q,2),3);

    #     end
        
    # end

if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)