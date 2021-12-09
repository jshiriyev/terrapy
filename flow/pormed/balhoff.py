import matplotlib.pyplot as plt
import numpy as np 

from scipy.sparse import csr_matrix as csr
from scipy.sparse import diags

from scipy.sparse.linalg import spsolve as sps

if __name__ == "__main__":
    import setup

from flow.pormed.conrelation import relative_permeability
from flow.pormed.conrelation import relative_permeability_balhoff

from interfaces.items import Formation
from interfaces.items import Wells
from interfaces.items import Fluids

class IMPES():
	
	def __init__(self,res,fluids,wells,relperm):

		self.res 	= res 
		self.fluids = fluids
		self.wells 	= wells
		self.rp 	= relperm

	def merge(self):

		wdtype = [('index',int),('grid',int),('bhp_flag',bool),('limit',float),('water_flag',bool),('oil_flag',bool)]

		self.wellsmerged = np.array([],dtype=wdtype)

		for index,(track,flag,limit,water,oil) in enumerate(zip(self.wells.tracks,self.wells.consbhp,self.wells.limits,self.wells.water,self.wells.oil)):

			ttrack = np.transpose(track[:,:,np.newaxis],(2,1,0))

			vector = self.res.grid_centers[:,:,np.newaxis]-ttrack

			distance = np.sqrt(np.sum(vector**2,axis=1))

			grid_indices = np.unique(np.argmin(distance,axis=0))

			well_indices = np.full(grid_indices.size,index,dtype=int)

			bhpflags = np.full(grid_indices.size,flag,dtype=bool)

			limits = np.full(grid_indices.size,limit,dtype=float)

			wflags = np.full(grid_indices.size,water,dtype=bool)
			oflags = np.full(grid_indices.size,oil,dtype=bool)

			wellmerged = np.rec.fromarrays((well_indices,grid_indices,bhpflags,limits,wflags,oflags),dtype=wdtype)

			self.wellsmerged = np.append(self.wellsmerged,wellmerged)

		self.req = np.empty(self.wellsmerged["index"].shape)

		for index,grid_index in enumerate(self.wellsmerged["grid"]):

			dx = self.res.grid_sizes[grid_index,0]
			dy = self.res.grid_sizes[grid_index,1]
			dz = self.res.grid_sizes[grid_index,2]

			self.req[index] = 0.14*np.sqrt(dx**2+dy**2)

	def initialize(self,pressure,saturation):

		self.pressure = np.empty(self.res.grid_numtot)
		self.pressure[:] = pressure

		self.Sw = np.empty(self.res.grid_numtot)
		self.Sw[:] = saturation

		# for index,(p,sw) in enumerate(zip(self.pressure,self.Sw)):
		# 	print("{:d}\tP\t{:3.0f}\tSw\t{:.4f}".format(index,p,sw))

	def set_time(self,step,total):

		self.time_step = step
		self.time_total = total

		self.time_array = np.arange(self.time_step,self.time_total+self.time_step,self.time_step)

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

	def solve(self):

		# print(self.res.grid_indices)

		muw = self.fluids.viscosity[0]
		muo = self.fluids.viscosity[1]
		
		fvfw = self.fluids.fvf[0]
		fvfo = self.fluids.fvf[1]
		
		cr = self.res.compressibility

		cw = self.fluids.compressibility[0]
		co = self.fluids.compressibility[1]

		windex = self.wellsmerged["index"]
		gindex = self.wellsmerged["grid"]

		bhp = self.wellsmerged["bhp_flag"]

		QB = self.wellsmerged["limit"]

		water = self.wellsmerged["water_flag"]
		oil = self.wellsmerged["oil_flag"]

		rw = self.wells.radii[windex]

		skin = self.wells.skinfactors[windex]

		Vp = self.res.grid_volumes*self.res.porosity

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

		cqw = np.logical_and(~bhp,water)
		cqo = np.logical_and(~bhp,oil)

		for time in self.time_array:

			print("@{:5.1f}th time step".format(time))

			d11 = (Vp*self.Sw)/(self.time_step*fvfw)*(cr+cw)
			d12 = (Vp)/(self.time_step*fvfw)
			d21 = (Vp*(1-self.Sw))/(self.time_step*fvfo)*(cr+co)
			d22 = (Vp)/(self.time_step*fvfo)*-1

			self.D = diags(-d22/d12*d11+d21)

			self.rp.system2phase(Sw=self.Sw,model="oil-water")

			TXMw = (self.TXM*self.rp.krw)/(muw*fvfw)*6.33e-3 # unit conversion
			TYMw = (self.TYM*self.rp.krw)/(muw*fvfw)*6.33e-3 # unit conversion
			TZMw = (self.TZM*self.rp.krw)/(muw*fvfw)*6.33e-3 # unit conversion

			TXPw = (self.TXP*self.rp.krw)/(muw*fvfw)*6.33e-3 # unit conversion
			TYPw = (self.TYP*self.rp.krw)/(muw*fvfw)*6.33e-3 # unit conversion
			TZPw = (self.TZP*self.rp.krw)/(muw*fvfw)*6.33e-3 # unit conversion

			TXMn = (self.TXM*self.rp.kro)/(muo*fvfo)*6.33e-3 # unit conversion
			TYMn = (self.TYM*self.rp.kro)/(muo*fvfo)*6.33e-3 # unit conversion
			TZMn = (self.TZM*self.rp.kro)/(muo*fvfo)*6.33e-3 # unit conversion

			TXPn = (self.TXP*self.rp.kro)/(muo*fvfo)*6.33e-3 # unit conversion
			TYPn = (self.TYP*self.rp.kro)/(muo*fvfo)*6.33e-3 # unit conversion
			TZPn = (self.TZP*self.rp.kro)/(muo*fvfo)*6.33e-3 # unit conversion

			self.Tw = csr(mshape)

			self.Tw -= csr((TXMw[self.res.grid_hasxmin],(cxm_0,cxm_m)),shape=mshape)
			self.Tw += csr((TXMw[self.res.grid_hasxmin],(cxm_0,cxm_0)),shape=mshape)

			self.Tw -= csr((TXPw[self.res.grid_hasxmax],(cxp_0,cxp_p)),shape=mshape)
			self.Tw += csr((TXPw[self.res.grid_hasxmax],(cxp_0,cxp_0)),shape=mshape)

			self.Tw -= csr((TYMw[self.res.grid_hasymin],(cym_0,cym_m)),shape=mshape)
			self.Tw += csr((TYMw[self.res.grid_hasymin],(cym_0,cym_0)),shape=mshape)

			self.Tw -= csr((TYPw[self.res.grid_hasymax],(cyp_0,cyp_p)),shape=mshape)
			self.Tw += csr((TYPw[self.res.grid_hasymax],(cyp_0,cyp_0)),shape=mshape)

			self.Tw -= csr((TZMw[self.res.grid_haszmin],(czm_0,czm_m)),shape=mshape)
			self.Tw += csr((TZMw[self.res.grid_haszmin],(czm_0,czm_0)),shape=mshape)

			self.Tw -= csr((TZPw[self.res.grid_haszmax],(czp_0,czp_p)),shape=mshape)
			self.Tw += csr((TZPw[self.res.grid_haszmax],(czp_0,czp_0)),shape=mshape)

			self.Tn = csr(mshape)

			self.Tn -= csr((TXMn[self.res.grid_hasxmin],(cxm_0,cxm_m)),shape=mshape)
			self.Tn += csr((TXMn[self.res.grid_hasxmin],(cxm_0,cxm_0)),shape=mshape)

			self.Tn -= csr((TXPn[self.res.grid_hasxmax],(cxp_0,cxp_p)),shape=mshape)
			self.Tn += csr((TXPn[self.res.grid_hasxmax],(cxp_0,cxp_0)),shape=mshape)

			self.Tn -= csr((TYMn[self.res.grid_hasymin],(cym_0,cym_m)),shape=mshape)
			self.Tn += csr((TYMn[self.res.grid_hasymin],(cym_0,cym_0)),shape=mshape)

			self.Tn -= csr((TYPn[self.res.grid_hasymax],(cyp_0,cyp_p)),shape=mshape)
			self.Tn += csr((TYPn[self.res.grid_hasymax],(cyp_0,cyp_0)),shape=mshape)

			self.Tn -= csr((TZMn[self.res.grid_haszmin],(czm_0,czm_m)),shape=mshape)
			self.Tn += csr((TZMn[self.res.grid_haszmin],(czm_0,czm_0)),shape=mshape)

			self.Tn -= csr((TZPn[self.res.grid_haszmax],(czp_0,czp_p)),shape=mshape)
			self.Tn += csr((TZPn[self.res.grid_haszmax],(czp_0,czp_0)),shape=mshape)

			self.T = diags(-d22/d12)*self.Tw+self.Tn

			Jw_v = (2*np.pi*self.res.grid_sizes[:,2][gindex]*self.res.permeability[:,0][gindex]*self.rp.krw[gindex])/(muw*fvfw*(np.log(self.req/rw)+skin))*6.33e-3 # unit conversion
			Jn_v = (2*np.pi*self.res.grid_sizes[:,2][gindex]*self.res.permeability[:,0][gindex]*self.rp.kro[gindex])/(muo*fvfo*(np.log(self.req/rw)+skin))*6.33e-3 # unit conversion

			self.Jw = csr((Jw_v[bhp],(gindex[bhp],gindex[bhp])),shape=mshape)
			self.Jn = csr((Jn_v[bhp],(gindex[bhp],gindex[bhp])),shape=mshape)

			self.J = diags(-d22/d12)*self.Jw+self.Jn
			
			self.Qw = csr(vshape)
			self.Qn = csr(vshape)

			self.Qw += csr((QB[bhp]*Jw_v[bhp],(gindex[bhp],vzeros[bhp])),shape=vshape)
			self.Qn += csr((QB[bhp]*Jn_v[bhp],(gindex[bhp],vzeros[bhp])),shape=vshape)

			self.Qw += csr((QB[cqw]*5.61,(gindex[cqw],vzeros[cqw])),shape=vshape) # unit conversion
			self.Qn += csr((QB[cqo]*5.61,(gindex[cqo],vzeros[cqo])),shape=vshape) # unit conversion

			self.Qw = self.Qw.toarray().flatten()
			self.Qn = self.Qn.toarray().flatten()

			self.Q = -d22/d12*self.Qw+self.Qn

			pressure_old = self.pressure

			self.pressure = sps(self.T+self.J+self.D,self.D.dot(self.pressure)+self.Q)

			self.Sw += (-d11*(self.pressure-pressure_old)+self.Qw+self.Tw.dot(self.pressure))/d12

		for index,(p,sw) in enumerate(zip(self.pressure,self.Sw)):
			print("{:d}\tP\t{:4.1f}\tSw\t{:.5f}".format(index,p,sw))

class SS():
	
	def __init__(self):

		pass

	def set_time(self,step,total):

		self.timesteps = np.arange(0,total+step,step)

	def solve():

		pass

if __name__ == "__main__":

	# FORMATION

	res = Formation("FU",None)

	res.set_dimensions((1200,600,200))

	res.grid((3,3,1))

	res.set_porosity(0.26)

	res.set_permeability(1800)

	res.set_compressibility(3e-6)

	# FLUID

	fluids = Fluids(2)

	fluids.set_names(("water","oil"))
	fluids.set_compressibility((2e-6,5e-6))
	fluids.set_viscosity((1.,5.))
	fluids.set_fvf((1.,1.))

	# WELLS

	wells = Wells(None)

	wells.set_names(["inj","prod1","prod2"])

	wells.set_tracks((
		(( 600,300,400),( 600,300,0)),
		(( 200,100,400),( 200,100,0)),
		((1000,500,400),(1000,500,0)),
		))

	wells.set_flowconds(
		conditions=("rate","rate","bhp"),
		limits=(3000,-2000,800),
		fluids=("water","oil","both"))

	wells.set_skinfactors((0,0,0))

	wells.set_radii((0.5,0.5,0.5))

	# RELATIVE PERMEABILITY

	# relperm = relative_permeability(
	# 	Sorow=0.2,
	# 	Swc=0.2,
	# 	krowc=1,
	# 	krwor=0.2,
	# 	no=3,
	# 	nw=3)

	relperm = relative_permeability_balhoff(
		Swi=0.2,
		Swr=0.2,
		krwo=0.2,
		kroo=1.0,
		nw=3,
		no=3,
		)

	# SOLVING

	solver = IMPES(res,fluids,wells,relperm)

	solver.merge()
	solver.initialize(pressure=1000,saturation=0.2)
	solver.set_time(0.1,500)
	solver.set_transmissibility()
	solver.solve()

	# PLOTTING

	# fig = plt.figure()

	# ax = plt.axes(projection='3d')

	# # ax.scatter3D(*res.edge_vertices.T)

	# for line in solver.res.edge_lines:
	#     ax.plot3D(*line,color='grey')

	# for track,name in zip(solver.wells.tracks,solver.wells.itemnames):

	# 	ax.plot3D(*track.T,color="black",linewidth=1)
	# 	ax.text(*track[0,:],name)

	# ax.scatter3D(*solver.res.grid_centers.T)

	# ax.set_xlabel("x-axis")
	# ax.set_ylabel("y-axis")

	# ax.set_box_aspect(solver.res.lengths)

	# ax.set_axis_off()

	# ax.margins(x=0,y=0)

	# plt.tight_layout()

	# plt.figure()

	# plt.plot(Sw,solver.rp.kro,"k--",label="oil")
	# plt.plot(Sw,solver.rp.krw,"k-",label="water")

	# plt.xlabel("Water Saturation")
	# plt.ylabel("Relative Permeability")

	# plt.legend()

	# plt.xlim((0,1))

	# plt.show()


