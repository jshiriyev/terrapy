import matplotlib.pyplot as plt
import numpy as np 

from scipy.sparse import csr_matrix as csr
from scipy.sparse import diags

from scipy.sparse.linalg import spsolve as sps

if __name__ == "__main__":
    import setup

from flow.pormed.conrelation import relative_permeability

from interfaces.items import Formation
from interfaces.items import Wells
from interfaces.items import Fluids

class IMPES():
	
	def __init__(self):

		pass

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

		self.Sw = np.empty(self.res.grid_numtot)

		self.pressure[:] = pressure

		self.pressure = np.array([self.pressure]).T

		self.Sw[:] = saturation

	def set_time(self,step,total):

		self.time_step = step
		self.time_total = total

		self.time_array = np.arange(0,self.time_total+self.time_step,self.time_step)

	def solve(self):

		kx = self.res.permeability[:,0]
		ky = self.res.permeability[:,1]
		kz = self.res.permeability[:,2]

		dx = self.res.grid_sizes[:,0]
		dy = self.res.grid_sizes[:,1]
		dz = self.res.grid_sizes[:,2]

		Ax = self.res.grid_areas[:,0]
		Ay = self.res.grid_areas[:,1]
		Az = self.res.grid_areas[:,2]

		muo = self.fluids.viscosity[0]
		muw = self.fluids.viscosity[1]

		fvfo = self.fluids.fvf[0]
		fvfw = self.fluids.fvf[1]

		cr = self.res.compressibility

		cw = self.fluids.compressibility[1]
		co = self.fluids.compressibility[0]

		windex = self.wellsmerged["index"]
		gindex = self.wellsmerged["grid"]

		bhp = self.wellsmerged["bhp_flag"]

		QB = self.wellsmerged["limit"]

		water = self.wellsmerged["water_flag"]
		oil = self.wellsmerged["oil_flag"]

		rw = self.wells.radii[windex]

		S = self.wells.skinfactors[windex]

		d11 = (self.res.grid_volumes*self.res.porosity*self.Sw)/(fvfw*self.time_step)*(cr+cw)
		d12 = (self.res.grid_volumes*self.res.porosity)/(self.time_step*fvfw)
		d21 = (self.res.grid_volumes*self.res.porosity*(1-self.Sw))/(fvfo*self.time_step)*(cr+co)
		d22 = (self.res.grid_volumes*self.res.porosity)/(self.time_step*fvfo)*-1

		self.D = diags(-d22/d12*d11+d21)

		self.rp.system2phase(Sw=self.Sw,model="oil-water")

		TXw = (kx*self.rp.krw*Ax)/(muw*fvfw*dx)/158 # unit conversion
		TYw = (ky*self.rp.krw*Ay)/(muw*fvfw*dy)/158 # unit conversion
		TZw = (kz*self.rp.kro*Az)/(muo*fvfo*dz)/158 # unit conversion

		TXn = (kx*self.rp.kro*Ax)/(muo*fvfo*dx)/158 # unit conversion
		TYn = (ky*self.rp.kro*Ay)/(muo*fvfo*dy)/158 # unit conversion
		TZn = (kz*self.rp.kro*Az)/(muo*fvfo*dz)/158 # unit conversion

		Jw_v = (2*np.pi*kx[gindex]*self.rp.krw[gindex]*dz[gindex])/(muw*fvfw*(np.log(self.req/rw)+S))/158 # unit conversion
		Jn_v = (2*np.pi*kx[gindex]*self.rp.kro[gindex]*dz[gindex])/(muo*fvfo*(np.log(self.req/rw)+S))/158 # unit conversion

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

		self.Tw = csr(mshape)

		self.Tw -= csr((TXw[self.res.grid_hasxmin],(cxm_0,cxm_m)),shape=mshape)
		self.Tw += csr((TXw[self.res.grid_hasxmin],(cxm_0,cxm_0)),shape=mshape)

		self.Tw -= csr((TXw[self.res.grid_hasxmax],(cxp_0,cxp_p)),shape=mshape)
		self.Tw += csr((TXw[self.res.grid_hasxmax],(cxp_0,cxp_0)),shape=mshape)

		self.Tw -= csr((TYw[self.res.grid_hasymin],(cym_0,cym_m)),shape=mshape)
		self.Tw += csr((TYw[self.res.grid_hasymin],(cym_0,cym_0)),shape=mshape)

		self.Tw -= csr((TYw[self.res.grid_hasymax],(cyp_0,cyp_p)),shape=mshape)
		self.Tw += csr((TYw[self.res.grid_hasymax],(cyp_0,cyp_0)),shape=mshape)

		self.Tw -= csr((TZw[self.res.grid_haszmin],(czm_0,czm_m)),shape=mshape)
		self.Tw += csr((TZw[self.res.grid_haszmin],(czm_0,czm_0)),shape=mshape)

		self.Tw -= csr((TZw[self.res.grid_haszmax],(czp_0,czp_p)),shape=mshape)
		self.Tw += csr((TZw[self.res.grid_haszmax],(czp_0,czp_0)),shape=mshape)

		self.Tn = csr(mshape)

		self.Tn -= csr((TXn[self.res.grid_hasxmin],(cxm_0,cxm_m)),shape=mshape)
		self.Tn += csr((TXn[self.res.grid_hasxmin],(cxm_0,cxm_0)),shape=mshape)

		self.Tn -= csr((TXn[self.res.grid_hasxmax],(cxp_0,cxp_p)),shape=mshape)
		self.Tn += csr((TXn[self.res.grid_hasxmax],(cxp_0,cxp_0)),shape=mshape)

		self.Tn -= csr((TYn[self.res.grid_hasymin],(cym_0,cym_m)),shape=mshape)
		self.Tn += csr((TYn[self.res.grid_hasymin],(cym_0,cym_0)),shape=mshape)

		self.Tn -= csr((TYn[self.res.grid_hasymax],(cyp_0,cyp_p)),shape=mshape)
		self.Tn += csr((TYn[self.res.grid_hasymax],(cyp_0,cyp_0)),shape=mshape)

		self.Tn -= csr((TZn[self.res.grid_haszmin],(czm_0,czm_m)),shape=mshape)
		self.Tn += csr((TZn[self.res.grid_haszmin],(czm_0,czm_0)),shape=mshape)

		self.Tn -= csr((TZn[self.res.grid_haszmax],(czp_0,czp_p)),shape=mshape)
		self.Tn += csr((TZn[self.res.grid_haszmax],(czp_0,czp_0)),shape=mshape)

		self.T = diags(-d22/d12)*self.Tw+self.Tn
		
		self.Jw = csr((Jw_v[bhp],(gindex[bhp],gindex[bhp])),shape=mshape)
		self.Jn = csr((Jn_v[bhp],(gindex[bhp],gindex[bhp])),shape=mshape)

		self.J = diags(-d22/d12)*self.Jw+self.Jn

		vshape = (self.res.grid_numtot,1)

		vzeros = np.zeros(len(self.wells.itemnames),dtype=int)
		
		self.Qw = csr(vshape)
		self.Qn = csr(vshape)

		self.Qw += csr((QB[bhp]*Jw_v[bhp],(gindex[bhp],vzeros[bhp])),shape=vshape)
		self.Qn += csr((QB[bhp]*Jn_v[bhp],(gindex[bhp],vzeros[bhp])),shape=vshape)

		cqw = np.logical_and(~bhp,water)
		cqo = np.logical_and(~bhp,oil)

		self.Qw += csr((QB[cqw]*5.61,(gindex[cqw],vzeros[cqw])),shape=vshape) # unit conversion
		self.Qn += csr((QB[cqo]*5.61,(gindex[cqo],vzeros[cqo])),shape=vshape) # unit conversion

		self.Q = diags(-d22/d12)*self.Qw+self.Qn

		A = (self.T+self.J+self.D)

		b = self.D.dot(self.pressure)+self.Q

		self.pressure = sps(A,b)

		print(self.pressure)

class SS():
	
	def __init__(self):

		pass

	def set_time(self,step,total):

		self.timesteps = np.arange(0,total+step,step)

	def solve():

		pass

if __name__ == "__main__":

	solver = IMPES()

	# FORMATION

	solver.res = Formation("FU",None)

	solver.res.set_dimensions((1200,600,200))

	solver.res.grid((3,3,1))

	solver.res.set_porosity(0.26)

	solver.res.set_permeability(1800)

	solver.res.set_compressibility(3e-6)

	# FLUID

	solver.fluids = Fluids(2)

	solver.fluids.set_names(("oil","water"))
	solver.fluids.set_compressibility((5e-6,2e-6))
	solver.fluids.set_viscosity((5.,1.))
	solver.fluids.set_fvf((1,1))

	# WELLS

	solver.wells = Wells(None)

	solver.wells.set_names(["inj","prod1","prod2"])

	solver.wells.set_tracks((
		(( 600,300,400),( 600,300,0)),
		(( 200,100,400),( 200,100,0)),
		((1000,500,400),(1000,500,0)),
		))

	solver.wells.set_flowconds(
		conditions=("rate","rate","bhp"),
		limits=(3000,-2000,800),
		fluids=("water","oil","both"))

	solver.wells.set_skinfactors((0,0,0))

	solver.wells.set_radii((0.5,0.5,0.5))

	# RELATIVE PERMEABILITY

	solver.rp = relative_permeability(
			Sorow=0.2,
			Swc=0.2,
			krowc=1.0,
			krwor=0.2,
			no=3,
			nw=3,
			)

	solver.rp.system2phase(Sw=np.linspace(0.2,0.8),model="oil-water")

	# SOLVING

	solver.merge()
	solver.initialize(pressure=1000,saturation=0.2)
	solver.set_time(0.1,500)
	solver.solve()

	# PLOTTING

	fig = plt.figure()

	ax = plt.axes(projection='3d')

	# ax.scatter3D(*res.edge_vertices.T)

	for line in solver.res.edge_lines:
	    ax.plot3D(*line,color='grey')

	for track,name in zip(solver.wells.tracks,solver.wells.itemnames):

		ax.plot3D(*track.T,color="black",linewidth=1)
		ax.text(*track[0,:],name)

	ax.scatter3D(*solver.res.grid_centers.T)

	ax.set_xlabel("x-axis")
	ax.set_ylabel("y-axis")

	ax.set_box_aspect(solver.res.lengths)

	ax.set_axis_off()

	ax.margins(x=0,y=0)

	plt.tight_layout()

	# plt.figure()

	# plt.plot(solver.rp.Sw,solver.rp.kro,"k--",label="oil")
	# plt.plot(solver.rp.Sw,solver.rp.krw,"k-",label="water")

	# plt.xlabel("Water Saturation")
	# plt.ylabel("Relative Permeability")

	# plt.legend()

	# plt.xlim((0,1))

	# plt.show()


