import matplotlib.pyplot as plt 
import numpy as np 

from scipy.sparse import csr_matrix as csr

from scipy.sparse.linalg import spsolve as sps

if __name__ == "__main__":
    import setup

from flow.pormed.conrelation import relative_permeability

from flow.fluids import Fluids

from interfaces.items import Formation
from interfaces.items import Wells

class IMPES():
	
	def __init__(self):

		pass

	def set_time(self,step,total):

		self.time_step = step
		self.time_total = total

		self.time_array = np.arange(0,self.time_total+self.time_step,self.time_step)

	def initialize(self,pressure,saturation):

		self.pressure = np.empty(self.res.grid_numtot)

		self.Sw = np.empty(self.res.grid_numtot)

		self.pressure[:] = pressure

		self.Sw[:] = saturation

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

		d11 = (self.res.grid_volumes*self.res.porosity*self.Sw)/(fvfw*self.time_step)*(cr+cw)
		d12 = (self.res.grid_volumes*self.res.porosity)/(self.time_step*fvfw)
		d21 = (self.res.grid_volumes*self.res.porosity*(1-self.Sw))/(fvfo*self.time_step)*(cr+co)
		d22 = (self.res.grid_volumes*self.res.porosity)/(self.time_step*fvfo)*-1

		self.rp.system2phase(Sw=self.Sw,model="oil-water")

		TXw = (kx*self.rp.krw*Ax)/(muw*fvfw*dx)
		TYw = (ky*self.rp.krw*Ay)/(muw*fvfw*dy)
		TZw = (kz*self.rp.kro*Az)/(muo*fvfo*dz)

		TXn = (kx*self.rp.kro*Ax)/(muo*fvfo*dx)
		TYn = (ky*self.rp.kro*Ay)/(muo*fvfo*dy)
		TZn = (kz*self.rp.kro*Az)/(muo*fvfo*dz)

		shape = (self.res.grid_numtot,self.res.grid_numtot)

		self.Tw = csr(shape)

		self.Tw -= csr((TXw[self.res.noxmin],(self.res.grid_indices[self.res.noxmin,0],self.res.grid_indices[self.res.noxmin,1])),shape=shape)
		self.Tw += csr((TXw[self.res.noxmin],(self.res.grid_indices[self.res.noxmin,0],self.res.grid_indices[self.res.noxmin,0])),shape=shape)

		self.Tw -= csr((TXw[self.res.noxmax],(self.res.grid_indices[self.res.noxmax,0],self.res.grid_indices[self.res.noxmax,2])),shape=shape)
		self.Tw += csr((TXw[self.res.noxmax],(self.res.grid_indices[self.res.noxmax,0],self.res.grid_indices[self.res.noxmax,0])),shape=shape)

		self.Tw -= csr((TYw[self.res.noymin],(self.res.grid_indices[self.res.noymin,0],self.res.grid_indices[self.res.noymin,3])),shape=shape)
		self.Tw += csr((TYw[self.res.noymin],(self.res.grid_indices[self.res.noymin,0],self.res.grid_indices[self.res.noymin,0])),shape=shape)

		self.Tw -= csr((TYw[self.res.noymax],(self.res.grid_indices[self.res.noymax,0],self.res.grid_indices[self.res.noymax,4])),shape=shape)
		self.Tw += csr((TYw[self.res.noymax],(self.res.grid_indices[self.res.noymax,0],self.res.grid_indices[self.res.noymax,0])),shape=shape)

		self.Tw -= csr((TZw[self.res.nozmin],(self.res.grid_indices[self.res.nozmin,0],self.res.grid_indices[self.res.nozmin,5])),shape=shape)
		self.Tw += csr((TZw[self.res.nozmin],(self.res.grid_indices[self.res.nozmin,0],self.res.grid_indices[self.res.nozmin,0])),shape=shape)

		self.Tw -= csr((TZw[self.res.nozmax],(self.res.grid_indices[self.res.nozmax,0],self.res.grid_indices[self.res.nozmax,6])),shape=shape)
		self.Tw += csr((TZw[self.res.nozmax],(self.res.grid_indices[self.res.nozmax,0],self.res.grid_indices[self.res.nozmax,0])),shape=shape)

		self.Tn = csr(shape)

		self.Tn -= csr((TXn[self.res.noxmin],(self.res.grid_indices[self.res.noxmin,0],self.res.grid_indices[self.res.noxmin,1])),shape=shape)
		self.Tn += csr((TXn[self.res.noxmin],(self.res.grid_indices[self.res.noxmin,0],self.res.grid_indices[self.res.noxmin,0])),shape=shape)

		self.Tn -= csr((TXn[self.res.noxmax],(self.res.grid_indices[self.res.noxmax,0],self.res.grid_indices[self.res.noxmax,2])),shape=shape)
		self.Tn += csr((TXn[self.res.noxmax],(self.res.grid_indices[self.res.noxmax,0],self.res.grid_indices[self.res.noxmax,0])),shape=shape)

		self.Tn -= csr((TYn[self.res.noymin],(self.res.grid_indices[self.res.noymin,0],self.res.grid_indices[self.res.noymin,3])),shape=shape)
		self.Tn += csr((TYn[self.res.noymin],(self.res.grid_indices[self.res.noymin,0],self.res.grid_indices[self.res.noymin,0])),shape=shape)

		self.Tn -= csr((TYn[self.res.noymax],(self.res.grid_indices[self.res.noymax,0],self.res.grid_indices[self.res.noymax,4])),shape=shape)
		self.Tn += csr((TYn[self.res.noymax],(self.res.grid_indices[self.res.noymax,0],self.res.grid_indices[self.res.noymax,0])),shape=shape)

		self.Tn -= csr((TZn[self.res.nozmin],(self.res.grid_indices[self.res.nozmin,0],self.res.grid_indices[self.res.nozmin,5])),shape=shape)
		self.Tn += csr((TZn[self.res.nozmin],(self.res.grid_indices[self.res.nozmin,0],self.res.grid_indices[self.res.nozmin,0])),shape=shape)

		self.Tn -= csr((TZn[self.res.nozmax],(self.res.grid_indices[self.res.nozmax,0],self.res.grid_indices[self.res.nozmax,6])),shape=shape)
		self.Tn += csr((TZn[self.res.nozmax],(self.res.grid_indices[self.res.nozmax,0],self.res.grid_indices[self.res.nozmax,0])),shape=shape)

		print((self.Tw.todense()))

class SS():
	
	def __init__(self):

		pass

	def set_time(self,step,total):

		self.timesteps = np.arange(0,total+step,step)

	def solve():

		pass

if __name__ == "__main__":

	solver = IMPES()

	solver.set_time(0.1,500)

	# FORMATION

	solver.res = Formation("FU",None)

	solver.res.set_dimensions((1200,600,200))

	solver.res.grid((3,3,1))

	solver.res.set_porosity(0.26)

	solver.res.set_permeability(((1800,1800,1800),))

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

	solver.wells.set_flowconds(conditions=("rate","rate","bhp"),boundaries=(3000,2000,800))

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

	solver.initialize(pressure=1000,saturation=0.2)
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


