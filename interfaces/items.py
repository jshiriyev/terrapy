import calendar

from datetime import datetime
from dateutil.parser import parse
from dateutil.relativedelta import relativedelta

import warnings

import matplotlib.pyplot as plt

import numpy as np

if __name__ == "__main__":
    import setup

from interfaces.dataset import dataset

# % The fracture segment is defined as a plane joining two node points
# % (point1 and point2). The heigth of fracture plane is taken the same
# % as reservoir thickness (it is not diffcult to model shorter planes).
# % z-coordinate of the points is given as the reservoir depth.

# fileDir
# nodeCoord
# map
# permeability
# width
# fracID
# nodeID
# numAfrac
# numAnode
# conductivity
# point1
# point2
# Length
# areatoreservoir
# areatofracture
# volume
# center
# signX
# signY
# azimuth

# fileDir
# Length
# xLength
# yLength
# zLength
# porosity
# permeability
# xPermeability
# yPermeability
# zPermeability
# initPressure
# diffusivity
# xDiffusivity
# yDiffusivity
# zDiffusivity
# isotropic
# anisotropic
# rockCompressibility
# oilViscosity
# oilFVF      % formation volume factor
# oilCompressibility
# totCompressibility

class Pipes():

    def __init__(self):

        pass

    def set_pipe(self,inner_diameter=None,outer_diameter=None,length=1,roughness=None):
        
        self.ID = inner_diameter
        self.OD = outer_diameter
        self.length = length
        self.roughness = roughness

        """cross sectional area"""
        self.csa = np.pi*self.ID**2/4

        """
        hydraulic_radius: the ratio of the cross-sectional area of a channel or pipe
        in which a fluid is flowing to the wetted perimeter of the conduit
        """
        self.hydraulic_radius = self.ID/4

        self.roughness_relative = self.roughness/self.ID

    def set_pipe2(self,inner_diameter=None,outer_diameter=None,length=1,roughness=None):

        self.ID2 = inner_diameter
        self.OD2 = outer_diameter
        self.length2 = length
        self.roughness2 = roughness

        """cross sectional area"""
        self.csa2 = np.pi*(self.ID2**2-self.OD**2)/4

        """
        hydraulic_radius: the ratio of the cross-sectional area of a channel or pipe
        in which a fluid is flowing to the wetted perimeter of the conduit
        """
        self.hydraulic_radius2 = (self.ID2-self.OD)/4

        self.roughness_relative2 = self.roughness2/self.ID2

    def set_nodes(self,zloc=None,elevation=[0,0]):

        """
        Nodes are the locations where the measurements are available, and
        coordinates are selected in such a way that:
        - r-axis shows radial direction
        - \theta-axis shows angular direction
        - z-axis shows lengthwise direction
        """

        if zloc is None:
            self.zloc = [0,self.length]

        self.elevation = elevation

class Core():

    def __init__(self):

        pass

class FormationCylindrical():
    # regular radial both numerically discretized and homogenous one
    def __init__(self,height,radius,porosity,permeability,compressibility):

        self.height = height
        self.radius = radius

        self.porosity = porosity
        self.permeability = permeability
        self.compressibility = compressibility

    def set_total_compressibility(self,fluid):

        self.ct = self.cr+fluid.cf
        
    def set_diffusivity(self,viscosity,total_compressibility):
        
        self.eta = (self.k)/(self.phi*self.mu*self.ct)

    def set_well(self,radius,location,true_vertical_depth,measured_depth):

        self.radius = radius
        
    def set_fracture_nodes(self,thickness,nodes):

        self.thickness = thickness

    def set_hydraulic_diffusivity(self,hydraulic_diffusivity=None):

        if hydraulic_diffusivity is not None:
            self.eta = hydraulic_diffusivity

class FormationRectangular():

    def __init__(self):

        pass

    def cartesian(self,length,grid_num):

        """
        length is a tuple with three entries for size in x,y,z direction
        for rectangular parallelepiped:
        """

        self.length_x = length[0]
        self.length_y = length[1]
        self.length_z = length[2]

        """
        grid_num is a tuple with three entries for discretization in x,y,z direction
        for rectangular parallelepiped:
        """

        self.num_x = grid_num[0]
        self.num_y = grid_num[1]
        self.num_z = grid_num[2]

        """
        self.num is a total number of grids for rectangular parallelepiped
        """
        self.num = self.num_x*self.num_y*self.num_z

        """
        self.id is a connectivity map and contain index of all grids
        their neighbours.
        """

        idx = np.arange(self.num)
        
        self.id = np.tile(idx,(7,1)).T

        self.id[idx.reshape(-1,self.num_x)[:,1:].ravel(),1] -= 1
        self.id[idx.reshape(-1,self.num_x)[:,:-1].ravel(),2] += 1
        self.id[idx.reshape(self.num_z,-1)[:,self.num_x:],3] -= self.num_x
        self.id[idx.reshape(self.num_z,-1)[:,:-self.num_x],4] += self.num_x
        self.id[idx.reshape(self.num_z,-1)[1:,:],5] -= self.num_x*self.num_y
        self.id[idx.reshape(self.num_z,-1)[:-1,:],6] += self.num_x*self.num_y

        """
        self.size is the size of grids in x,y,z direction
        """

        node_x = np.linspace(0,self.length_x,self.num_x+1)
        node_y = np.linspace(0,self.length_y,self.num_y+1)
        node_z = np.linspace(0,self.length_z,self.num_z+1)
        
        xsize = node_x[1:]-node_x[:-1]
        ysize = node_y[1:]-node_y[:-1]
        zsize = node_z[1:]-node_z[:-1]
        
        self.size = np.zeros((self.num,3))
        self.size[:,0] = np.tile(xsize,self.num_y*self.num_z)
        self.size[:,1] = np.tile(ysize.repeat(self.num_x),self.num_z)
        self.size[:,2] = zsize.repeat(self.num_x*self.num_y)

        """
        self.area is the area of three faces of grids in x,y,z direction
        """
        
        self.area = np.zeros((self.num,3))
        self.area[:,0] = self.size[:,1]*self.size[:,2]
        self.area[:,1] = self.size[:,2]*self.size[:,0]
        self.area[:,2] = self.size[:,0]*self.size[:,1]

        """
        self.volume is the volume of grids in x,y,z direction
        """

        self.volume = np.prod(self.size,axis=1)

        xcenter = node_x[:-1]+xsize/2
        ycenter = node_y[:-1]+ysize/2
        zcenter = node_z[:-1]+zsize/2

        """
        self.center is the x,y,z coordinate of the center of grids
        """
        
        self.center = np.zeros((self.num,3))
        self.center[:,0] = np.tile(xcenter,self.num_y*self.num_z)
        self.center[:,1] = np.tile(ycenter.repeat(self.num_x),self.num_z)
        self.center[:,2] = zcenter.repeat(self.num_x*self.num_y)

class Fractures():

    def __init__(self):

        pass

class Wells(dataset):
    #previously known as schedule

    def __init__(self,fprod,fcomp):

        super().__init__(None)

        # KEYWORDS = [DATES,COMPDATMD,COMPORD,WCONHIST,WCONINJH,WEFAC,WELOPEN]

        self.strdates       = " {} / "#.format(date)

        self.strwspec       = " '{}'\t1*\t2* / "
        self.strcompdat     = " '{}'\t1*\t{}\t{}\tMD\t{}\t2*\t0.14 / "#.format(wellname,top,bottom,status)
        self.strcompord     = " '{}'\tINPUT\t/ "#.format(wellname)
        self.strprodhist    = " '{}'\tOPEN\tORAT\t{}\t{}\t{} / "#.format(wellname,oilrate,waterrate,gasrate)
        self.strinjhist     = " '{}'\tWATER\tOPEN\t{}\t7*\tRATE / "#.format(wellname,waterrate)
        self.strwefac       = " '{}'\t{} / "#.format(wellname,efficiency)
        self.strwopen       = " '{}'\tSHUT\t3* / "#.format(wellname)

        # MAIN SCHEDULE DATA

        self._headers = ["DATE","KEYWORD","DETAILS"]
        self._running = [np.array([]) for _ in self._headers]

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

        # INPUT, production and completion data

        self.fprod = fprod
        self.fcomp = fcomp

        self.prods = dataset(fprod,skiplines=1)
        self.comps = dataset(fcomp,skiplines=1)

        self.prods.texttocolumn(0,"\t",maxsplit=7)
        self.comps.texttocolumn(0,"\t",maxsplit=6)

        self.prods.get_columns(headers=["WELL","DATE","DAYS","OPROD","WPROD","GPROD","WINJ"],inplace=True)
        self.comps.get_columns(headers=["WELL","DATE","EVENT","TOP","BOTTOM"],inplace=True)

        self.prods.sort(header_indices=[1],inplace=True)
        self.comps.sort(header_indices=[1],inplace=True)

        self.prodwellnames = np.unique(self.prods.running[0])

        self.prods.astype(1,dtype=np.datetime64,datestring=True,shiftmonths=-1)
        self.prods.astype(2,dtype=np.int64)
        self.prods.astype(3,dtype=np.float64)
        self.prods.astype(4,dtype=np.float64)
        self.prods.astype(5,dtype=np.float64)
        self.prods.astype(6,dtype=np.float64)

        self.compwellnames = np.unique(self.comps.running[0])

        self.comps.astype(1,dtype=np.datetime64,datestring=True)
        self.comps.astype(3,dtype=np.float64)
        self.comps.astype(4,dtype=np.float64)

    def getwell(self,wellname):

        self.prods.filter(0,keywords=[wellname],inplace=False)
        self.comps.filter(0,keywords=[wellname],inplace=False)

        class well: pass

        well.name = wellname

        well.opdates = self.prods.running[1]

        well.opdays = self.prods.running[2]

        well.prodtotal = self.prods.running[3]+self.prods.running[4]+self.prods.running[5]
        well.injtotal = self.prods.running[6]

        well.total = well.prodtotal+well.injtotal

        well.opstatus = np.empty(well.opdays.shape,dtype=object)

        well.oil = np.zeros(well.opdays.shape,dtype=np.float32)
        well.water = np.zeros(well.opdays.shape,dtype=np.float32)
        well.gas = np.zeros(well.opdays.shape,dtype=np.float32)

        well.opstatus[well.prodtotal>0] = "production"

        well.oil[well.prodtotal>0] = self.prods.running[3][well.prodtotal>0]
        well.water[well.prodtotal>0] = self.prods.running[4][well.prodtotal>0]
        well.gas[well.prodtotal>0] = self.prods.running[5][well.prodtotal>0]
        
        well.opstatus[well.injtotal>0] = "injection"

        well.water[well.injtotal>0] = self.prods.running[6][well.injtotal>0]

        well.compdates = self.comps.running[1]

        well.compevents = self.comps.running[2]
        well.compuppers = self.comps.running[3]
        well.complowers = self.comps.running[4]

        well.compupdatedates = np.unique(well.compdates)

        compcounts = np.insert(np.cumsum(np.sum(well.compdates==well.compupdatedates.reshape((-1,1)),axis=1)),0,0)

        well.compupdatecounts = np.empty(well.compupdatedates.shape,dtype=int)

        compopenintervals = np.empty((0,2))

        for index,date in enumerate(well.compupdatedates):

            compevents = well.compevents[compcounts[index]:compcounts[index+1]]
            compuppers = well.compuppers[compcounts[index]:compcounts[index+1]]
            complowers = well.complowers[compcounts[index]:compcounts[index+1]]

            perfevents = compevents=="PERF"

            perfintervals = np.array([compuppers[perfevents],complowers[perfevents]]).T

            compopenintervals = np.concatenate((compopenintervals,perfintervals),axis=0)

            plugevents = compevents=="PLUG"

            pluguppermatch = np.any(compopenintervals[:,0]==compuppers[plugevents].reshape((-1,1)),axis=0)
            pluglowermatch = np.any(compopenintervals[:,1]==complowers[plugevents].reshape((-1,1)),axis=0)

            plugmatch = np.where(np.logical_and(pluguppermatch,pluglowermatch))[0]

            compopenintervals = np.delete(compopenintervals,plugmatch,0)

            well.compupdatecounts[index] = compopenintervals.shape[0]

        return well

    def wellcompletioncheck(self,well):

        compdata = zip(well.compdates,well.compevents,well.compuppers,well.complowers)

        for index,(compdate,compevent,compupper,complower) in enumerate(compdata):

            if index==0:
                self.set_rows([compdate,"WELSPECS",self.strwspec.format(well.name)])

            if compevent == "PERF":
                bottom = complower
                perfs = "OPEN"
            elif compevent == "PLUG":
                bottom = "1*"
                perfs = "SHUT"

            self.set_rows([compdate,"COMPDATMD",self.strcompdat.format(well.name,compupper,bottom,perfs)])
            self.set_rows([compdate,"COMPORD",self.strcompord.format(well.name)])

    def wellcrosscheck(self,well,flagReturn=False,flagShow=False):

        warnWPLUGPERF = "{:%Y-%m-%d}: {} first perf and last plug dates do not fit production days."
        warnWPERF = "{:%Y-%m-%d}: {} first perf date does not fit production days."
        warnWPLUG = "{:%Y-%m-%d}: {} last plug date does not fit production days."
        warnWEFAC = "{:%Y-%m-%d}: {} efficiency is more than unit [{:2d} out of {:2d} days]."

        flagNoPrevProd = True

        print("{} check is in progress ...".format(well.name))

        opdata = zip(well.opdates,well.opdays,well.opstatus,well.oil,well.water,well.gas)

        shutdates = []

        for index,(date,day,status,oil,water,gas) in enumerate(opdata):

            prodmonthSTARTday = date+relativedelta(days=1)

            prodmonthdaycount = calendar.monthrange(prodmonthSTARTday.year,prodmonthSTARTday.month)[1]

            prodmonthENDday = datetime(prodmonthSTARTday.year,prodmonthSTARTday.month,prodmonthdaycount)

            if np.sum(well.compupdatedates<prodmonthSTARTday)==0:
                compSTARTindex = 0
            else:
                compSTARTindex = np.sum(well.compupdatedates<prodmonthSTARTday)-1

            compENDindex = np.sum(well.compupdatedates<=prodmonthENDday)

            compupdatecounts = well.compupdatecounts[compSTARTindex:compENDindex]

            compupdatedates = well.compupdatedates[compSTARTindex:compENDindex]

            perfdates = compupdatedates[compupdatecounts!=0]
            plugdates = compupdatedates[compupdatecounts==0]

            try:
                flagNoPostProd = True if well.opdates[index+1]-relativedelta(months=1)>prodmonthENDday else False
            except IndexError:
                flagNoPostProd = True

            if np.sum(well.compupdatedates<prodmonthSTARTday)==0:
                flagCompShutSTART = True
            else:
                flagCompShutSTART = compupdatecounts[0]==0

            flagCompShutEND = compupdatecounts[-1]==0

            flagPlugPerf = any([compopencount==0 for compopencount in compupdatecounts[1:-1]])

            if flagCompShutSTART and flagCompShutEND:
                compday = plugdates[-1].day-perfdates[0].day
                prodeff = day/compday
                if status == "production":
                    self.set_rows([perfdates[0],"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                elif status == "injection":
                    self.set_rows([perfdates[0],"WCONINJH",self.strinjhist.format(well.name,water)])
                self.set_rows([perfdates[0],"WEFAC",self.strwefac.format(well.name,prodeff)])
                well.opdates[index] = perfdates[0]
                self.set_rows([plugdates[-1],"WELOPEN",self.strwopen.format(well.name)])
                shutdates.append(plugdates[-1])
                flagNoPrevProd = True
                if flagShow:
                    print("{:%d %b %Y} Peforated and Plugged: OPEN ({:%d %b %Y}) and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,perfdates[0],plugdates[-1],prodeff))

            elif flagCompShutSTART:
                compday = prodmonthENDday.day-perfdates[0].day
                prodeff = day/compday
                if status == "production":
                    self.set_rows([perfdates[0],"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                elif status == "injection":
                    self.set_rows([perfdates[0],"WCONINJH",self.strinjhist.format(well.name,water)])
                self.set_rows([perfdates[0],"WEFAC",self.strwefac.format(well.name,prodeff)])
                well.opdates[index] = perfdates[0]
                if flagNoPostProd:
                    self.set_rows([prodmonthENDday,"WELOPEN",self.strwopen.format(well.name)])
                    shutdates.append(prodmonthENDday)
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} Peforated and Open: OPEN ({:%d %b %Y}) and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,perfdates[0],prodmonthENDday,prodeff))
                else:                  
                    flagNoPrevProd = False
                    if flagShow:
                        print("{:%d %b %Y} Peforated and Open: OPEN ({:%d %b %Y}) and CONT WEFAC ({:.3f})".format(prodmonthENDday,perfdates[0],prodeff))

            elif flagCompShutEND:
                for plugdate in plugdates:
                    if plugdate.day>=day: break
                if not plugdate.day>=day:
                    warnings.warn(warnWPLUG.format(prodmonthENDday,well.name))
                compday = plugdate.day
                prodeff = day/compday
                if status == "production":
                    self.set_rows([date,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                elif status == "injection":
                    self.set_rows([date,"WCONINJH",self.strinjhist.format(well.name,water)])
                self.set_rows([date,"WEFAC",self.strwefac.format(well.name,prodeff)])
                self.set_rows([plugdate,"WELOPEN",self.strwopen.format(well.name)])
                shutdates.append(plugdate)
                flagNoPrevProd = True
                if flagShow:
                    print("{:%d %b %Y} Open and Plugged: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,plugdate,prodeff))

            elif flagPlugPerf:
                if flagNoPrevProd and flagNoPostProd:
                    # shift the start day to the first perf day
                    # shut the well at the last plug day
                    if not plugdates[-1].day-perfdates[1].day>=day:
                        warnings.warn(warnWPLUGPERF.format(prodmonthENDday,well.name))
                    compday = plugdates[-1].day-perfdates[1].day
                    prodeff = day/compday
                    if status == "production":
                        self.set_rows([perfdates[1],"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                    elif status == "injection":
                        self.set_rows([perfdates[1],"WCONINJH",self.strinjhist.format(well.name,water)])
                    self.set_rows([perfdates[1],"WEFAC",self.strwefac.format(well.name,prodeff)])
                    well.opdates[index] = perfdates[1]
                    self.set_rows([plugdates[-1],"WELOPEN",self.strwopen.format(well.name)])
                    shutdates.append(plugdates[-1])
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} Plugged and Perforated: OPEN ({:%d %b %Y}) and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,perfdates[1],plugdates[-1],prodeff))
                elif flagNoPrevProd and not flagNoPostProd:
                    # shift the start day to the proper perf day
                    for perfdate in np.flip(perfdates[1:]):
                        if prodmonthENDday.day-perfdate.day>=day: break
                    if not prodmonthENDday.day-perfdate.day>=day:
                        warnings.warn(warnWPERF.format(prodmonthENDday,well.name))
                    compday = prodmonthENDday.day-perfdate.day
                    prodeff = day/compday
                    if status == "production":
                        self.set_rows([perfdate,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                    elif status == "injection":
                        self.set_rows([perfdate,"WCONINJH",self.strinjhist.format(well.name,water)])
                    self.set_rows([perfdate,"WEFAC",self.strwefac.format(well.name,prodeff)])
                    well.opdates[index] = perfdate
                    flagNoPrevProd = False
                    if flagShow:
                        print("{:%d %b %Y} Plugged and Perforated: OPEN ({:%d %b %Y}) and CONT WEFAC ({:.3f})".format(prodmonthENDday,perfdate,prodeff))
                elif not flagNoPrevProd and flagNoPostProd:
                    # try shut the well at the proper plug day if not successful shut it at the end of month
                    for plugdate in plugdates:
                        if plugdate.day>=day: break
                    if not plugdate.day>=day:
                        plugdate = prodmonthENDday
                    compday = plugdate.day
                    prodeff = day/compday
                    if status == "production":
                        self.set_rows([date,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                    elif status == "injection":
                        self.set_rows([date,"WCONINJH",self.strinjhist.format(well.name,water)])
                    self.set_rows([date,"WEFAC",self.strwefac.format(well.name,prodeff)])
                    self.set_rows([plugdate,"WELOPEN",self.strwopen.format(well.name)])
                    shutdates.append(plugdate)
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} Plugged and Perforated: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,plugdate,prodeff))
                elif not flagNoPrevProd and not flagNoPostProd:
                    # try shut the well if not successful do nothing
                    for plugdate in plugdates:
                        if plugdate.day>=day: break
                    if not plugdate.day>=day:
                        compday = prodmonthdaycount
                        prodeff = day/compday
                        flagNoPrevProd = False
                        if flagShow:
                            print("{:%d %b %Y} Plugged and Perforated: CONT and CONT WEFAC ({:.3f})".format(prodmonthENDday,prodeff))
                    else:
                        compday = plugdate.day
                        prodeff = day/compday
                        self.set_rows([plugdate,"WELOPEN",self.strwopen.format(well.name)])
                        shutdates.append(plugdate)
                        flagNoPrevProd = True
                        if flagShow:
                            print("{:%d %b %Y} Plugged and Perforated: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,plugdate,prodeff))
                    if status == "production":
                        self.set_rows([date,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                    elif status == "injection":
                        self.set_rows([date,"WCONINJH",self.strinjhist.format(well.name,water)])
                    self.set_rows([date,"WEFAC",self.strwefac.format(well.name,prodeff)])

            else:
                compday = prodmonthdaycount
                prodeff = day/compday
                if status == "production":
                    self.set_rows([date,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                elif status == "injection":
                    self.set_rows([date,"WCONINJH",self.strinjhist.format(well.name,water)])
                self.set_rows([date,"WEFAC",self.strwefac.format(well.name,prodeff)])
                if flagNoPostProd:
                    self.set_rows([prodmonthENDday,"WELOPEN",self.strwopen.format(well.name)])
                    shutdates.append(prodmonthENDday)
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} No completion events: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,prodmonthENDday,prodeff))
                else:
                    flagNoPrevProd = False
                    if flagShow:
                        print("{:%d %b %Y} No completion events: CONT and CONT WEFAC ({:.3f})".format(prodmonthENDday,prodeff))

            if prodeff>1:
                warnings.warn(warnWEFAC.format(prodmonthENDday,well.name,day,compday))

            well.shutdates = np.array(shutdates)

        if flagShow:
            print("{} check is complete.".format(well.name))

        if flagReturn:

            well.opdates = np.append(well.opdates,well.shutdates)
            well.total = np.append(well.total,np.zeros(well.shutdates.shape))
            
            sortindex = np.argsort(well.opdates)

            well.opdates = well.opdates[sortindex]
            well.total = well.total[sortindex]

            return well

    def wellcrossplot(self,wellname,fignum=1):

        well = self.getwell(wellname)

        fig = plt.figure(num=fignum,figsize=(16,6),tight_layout=True)

        ax1 = fig.add_subplot(1,2,1)
        ax2 = ax1.twinx()

        ax1.scatter(well.opdates,well.total)

        ax1.step(well.opdates,well.total,'b',where='post')
        ax2.step(well.compupdatedates,well.compupdatecounts,'r--',where='post')

        ax1.set_ylabel('Total Production or Injection [m3/day]')
        ax2.set_yticks([])

        ax1.set_title("BEFORE CORRECTIONS")

        ax1.set_ylim(ymin=0,ymax=max(well.total)*1.1)
        ax2.set_ylim(ymin=0,ymax=max(well.compupdatecounts)+0.5)

        for tick in ax1.get_xticklabels():
            tick.set_rotation(45)

        well = self.wellcrosscheck(well,flagReturn=True,flagShow=True)

        ax3 = fig.add_subplot(1,2,2)
        ax4 = ax3.twinx()

        ax3.scatter(well.opdates,well.total)

        ax3.step(well.opdates,well.total,'b',where='post')
        ax4.step(well.compupdatedates,well.compupdatecounts,'r--',where='post')

        ax3.set_yticks([])
        ax4.set_ylabel('Open Perforation Intervals',rotation=270)

        ax3.set_title("AFTER CORRECTIONS")

        ax4.yaxis.set_label_coords(1.05,0.5)

        ax3.set_ylim(ymin=0,ymax=max(well.total)*1.1)
        ax4.set_ylim(ymin=0,ymax=max(well.compupdatecounts)+0.5)

        ax4.set_yticks(range(0,max(well.compupdatecounts)+1))

        for tick in ax3.get_xticklabels():
            tick.set_rotation(45)

        plt.show()

    def productioncheck(self):

        # warnings.filterwarnings("ignore",'.*',UserWarning)

        if any(self.prods.running[3]<0):
            for index in np.where(self.self.prods.running[3]<0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} oil production has negative entry.".format(date,well))

        if any(self.prods.running[4]<0):
            for index in np.where(self.self.prods.running[4]<0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} water production has negative entry.".format(date,well))

        if any(self.prods.running[5]<0):
            for index in np.where(self.self.prods.running[5]<0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} gas production has negative entry.".format(date,well))

        if any(self.prods.running[6]<0):
            for index in np.where(self.self.prods.running[6]<0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} water injection has negative entry.".format(date,well))

        producedtotal = self.prods.running[3]+self.prods.running[4]+self.prods.running[5]
        injectedtotal = self.prods.running[6]

        producedmonthcounts = np.sum(producedtotal!=0)
        injectedmonthcounts = np.sum(injectedtotal!=0)

        totalmonthcounts = self.prods.running[2].size

        if any(producedtotal+injectedtotal==0):
            for index in np.where(producedtotal+injectedtotal==0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} has zero production and injection.".format(date,well))

        if producedmonthcounts+injectedmonthcounts>totalmonthcounts:
            for index in np.where(np.logical_and(producedtotal!=0,injectedtotal!=0))[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} has both production and injection data.".format(date,well))

        # production dates must be the last day of month

    def completioncheck(self):
        # completion data should be sorted, first perforation of the interval and then plug
        # for the future there can be more than two completion scenarios (perf and plug)
        # completion top must be smaller than bottom
        # they must be positive values

        for wellname in self.prodwellnames:

            well = self.getwell(wellname)

            self.wellcompletioncheck(well)

    def crosscheck(self):

        warnNOPROD = "{} has completion but no production data."
        warnNOCOMP = "{} has production but no completion data."

        warnCROSS = "{} production has been defined before completion."

        for wellname in np.setdiff1d(self.prodwellnames,self.compwellnames):
            warnings.warn(warnNOCOMP.format(wellname))

        for wellname in np.setdiff1d(self.compwellnames,self.prodwellnames):
            warnings.warn(warnNOPROD.format(wellname))

        for wellname in self.prodwellnames:

            well = self.getwell(wellname)

            try:
                datemin = well.opdates.min()
            except ValueError:
                datemin = datetime(3000,1,1)

            date = datemin+relativedelta(months=1)

            days = calendar.monthrange(date.year,date.month)[1]

            date = datetime(date.year,date.month,days)

            if well.compupdatedates.min()>=date:
                warnings.warn(warnCROSS.format(well.name))

        for wellname in self.prodwellnames:

            well = self.getwell(wellname)

            self.wellcrosscheck(well,flagReturn=False,flagShow=False)

    def write(self,filepath):

        with open(filepath,"w",encoding='utf-8') as wfile:

            welspec = self.running[1]=="WELSPECS"
            compdat = self.running[1]=="COMPDATMD"
            compord = self.running[1]=="COMPORD"
            prodhst = self.running[1]=="WCONHIST"
            injdhst = self.running[1]=="WCONINJH"
            wefffac = self.running[1]=="WEFAC"
            welopen = self.running[1]=="WELOPEN"

            for date in np.unique(self.running[0]):

                currentdate = self.running[0]==date

                currentcont = self.running[1][currentdate]

                wfile.write("\n\n")
                wfile.write("DATES\n")
                wfile.write(self.strdates.format(date.strftime("%d %b %Y").upper()))
                wfile.write("\n")
                wfile.write("/\n\n")

                if any(currentcont=="WELSPECS"):
                    indices = np.logical_and(currentdate,welspec)
                    wfile.write("WELSPECS\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="COMPDATMD"):
                    indices = np.logical_and(currentdate,compdat)
                    wfile.write("COMPDATMD\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="COMPORD"):
                    indices = np.logical_and(currentdate,compord)
                    wfile.write("COMPORD\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WCONHIST"):
                    indices = np.logical_and(currentdate,prodhst)
                    wfile.write("WCONHIST\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WCONINJH"):
                    indices = np.logical_and(currentdate,injdhst)
                    wfile.write("WCONINJH\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WEFAC"):
                    indices = np.logical_and(currentdate,wefffac)
                    wfile.write("WEFAC\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WELOPEN"):
                    indices = np.logical_and(currentdate,welopen)
                    wfile.write("WELOPEN\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

if __name__ == "__main__":

    import unittest

    from tests import pipes
    from tests import porous_media
    from tests import fractures
    from tests import wells

    unittest.main(pipes)
    unittest.main(porous_media)
    unittest.main(fractures)
    unittest.main(wells)
