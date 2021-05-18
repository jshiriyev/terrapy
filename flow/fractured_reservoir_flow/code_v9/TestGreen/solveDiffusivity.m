function pwf = solveDiffusivity(T,res,well)

    timeUpper = 948*res.porosity*res.fluidViscosity*...
        res.totCompressibility*(res.xLength/2)^2/res.permeability;

    bndCond = timeUpper<=T;

    timePs = T(bndCond);

    timePs = timePs';

    reD = (res.xLength/2)/(well.radius);
    
    tD = 0.000264*(res.permeability*timePs)/...
         (res.porosity*res.fluidViscosity*...
          res.totCompressibility*well.radius^2);

    pwf = res.initPressure-141.2*(well.consFlowrate*res.fluidViscosity)/...
          (res.permeability*res.zLength)*(2*tD/reD^2+log(reD)-3/4);

end