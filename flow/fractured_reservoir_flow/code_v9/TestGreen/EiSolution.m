function pressure = EiSolution(R,T,res,well)
    
    % R is in feet
    % T is in hour
    
    T = T';
    
    cons1 = 0.000264;
    cons2 = 0.000264*948;
    cons3 = 0.000264*3.79e5;
    
    eta = cons1*res.permeability/...
        (res.porosity*res.fluidViscosity*res.totCompressibility);

    timeLower = cons3*well.radius^2/eta;

    timeUpper = cons2*(res.xLength/2)^2/eta;

    infCond = and(timeLower<T,T<timeUpper);
%     bndCond = obj.timeUpper<=T;

    timeEi = T(infCond);
%     timePs = T(bndCond);

    EiTerm = -expint((cons2*R.^2)./(eta*timeEi));

    pressure = res.initPressure+...
          70.6*(well.consFlowrate*res.fluidViscosity)/...
               (res.permeability*res.zLength)*EiTerm;

end

