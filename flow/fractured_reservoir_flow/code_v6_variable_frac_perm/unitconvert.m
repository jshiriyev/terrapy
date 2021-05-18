function [res,frac,bhc,time] = unitconvert(res,frac,bhc,time)

% ---------------------------------------------------------------------- %
%   
%   conversion from field units to SI units
%
% ---------------------------------------------------------------------- %

    res.xlength.SI = res.xlength.FU*0.3048;	% [ft] to [m]
    res.ylength.SI = res.ylength.FU*0.3048;	% [ft] to [m]
    res.zlength.SI = res.zlength.FU*0.3048; % [ft] to [m]
    
    res.totcompres.SI = res.totcompres.FU/6894.76;              % [1/psi] to [1/Pa]
    
    res.fluid.viscosity.SI = res.fluid.viscosity.FU*1e-3;       % [cp] to [Pa.s]
    
    res.xpermeability.SI = res.xpermeability.FU*9.869233e-16;	% [mD] to [m2]
    res.ypermeability.SI = res.ypermeability.FU*9.869233e-16;	% [mD] to [m2]
    res.zpermeability.SI = res.zpermeability.FU*9.869233e-16;	% [mD] to [m2]
        
    res.initpressure.SI = res.initpressure.FU*6894.76;          % [psi] to [Pa]
    
    frac.nodes.SI = frac.nodes.FU*0.3048;                       %[ft] to [m]
    
    frac.permeability.SI = frac.permeability.FU*9.869233e-16;   % [mD] to [m2]
    
    frac.width.SI = frac.width.FU*0.3048;                       % [ft] to [m]
    
    if bhc.type == 0
        bhc.cond.SI = bhc.cond.FU*6894.76;                      % [psi] to [Pa]
    elseif bhc.type == 1
        bhc.cond.SI = bhc.cond.FU/(6.29*24*60*60);              % [bbl/day] to [m3/sec]
    end
    
    time.tt.SI = time.tt.FU*(24*60*60);                         % [day] to [sec]
    time.dt.SI = time.dt.FU*(24*60*60);                         % [day] to [sec]
    
end