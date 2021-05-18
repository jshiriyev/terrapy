function [res,frac,bhc,time] = unitconvert(res,frac,bhc,time,type)

% ---------------------------------------------------------------------- %
%   
%   conversion from field units to SI units
%
% ---------------------------------------------------------------------- %
    
    if strcmp(type,'Field_to_SI')
        id = 1;
    elseif strcmp(type,'SI_to_Field')
        id = -1;
    else
        error('could not figure out conversion type')
    end

    res.xlength = res.xlength*0.3048^id;	% [ft] to [m]
    res.ylength = res.ylength*0.3048^id;	% [ft] to [m]
    res.zlength = res.zlength*0.3048^id;    % [ft] to [m]
    
    res.totcompres = res.totcompres/6894.76^id;	% [1/psi] to [1/Pa]
    
    res.fluid.viscosity = res.fluid.viscosity*1e-3^id;	% [cp] to [Pa.s]
    
    res.xpermeability = res.xpermeability*9.869233e-16^id;	% [mD] to [m2]
    res.ypermeability = res.ypermeability*9.869233e-16^id;	% [mD] to [m2]
    res.zpermeability = res.zpermeability*9.869233e-16^id;	% [mD] to [m2]
        
    res.initpressure = res.initpressure*6894.76^id;	% [psi] to [Pa]
    
    frac.nodes = frac.nodes*0.3048^id;   %[ft] to [m]
    
    frac.permeability = frac.permeability*9.869233e-16^id; % [mD] to [m2]
    
    frac.width = frac.width*0.3048^id;	% [ft] to [m]
    
    if bhc.type == 0
        bhc.cond = bhc.cond*6894.76^id;         % [psi] to [Pa]
    elseif bhc.type == 1
        bhc.cond = bhc.cond/(6.29*24*60*60)^id;	% [bbl/day] to [m3/sec]
    end
    
    time.tt = time.tt*(24*60*60)^id;	% [day] to [sec]
    time.dt = time.dt*(24*60*60)^id;	% [day] to [sec]
    
end