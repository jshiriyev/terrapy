classdef pressure
    
    % Equating the pressure at the center of fracture
    % first set of equations are from fracture flow
    % second set of equations are from analytical solution of reservoir
    % flow provided in form of Green's function.
    
    properties
        presQn
        presP1
    end
    
    methods (Static)
        
        function obj = pressure(frac)
            obj.presQn = frac.fracID == frac.fracID';
            obj.presP1 = frac.map(:,1) == frac.nodeID';
        end
        
        function ContQ1 = arrayContQ1(obj,res,frac)
            ContQ1 = double(obj.presQn);
            ContQ1(obj.presQn(:)) = res.fluidViscosity./...
                                    (frac.conductivity*res.zLength).*...
                                    frac.Length/2;
        end
        
        function ContQf = arrayContQf(obj,res,frac,gterm)
            ContQf = double(obj.presQn);
            ContQf(obj.presQn(:)) = res.fluidViscosity./...
                                    (frac.conductivity*res.zLength).*...
                                    (frac.Length.^2)/8;
            ContQf = ContQf-gterm(:,:,1);
        end
        
        function ContP1 = arrayContP1(obj)
            ContP1 = double(obj.presP1);
            ContP1(obj.presP1(:)) = -1;
        end
        
        function ContP1 = vecContPn(sol,res,frac,gterm,G)
            idxQ = sol.currentTimeStep:-1:1;
            idxG = 2:1:sol.currentTimeStep+1;
            ContP1 = sparse(frac.fracID,1,-res.initPressure*G.volume(:,:,sol.currentTimeStep+1))+...
                     sum(sum(permute(sol.qf(:,idxQ),[3,1,2]).*...
                     gterm(:,:,idxG),3),2);
        end
        
    end
    
end