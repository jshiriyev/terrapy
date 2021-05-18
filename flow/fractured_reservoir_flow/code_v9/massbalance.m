classdef massbalance
    
    properties
        fracQ1
        fracQf
        fracQw
        wellPn
        wellQw
    end
    
    methods (Static)
        
        function obj = massbalance(frac,well)
            obj.fracQ1 = frac.nodeID == frac.map(:,1)';
            obj.fracQf = frac.nodeID == frac.map(:,2)';
            obj.fracQw = frac.nodeID == well.wellID';
            obj.wellPn = well.wellID == frac.nodeID';
            obj.wellQw = well.wellID == well.wellID';
        end
        
        function FracQ1 = arrayFracQ1(obj)
            FracQ1 = double(obj.fracQf)-double(obj.fracQ1);  %*obj.scaleF
        end
        
        function FracQf = arrayFracQf(obj,frac)
            FracQf = double(obj.fracQf);
            FracQf(obj.fracQf) = frac.Length;   % *obj.scaleF
        end
        
        function FracQw = arrayFracQw(obj)
            FracQw = -double(obj.fracQw);        % *obj.scaleF
        end
        
        function WellPn = arrayWellPn(obj)
            WellPn = double(obj.wellPn);        % *obj.scaleF
        end
        
        function WellPn = vecWellPn(well)  
            WellPn = well.consPressure;      % *obj.scaleF
        end
        
        function WellQw = arrayWellQw(obj)
            WellQw = double(obj.wellQw);        % *obj.scaleF
        end
        
        function WellQw = vecWellQw(well)
            WellQw = well.consFlowrate;  % *obj.scaleF
        end
        
    end
    
end

