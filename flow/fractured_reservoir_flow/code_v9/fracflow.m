classdef fracflow
    
    % Equations for fluid flow from fractures to wellbore
    % Only Darcy flow behavior is simulated for now!
    
  properties
      flowQn
      flowP1
      flowP2
  end
    
  methods (Static)
        
    function obj = fracflow(frac)
        obj.flowQn = frac.fracID == frac.fracID';
        obj.flowP1 = frac.map(:,1) == frac.nodeID';
        obj.flowP2 = frac.map(:,2) == frac.nodeID';
    end
        
    function FlowQ1 = arrayFlowQ1(obj,res,frac)
        FlowQ1 = double(obj.flowQn);
        FlowQ1(obj.flowQn) = res.fluidViscosity./...
            (frac.conductivity*res.zLength).*frac.Length;
    end
        
    function FlowQf = arrayFlowQf(obj,res,frac)
        FlowQf = double(obj.flowQn);
        FlowQf(obj.flowQn) = res.fluidViscosity./...
            (frac.conductivity*res.zLength).*(frac.Length.^2)/2;
    end
        
    function FlowPn = arrayFlowPn(obj)
        FlowPn = double(obj.flowP1)+double(obj.flowP2);
        FlowPn(obj.flowP1) = -1;
        FlowPn(obj.flowP2) = 1;
    end
        
%     sigma_hormin = 5345;     % Wanrning!!! [psi]
%     sigma_closure = sigma_hormin-sol.pm(:,t)/6894.76;
%     norm_fraccond = power(10,-0.0004*sigma_closure+0.2191);
%     FC = FC.*norm_fraccond;

  end
    
end

