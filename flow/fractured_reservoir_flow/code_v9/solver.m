classdef solver
    
    % Fracture Modeling using Green's solution of Diffusivity Equation
    
    properties
        currentTimeStep
        q1 % flow at the starting point of fracture segment
        qw % wellbore flow rate
        qf % flux to fracture segments
        pn % pressure at the nodes
    end
    
    methods (Static)
        
        function obj = solver(res,frac,time,well,G)
        
        eq1_ID = (1:frac.numAnode);                                 % node mass balance
        eq2_ID = (1:frac.numAfrac)+frac.numAnode;                   % fracture flow
        eq3_ID = (1:frac.numAfrac)+frac.numAfrac+frac.numAnode;     % pressure continuity
        eq4_ID = (1:well.numWnode)+2*frac.numAfrac+frac.numAnode;	% wellbore conditions
        
        q1_ID = (1:frac.numAfrac);
        qw_ID = (1:well.numWnode)+frac.numAfrac;
        qf_ID = (1:frac.numAfrac)+frac.numAfrac+well.numWnode;
        pn_ID = (1:frac.numAnode)+2*frac.numAfrac+well.numWnode;
        
        obj.currentTimeStep = 0;
        
        obj.q1 = zeros(frac.numAfrac,time.numTimeStep);
        obj.qw = zeros(well.numWnode,time.numTimeStep);
        obj.qf = zeros(frac.numAfrac,time.numTimeStep);
        obj.pn = zeros(frac.numAnode,time.numTimeStep);

        Amat = zeros(2*frac.numAfrac+frac.numAnode+well.numWnode);
        bvec = zeros(2*frac.numAfrac+frac.numAnode+well.numWnode,1);
        
        mass = massbalance(frac,well);
        flow = fracflow(frac);
        cont = pressure(frac);
        
        gterm = G.cplane*time.deltaTime;
        
        Amat(eq1_ID,q1_ID) = massbalance.arrayFracQ1(mass);
        Amat(eq1_ID,qf_ID) = massbalance.arrayFracQf(mass,frac);
        Amat(eq1_ID,qw_ID) = massbalance.arrayFracQw(mass);
        
        Amat(eq2_ID,q1_ID) = fracflow.arrayFlowQ1(flow,res,frac);
        Amat(eq2_ID,qf_ID) = fracflow.arrayFlowQf(flow,res,frac);
        Amat(eq2_ID,pn_ID) = fracflow.arrayFlowPn(flow);

        Amat(eq3_ID,q1_ID) = pressure.arrayContQ1(cont,res,frac);
        Amat(eq3_ID,qf_ID) = pressure.arrayContQf(cont,res,frac,gterm);
        Amat(eq3_ID,pn_ID) = pressure.arrayContP1(cont);

        if ~isempty(well.consPressure)
            Amat(eq4_ID,pn_ID) = massbalance.arrayWellPn(mass);
            bvec(eq4_ID) = massbalance.vecWellPn(well);
        elseif ~isempty(well.consFlowrate)
            Amat(eq4_ID,qw_ID) = massbalance.arrayWellQw(mass);
            bvec(eq4_ID) = massbalance.vecWellQw(well);
        end

        % solving system of linear equations, x=A\b
        
        for timeStep = 1:time.numTimeStep
        
        bvec(eq3_ID) = pressure.vecContPn(obj,res,frac,gterm,G);
        
        x = Amat\bvec;

        obj.q1(:,timeStep) = x(q1_ID);
        obj.qw(:,timeStep) = x(qw_ID);
        obj.qf(:,timeStep) = x(qf_ID);
        obj.pn(:,timeStep) = x(pn_ID);

        obj.currentTimeStep = timeStep;

        disp(['Time iteration... ',num2str(timeStep/time.numTimeStep*100),'% is complete'])

        end
        
        end
        
    end
    
end