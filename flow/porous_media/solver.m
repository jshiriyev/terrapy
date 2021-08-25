classdef solver
    
    properties
        currentTimeStep
        wellpressure
        wellflowrate
        pressure
        fracflux
    end
    
    methods (Static)
        
        function obj = solver(res,frac,well,time,Gplane)
            
            % for now only constant fracture properties is modeled
            
%             T = frac.permeability.*frac.areatofracture/...
%                 res.oilViscosity/res.oilFVF./frac.Length;

            req = 0.14*sqrt(frac.Length(well.wellID).^2+res.zLength^2);
            
            J = 2*pi*frac.width(well.wellID).*frac.permeability(well.wellID)./...
                    (res.oilViscosity*res.oilFVF.*log(req/well.radius));
                
            if ~isempty(well.consPressure)
                Q = J.*well.consPressure;
                Jmat = sparse(well.wellID,well.wellID,J,frac.numAfrac,frac.numAfrac);
            elseif ~isempty(well.consFlowrate)
                Q = -well.consFlowrate;
                Jmat = sparse(frac.numAfrac,frac.numAfrac);
            end
            
            Qvec = sparse(frac.numAfrac,1);
            Qvec(well.wellID) = Qvec(well.wellID)+Q;
            
            Tmat = solver.transmissibility(res,frac);

            obj.pressure = zeros(frac.numAfrac,time.numTimeStep);
            obj.fracflux = zeros(frac.numAfrac,time.numTimeStep);
            
            Af = diag(frac.areatoreservoir);
            
            for N = 1:time.numTimeStep
                
                Amat = (Tmat+Jmat)*Gplane(:,:,1)*time.deltaTime+Af/N;
                
                green = solver.convolution(Gplane,obj.fracflux,2,N)*time.deltaTime;
                fluxx = sum(obj.fracflux(:,1:N-1),2);
            
                bvec = (Tmat+Jmat)*(res.initPressure-green)-Af/N*fluxx-Qvec;

                obj.fracflux(:,N) = Amat\bvec;
%               obj.pressure(:,N+1) = (Tmat+Jmat)\(Af*obj.fracflux(:,N+1)+Qvec);
                obj.pressure(:,N) = res.initPressure-Gplane(:,:,1)*obj.fracflux(:,N)*time.deltaTime-green;
                
                disp(['Time iteration... ',num2str(N/time.numTimeStep*100),'% is complete'])

            end
            
            if ~isempty(well.consPressure)
                obj.wellpressure = zeros(1,time.numTimeStep)+well.consPressure;
                obj.wellflowrate = -Q+J.*obj.pressure(well.wellID,:);
            elseif ~isempty(well.consFlowrate)
                obj.wellpressure = obj.pressure(well.wellID,:)+Q./J;
                obj.wellflowrate = zeros(1,time.numTimeStep)-Q;
            end
            
        end
        
        function Tmat = transmissibility(res,frac)
            
            % construction of transmissibility matrix
            
            Tmat = zeros(frac.numAfrac,frac.numAfrac);
            
            for host = 1:frac.numAfrac
                
                [neighbor,~] = find(sum(permute(frac.map,[1,3,2])==frac.map(host,:),3));
    
                neighbor(neighbor==host)=[];

                if length(neighbor)~=length(unique(neighbor))
                    error('there is an error in the fracture map')
                end
                
                deltax = (frac.Length(host)+frac.Length(neighbor))/2;
                
                perm = deltax./(frac.Length(host)/frac.permeability(host)/2+...
                    frac.Length(neighbor)./frac.permeability(neighbor)/2);
                
                trans = perm.*frac.areatofracture(host)/...
                        res.oilViscosity/res.oilFVF./deltax;

                Tmat(host,host) = sum(trans);
                Tmat(host,neighbor) = -trans;

            end
            
        end
        
        function array = convolution(matrix,vector,timeidx1,timeidx2)

            switch nargin
                case 2
                    timeidx1 = 1;
                    timeidx2 = size(vector,2);
            end
            
            n1 = timeidx1:timeidx2;
            n2 = timeidx2-n1+1;
            
            G = matrix(:,:,n1);
            Q = permute(vector(:,n2),[3,1,2]);
            
            array = sum(sum(G.*Q,2),3);

        end
        
    end
    
end