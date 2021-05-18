classdef fracture
    %UNTITLED Summary of this class goes here
    %   Detailed explanation goes here
    
    properties
        Property1
    end
    
    methods
        
        function obj = fracture(inputArg1,inputArg2)
            %UNTITLED Construct an instance of this class
            %   Detailed explanation goes here
            obj.Property1 = inputArg1 + inputArg2;
        end
        
        function outputArg = method1(obj,inputArg)
            %METHOD1 Summary of this method goes here
            %   Detailed explanation goes here
            outputArg = obj.Property1 + inputArg;
        end
        
        function outputArg = method1(obj,inputArg)
            
            cfA = (res.fluid.viscosity)./FC;
            
            cf2 = cfA.*frac.length;
            cf3 = cfA.*(frac.length.^2)/2;
        
            A31 = sparse(Nf,Nw);
            A32 = sparse(idy1d,idx_d,cf2(idy1d),Nf,Nd);
            A33 = sparse(idx_f,idx_f,cf3,Nf,Nf);
            A34 = sparse([idx_f,idx_f],frac.map,[-ones(Nf,1),ones(Nf,1)],Nf,Nn);
            
            b3 = sparse(Nf,1);
            
        end
        
    end
    
    methods (Static)
        function obj = equations
            A31 = sparse(Nf,Nw);
            A34 = sparse([idx_f,idx_f],frac.map,[-ones(Nf,1),ones(Nf,1)],Nf,Nn);
        end
    end
    
end

