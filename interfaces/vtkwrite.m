function vtkwrite(frac,time,sol)
    
    % deleteing files in results file
    
    delete 'results\*.fig'
    delete 'results\*.vtk'
    delete 'results\*.out'
    
    % conversion to field units
    
    T = time.tau/setup.convFactorDetermine('time');
    
    Pf = sol.pressure/setup.convFactorDetermine('pressure');
%   Qf = sol.fracflux/setup.convFactorDetermine('velocity');
    
    Pw = sol.wellpressure/setup.convFactorDetermine('pressure');
    Qw = sol.wellflowrate/setup.convFactorDetermine('flowrate');
    
    % writing time values of well pressure and flowrate
    
    fid = fopen('results\solution.out','w');
    
    fprintf(fid,'FRACTURE FLOW ANALYTICAL SOLUTION\r\n');
    fprintf(fid,'WELL PRESSURE AND FLOW-RATE\r\n');
    fprintf(fid,'\r\n%-10s\t%-10s\t%-10s\r\n','Time','Pressure','Flow-Rate');
    fprintf(fid,'%-10s\t%-10s\t%-10s\r\n','[days]','[psi]','[bbl/day]');
    
    fclose(fid);
    
    dlmwrite('results\solution.out',[T,Pw',Qw'],'-append',...
             'delimiter','\t','precision','%-10.3f');
    
    % writing time values of fracture pressure
    
    for j = 1:time.numTimeStep
    
        fid = fopen(['results\fracPressure',num2str(j),'.vtk'],'w');

        fprintf(fid,'# vtk DataFile Version 1.0\r\n');
        fprintf(fid,'FRACTURE FLOW ANALYTICAL SOLUTION\r\n');
        fprintf(fid,'ASCII\r\n');

        fprintf(fid,'\r\nDATASET UNSTRUCTURED_GRID\r\n');

        fprintf(fid,'\r\nPOINTS %d FLOAT\r\n',frac.numAnode*2);

        for i = 1:frac.numAnode
            fprintf(fid,'%f %f %f\r\n',frac.nodeCoord(i,:));
        end

        for i = 1:frac.numAnode
            fprintf(fid,'%f %f %f\r\n',[frac.nodeCoord(i,1:2),0]);
        end

        fprintf(fid,'\r\nCELLS %d %d\r\n',frac.numAfrac,5*frac.numAfrac);

        for i = 1:frac.numAfrac
            fprintf(fid,'%d %d %d %d %d\r\n',[4,frac.map(i,:)-1,frac.map(i,:)+frac.numAnode-1]);
        end

        fprintf(fid,'\r\nCELL_TYPES %d\r\n',frac.numAfrac);

        for i = 1:frac.numAfrac
            fprintf(fid,'%d\r\n',8);
        end

        fprintf(fid,'\r\nCELL_DATA %d\r\n',frac.numAfrac);
        fprintf(fid,'SCALARS pressure float\r\n');
        fprintf(fid,'LOOKUP_TABLE default\r\n');

        for i = 1:frac.numAfrac
            fprintf(fid,'%f\r\n',Pf(i,j));
        end

        fclose(fid);
    
    end

end