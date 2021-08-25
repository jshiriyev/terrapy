function vtkwrite(res,frac,well,time,sol)
    
    % deleteing files in results file
    delete 'results\*.fig'
    delete 'results\*.vtk'
    
    % conversion to field units    
    time.tau = time.tau/inpput.convFactorDetermine('time');
    sol.pressure = sol.pressure/inpput.convFactorDetermine('pressure');
    
    % writing time values
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
        fprintf(fid,'%f\r\n',sol.pressure(i,j));
    end
    
    fclose(fid);
    
    end

end