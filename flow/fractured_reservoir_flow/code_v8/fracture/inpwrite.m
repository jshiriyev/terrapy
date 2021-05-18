function inpwrite(fileName,varUnit,varValue)

    headName = 'fileHead.txt';
    
    fid = fopen(headName,'w+');
    
    fprintf(fid,[varUnit,'\n']);    % 'length, SI'
    
    fclose(fid);
    
    bodyName = 'fileBody.txt';
    
    dlmwrite(bodyName,varValue,'delimiter','\t');
    
    system(['type fileHead.txt fileBody.txt > ',fileName]);
    
    delete fileHead.txt fileBody.txt;
    
end