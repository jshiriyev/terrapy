function [ke,qe] = estiffandq(e,Np,DNu)

    global coord connect alfa m C Nld
    
    Dmat=[1 0 0 0; 0 0 0 1; 0 1 1 0];
    
    J=DNu*coord(connect(e,:),:);
    
    Jmat(1:2,1:2)=inv(J);
    Jmat(3:4,3:4)=inv(J);
    
    Nmat(1:2,1:2:2*Nld-1)=DNu;
    Nmat(3:4,2:2:2*Nld)=DNu;
    
    B=Dmat*Jmat*Nmat;

    ke=B'*C*B*det(J);
    qe=alfa*B'*m*Np*det(J);
    
end





































