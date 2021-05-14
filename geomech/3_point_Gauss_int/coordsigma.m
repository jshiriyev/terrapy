function [xs,ys,Sigma] = coordsigma(e,Nu,DNu,u)

    global coord connect C Nld

    Dmat=[1 0 0 0; 0 0 0 1; 0 1 1 0];
    
    xs=Nu*coord(connect(e,:),1);
    ys=Nu*coord(connect(e,:),2);
    
    J=DNu*coord(connect(e,:),:);
    
    Jmat(1:2,1:2)=inv(J);
    Jmat(3:4,3:4)=inv(J);
    
    Nmat(1:2,1:2:2*Nld-1)=DNu;
    Nmat(3:4,2:2:2*Nld)=DNu;
    
    B=Dmat*Jmat*Nmat;
    
    Sigma=C*B*u;
    
end

