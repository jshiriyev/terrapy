function [Kt]=computeTgStif(defpos,dNu,s,disp)
    
    global Nld

    h=1e-50;
    D=zeros(2,Nld);
    k=zeros(2*Nld,2*Nld);
    
    for i=1:2*Nld
        D(i)=1i*h;
        k(:,i)=computeForce(defpos,dNu,s,disp+transpose(D));
        D(i)=0;
    end
    
    Kt=-imag(transpose(k))/h;
    
end

