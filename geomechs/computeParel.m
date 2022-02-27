function [dNu]=computeParel(Nld)
    
    syms t z

    assume(t,'real')
    assume(z,'real')
    
    Xiso=[1 t z t*z t^2 z^2 t^2*z t*z^2 t^2*z^2];
    ciso=[-1 -1; 1 -1; 1 1; -1 1; 0 -1; 1 0; 0 1; -1 0; 0 0];
    
    Xu(t,z)=Xiso(1:Nld);
    Au=sym(zeros(Nld,Nld));
    
    for i=1:Nld
        Au(i,:)=Xu(ciso(i,1),ciso(i,2));
    end
    
    Xu=Xiso(1:Nld);
    
    NNu(t,z)=(Au'\Xu')';
    diffNu(t,z)=[diff(NNu,t); diff(NNu,z)];
    
    v=sqrt(1/3);
    dNu.p1=double(diffNu(v,v));
    dNu.p2=double(diffNu(-v,v));
    dNu.p3=double(diffNu(v,-v));
    dNu.p4=double(diffNu(-v,-v));
    
end

