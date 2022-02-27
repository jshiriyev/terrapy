function [Nu,Np,DNu] = parentel()
    
    global v Nld Nlp
    
    syms t z

    assume(t,'real')
    assume(z,'real')
    
    X=[1 t z t*z t^2 z^2 t^2*z t*z^2 t^2*z^2];
    cooiso=[-1 -1; 1 -1; 1 1; -1 1; 0 -1; 1 0; 0 1; -1 0; 0 0];
    
    Xu(t,z)=X(1:Nld);
    Xp(t,z)=X(1:Nlp);

    Au=sym(zeros(Nld,Nld));
    Ap=sym(zeros(Nlp,Nlp));

    for i=1:Nld
        Au(i,:)=Xu(cooiso(i,1),cooiso(i,2));
    end

    for i=1:Nlp
        Ap(i,:)=Xp(cooiso(i,1),cooiso(i,2));
    end

    Xu=X(1:Nld);
    Xp=X(1:Nlp);

    NNu(t,z)=(Au'\Xu')';
    Nu.p1=double(NNu(-v,-v));
    Nu.p2=double(NNu(v,-v));
    Nu.p3=double(NNu(v,v));
    Nu.p4=double(NNu(-v,v));

    NNp(t,z)=(Ap'\Xp')';
    Np.p1=double(NNp(-v,-v));
    Np.p2=double(NNp(v,-v));
    Np.p3=double(NNp(v,v));
    Np.p4=double(NNp(-v,v));

    diffNu(t,z)=[diff(NNu,t); diff(NNu,z)];
    DNu.p1=double(diffNu(-v,-v));
    DNu.p2=double(diffNu(v,-v));
    DNu.p3=double(diffNu(v,v));
    DNu.p4=double(diffNu(-v,v));
    
end

