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
    Nu.p5=double(NNu(0,-v));
    Nu.p6=double(NNu(v,0));
    Nu.p7=double(NNu(0,v));
    Nu.p8=double(NNu(-v,0));
    Nu.p9=double(NNu(0,0));

    NNp(t,z)=(Ap'\Xp')';
    Np.p1=double(NNp(-v,-v));
    Np.p2=double(NNp(v,-v));
    Np.p3=double(NNp(v,v));
    Np.p4=double(NNp(-v,v));
    Np.p5=double(NNp(0,-v));
    Np.p6=double(NNp(v,0));
    Np.p7=double(NNp(0,v));
    Np.p8=double(NNp(-v,0));
    Np.p9=double(NNp(0,0));

    diffNu(t,z)=[diff(NNu,t); diff(NNu,z)];
    DNu.p1=double(diffNu(-v,-v));
    DNu.p2=double(diffNu(v,-v));
    DNu.p3=double(diffNu(v,v));
    DNu.p4=double(diffNu(-v,v));
    DNu.p5=double(diffNu(0,-v));
    DNu.p6=double(diffNu(v,0));
    DNu.p7=double(diffNu(0,v));
    DNu.p8=double(diffNu(-v,0));
    DNu.p9=double(diffNu(0,0));
    
end

