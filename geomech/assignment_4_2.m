clear all; clc;

a=1; uo=100;

syms x y t z
assume(x,'real')
assume(y,'real')
assume(t,'real')
assume(z,'real')

en=[1 2 6 5; 2 3 7 6; 3 4 8 7; 5 6 10 9; 6 7 11 10; 7 8 12 11];
nc=[0 0; a 0; 2*a 0; 3*a 0; 0 a; a a; 2*a a; 3*a a; 0 2*a; a 2*a; 2*a 2*a; 3*a 2*a;];

C=eye(3);

Ne=6; Nn=12;
K=zeros(Nn,Nn);
F=zeros(Nn,1);

Xiso=[1 t z t*z];
A=[1 -1 -1 1; 1 1 -1 -1; 1 1 1 1; 1 -1 1 -1];
Niso=(A'\Xiso')';
D=sym(zeros(4,4));
v=1/sqrt(3);

for e=1:Ne
    Niso=(A'\Xiso')';
    x=nc(en(e,:),1)'*Niso';
    y=nc(en(e,:),2)'*Niso';
    J=[diff(x,t) diff(x,z) 0; diff(y,t) diff(y,z) 0; 0 0 1];
    Biso=J\[diff(Niso,t); diff(Niso,z); Niso];
    D(t,z)=Biso'*C*Biso*det(J);
    k=D(v,v)+D(-v,v)+D(-v,-v)+D(v,-v);
    K(en(e,:),en(e,:))=K(en(e,:),en(e,:))+k;
end

% Applying Dirichlet Boundary Conditions

K(4,:)=0;   K(4,4)=1;   F(4,1)=0;
K(8,:)=0;   K(8,8)=1;   F(8,1)=0;
K(9,:)=0;   K(9,9)=1;   F(9,1)=uo*cos(pi*nc(9,1)/6/a);
K(10,:)=0;  K(10,10)=1; F(10,1)=uo*cos(pi*nc(10,1)/6/a);
K(11,:)=0;  K(11,11)=1; F(11,1)=uo*cos(pi*nc(11,1)/6/a);
K(12,:)=0;  K(12,12)=1; F(12,1)=uo*cos(pi*nc(12,1)/6/a);

u=K\F;

% Plot

X=linspace(0,3,4);
Y=linspace(0,2,3);
U=(reshape(u,4,3))';
[C,h]=contourf(X,Y,U,'ShowText','on');
axis equal tight
colorbar
caxis([0 100]);

