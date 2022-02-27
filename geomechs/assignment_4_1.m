clear all; clc;

a=1; uo=100;

syms x y
assume(x,'real')
assume(y,'real')

% Input
en=[1 2 6; 2 3 7; 3 4 8; 1 6 5; 2 7 6; 3 8 7; 5 6 10; 6 7 11; 7 8 12; 5 10 9; 6 11 10; 7 12 11];
nc=[0 0; a 0; 2*a 0; 3*a 0; 0 a; a a; 2*a a; 3*a a; 0 2*a; a 2*a; 2*a 2*a; 3*a 2*a;];
by=[0 x; 0 x-a; 0 x-2*a; x a; x-a a; x-2*a a; a x+a; a x; a x-a; x+a 2*a; x 2*a; x-a 2*a];
bx=[0 a; a 2*a; 2*a 3*a; 0 a; a 2*a; 2*a 3*a; 0 a; a 2*a; 2*a 3*a; 0 a; a 2*a; 2*a 3*a;];
C=eye(3);

Ne=12; Nn=12;
K=zeros(Nn,Nn);
F=zeros(Nn,1);
X=[1 x y];

for e=1:Ne
    xx=nc(en(e,:),1);
    yy=nc(en(e,:),2);
    A=[ones(3,1) xx yy];
    N=(A'\X')';
    Dx=diff(N,x);
    Dy=diff(N,y);
    B=[Dx; Dy; N];
    k=int(int(B'*C*B,y,by(e,1),by(e,2)),x,bx(e,1),bx(e,2));
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