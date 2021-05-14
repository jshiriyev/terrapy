clear all; clc;

% Initialization
syms x
a(x)=x; c(x)=x; f(x)=x;

%% Input
a(x)=1;
c(x)=0;
f(x)=x^2;
uo=0;
Qo=2;
xj=[1 2 3 4]';

%% Computation
Ne=size(xj,1)-1;
xi=[xj(1:Ne),xj(2:Ne+1)];
Lo=xj(1); Lf=xj(Ne+1);

% Shape Functions
N=sym(zeros(Ne,2));

for el=1:Ne
    A=[1 xi(el,1); 1 xi(el,2)];
    C=A\[1;0];
    N(el,1)=C(1,1)+C(2,1)*x;
    C=A\[0;1];
    N(el,2)=C(1,1)+C(2,1)*x;
end

dN=diff(N,x);

% Generation of Stiffness and Force matrices
K=zeros(Ne+1,Ne+1);
F=zeros(Ne+1,1);

for el=1:Ne
    K(el,el)=K(el,el)+int((a*dN(el,1)*dN(el,1)+c*N(el,1)*N(el,1)),x,xi(el,1),xi(el,2));
    K(el,el+1)=K(el,el+1)+int((a*dN(el,1)*dN(el,2)+c*N(el,1)*N(el,2)),x,xi(el,1),xi(el,2));
    K(el+1,el)=K(el+1,el)+int((a*dN(el,2)*dN(el,1)+c*N(el,2)*N(el,1)),x,xi(el,1),xi(el,2));
    K(el+1,el+1)=K(el+1,el+1)+int((a*dN(el,2)*dN(el,2)+c*N(el,2)*N(el,2)),x,xi(el,1),xi(el,2));
    F(el,1)=F(el,1)+int(f*N(el,1),x,xi(el,1),xi(el,2));
    F(el+1,1)=F(el+1,1)+int(f*N(el,2),x,xi(el,1),xi(el,2));
end

% Application of Dirichlet Boundary Condition at x=Lo
K(1,:)=0;
K(1,1)=1;
F(1,1)=uo;

% Application of Neumann Boundary Condition at x=Lf
F(Ne+1,1)=F(Ne+1,1)+Qo;

% Solution for u
u=K\F;
fprintf('%7s%11s\n','x', 'u(x)');
fprintf('%7.2f %10.5f\n',[xj,u].');

% %% The End %%