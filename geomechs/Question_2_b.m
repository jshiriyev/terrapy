clear all; clc;

% Initialization

syms x

%% Input

mu=1;           % Pa.s
uo=100000;      % Pa
Qo=100;         % m/s

fileID = fopen('q2_b_pressure.txt');
AA=fscanf(fileID,'%f');
BB=reshape(AA,2,21)';

xj=BB(:,1);     % m
P=BB(:,2)*1000; % Pa

%% Computation

Ne=size(xj,1)-1;
xi=[xj(1:Ne),xj(2:Ne+1)];
Lo=xj(1); Lf=xj(Ne+1);

k=8.94*x^2+0.58*x+0.96;       % m^2
a=k/mu;                 % m^2/Pa/s

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
    K(el,el)=K(el,el)+int((a*dN(el,1)*dN(el,1)),x,xi(el,1),xi(el,2));
    K(el,el+1)=K(el,el+1)+int((a*dN(el,1)*dN(el,2)),x,xi(el,1),xi(el,2));
    K(el+1,el)=K(el+1,el)+int((a*dN(el,2)*dN(el,1)),x,xi(el,1),xi(el,2));
    K(el+1,el+1)=K(el+1,el+1)+int((a*dN(el,2)*dN(el,2)),x,xi(el,1),xi(el,2));
end

% Application of Dirichlet Boundary Condition at x=Lo

K(1,:)=0;
K(1,1)=1;
F(1,1)=uo;

% Application of Neumann Boundary Condition at x=Lf

F(Ne+1,1)=F(Ne+1,1)+Qo;

% Solution for u

u=K\F;
fprintf('%8s %10s %10s\n','x (m)', 'u(x)(Pa)', 'p(x)(Pa)');
fprintf('%7.2f %10.0f %10.0f\n',[xj,u,P].');

% %% The End %%