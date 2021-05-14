clear all; clc;

%% Input of Problem 1

global coord connect Nld Nlp alfa m C v 

alfa=1;         % Biot's coefficient
nu=0.3;         % Poisson's ratio
mu=1;           % shear modulus
m=[1; 1; 0];    % 2D analysis
v=sqrt(3/5);
w1=5/9;
w2=8/9;

% Stiffness Tensor for Plane Strain Conditions
C=zeros(3,3);
C(1,1)=2*mu*(1-nu)/(1-2*nu);
C(2,2)=C(1,1);
C(1,2)=2*mu*nu/(1-2*nu);
C(2,1)=C(1,2);
C(3,3)=mu;

% B.C. for presssure; (far field boundary is stress free)
po=5;           % interior pressure
pinf=1;         % far field pressure

% Coordinates of the Global Nodes
fid=fopen('coords.txt');
fis=fscanf(fid,'%f');
coord=reshape(fis,2,length(fis)/2)';

% Connectivity Array
fid=fopen('connect.txt');
fis=fscanf(fid,'%f');
connect=reshape(fis,9,length(fis)/9)';

% Interior Boundary Nodes
fid=fopen('interior.txt');
interior=fscanf(fid,'%f');

% Exterior Boundary Nodes
fid=fopen('exterior.txt');
exterior=fscanf(fid,'%f');

% y=0 line
fid=fopen('horsym.txt');
horsym=fscanf(fid,'%f');

% x=0 line
fid=fopen('versym.txt');
versym=fscanf(fid,'%f');

%% Parent Element Calculations

Nld=9;                              % # of Local Nodes for Displacement
Nlp=4;                              % # of Local Nodes for Pressure
Ne=size(connect,1);                 % Total # of Elements
Nod=size(unique(connect(:,1:9)),1);	% Total # of Nodes for Displacement
Nop=size(unique(connect(:,1:4)),1); % Total # of Nodes for Pressure

[Nu,Np,DNu]=parentel();

%% Degree of Freedom

count=1;
dof=zeros(Nod,3);

for i=1:Nod
    dof(i,1)=count; count=count+1;
    dof(i,2)=count; count=count+1;
    if any(any(connect(:,1:Nlp)==i))
        dof(i,3)=count; count=count+1;
    else
        dof(i,3)=NaN;
    end
end

%% Assembled Matrices

A=zeros(Nod*2+Nop);
F=zeros(Nod*2+Nop,1);

for e=1:Ne
    [ke1,qe1]=estiffandq(e,Np.p1,DNu.p1);
    [ke2,qe2]=estiffandq(e,Np.p2,DNu.p2);
    [ke3,qe3]=estiffandq(e,Np.p3,DNu.p3);
    [ke4,qe4]=estiffandq(e,Np.p4,DNu.p4);
    [ke5,qe5]=estiffandq(e,Np.p5,DNu.p5);
    [ke6,qe6]=estiffandq(e,Np.p6,DNu.p6);
    [ke7,qe7]=estiffandq(e,Np.p7,DNu.p7);
    [ke8,qe8]=estiffandq(e,Np.p8,DNu.p8);
    [ke9,qe9]=estiffandq(e,Np.p9,DNu.p9);
    idx=reshape((dof(connect(e,1:Nld),1:2))',1,Nld*2);
    idy=(dof(connect(e,1:Nlp),3))';
    ke=w1*w1*(ke1+ke2+ke3+ke4)+w1*w2*(ke5+ke6+ke7+ke8)+w2*w2*ke9;
    qe=w1*w1*(qe1+qe2+qe3+qe4)+w1*w2*(qe5+qe6+qe7+qe8)+w2*w2*qe9;
    A(idx,idx)=A(idx,idx)+ke;
    A(idx,idy)=A(idx,idy)-qe;
    A(idy,idx)=A(idy,idx)+qe';
end

%% Application of Boundary Conditions for Pressure

doub=dof(horsym,2);
dour=dof(versym,1);
dopi=dof(interior,3); dopi(isnan(dopi))=[];
dope=dof(exterior,3); dope(isnan(dope))=[];

A(doub,:)=0; A(doub,doub)=A(doub,doub)+eye(size(doub,1));
A(dour,:)=0; A(dour,dour)=A(dour,dour)+eye(size(dour,1));
A(dopi,:)=0; A(dopi,dopi)=A(dopi,dopi)+eye(size(dopi,1)); F(dopi,1)=po;
A(dope,:)=0; A(dope,dope)=A(dope,dope)+eye(size(dope,1)); F(dope,1)=pinf;

%% Solving for the general u_x, u_y and p

b=A\F;

dox=dof(:,1); ux=b(dox);
doy=dof(:,2); uy=b(doy);
dop=dof(:,3); dop(isnan(dop))=[]; p=b(dop);

%% Post Processing for Sigma_xx, Sigma_yy and Sigma_xy

xu=coord(:,1); xp=xu(unique(connect(:,1:Nlp)));
yu=coord(:,2); yp=yu(unique(connect(:,1:Nlp)));

Sigma=zeros(Ne*4,3); xs=zeros(Ne*4,1); ys=zeros(Ne*4,1);

for e=1:Ne
    idx=reshape((dof(connect(e,1:Nld),1:2))',1,Nld*2);
    [xs1,ys1,Sigma1]=coordsigma(e,Nu.p1,DNu.p1,b(idx,1));
    [xs2,ys2,Sigma2]=coordsigma(e,Nu.p2,DNu.p2,b(idx,1));
    [xs3,ys3,Sigma3]=coordsigma(e,Nu.p3,DNu.p3,b(idx,1));
    [xs4,ys4,Sigma4]=coordsigma(e,Nu.p4,DNu.p4,b(idx,1));
    [xs5,ys5,Sigma5]=coordsigma(e,Nu.p5,DNu.p5,b(idx,1));
    [xs6,ys6,Sigma6]=coordsigma(e,Nu.p6,DNu.p6,b(idx,1));
    [xs7,ys7,Sigma7]=coordsigma(e,Nu.p7,DNu.p7,b(idx,1));
    [xs8,ys8,Sigma8]=coordsigma(e,Nu.p8,DNu.p8,b(idx,1));
    [xs9,ys9,Sigma9]=coordsigma(e,Nu.p9,DNu.p9,b(idx,1));
    Sigma(9*e-8:9*e,:)=[Sigma1'; Sigma2'; Sigma3'; Sigma4'; Sigma5'; Sigma6'; Sigma7'; Sigma8'; Sigma9'];
    xs(9*e-8:9*e,1)=[xs1; xs2; xs3; xs4; xs5; xs6; xs7; xs8; xs9];
    ys(9*e-8:9*e,1)=[ys1; ys2; ys3; ys4; ys5; ys6; ys7; ys8; ys9];
end

Sxx=Sigma(:,1);
Syy=Sigma(:,2);
Sxy=Sigma(:,3);

plottingresults(xu,yu,xp,yp,xs,ys,ux,uy,Sxx,Syy,Sxy,p);