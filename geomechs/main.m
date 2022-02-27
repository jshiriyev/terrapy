clear all; close all; clc;

global Nld m Ey nu Cmat

%% Global Mesh Properties

coord=[0 0; 1 0; 1 1; 0 1];
cnnct=[1 2 3 4];

Ne=size(cnnct,1);
Nod=length(coord);

defPos=coord;

%% Material Properties

Ey=200;
nu=0.29;

Cmat(1,:)=[Ey/(1-nu^2) nu*Ey/(1-nu^2) 0];
Cmat(2,:)=[nu*Ey/(1-nu^2) Ey/(1-nu^2) 0];
Cmat(3,:)=[0 0 Ey/(2*(1+nu))];

%% Element Analysis

Nld=4;
m=[1 1 0]';
dNu=computeParel(Nld);

sn.p1=[0 0 0]';
sn.p2=[0 0 0]';
sn.p3=[0 0 0]';
sn.p4=[0 0 0]';

e.p1=[0 0 0]';
e.p2=[0 0 0]';
e.p3=[0 0 0]';
e.p4=[0 0 0]';

ep.p1=0;
ep.p2=0;
ep.p3=0;
ep.p4=0;

T=500;
stress=zeros(T+1,3);
strain=zeros(T+1,3);

for i=1:T
    disp=zeros(Nod,2);
    disp(2,:)=disp(2,:)+[0.001 0];
    disp(3,:)=disp(3,:)+[0.001 0];
    for j=1:10
        [f,snp1,de,dep]=computeForce(defPos,dNu,sn,disp);
        dbc=[1 2 3 4 5 7];
        f(dbc,1)=zeros(size(dbc,2),1);
        res=norm(f);
        if(res<0.001)
            break
        end
        K=computeTgStif(defPos,dNu,sn,disp);
        K(dbc,:)=0;
        K(dbc,dbc)=K(dbc,dbc)+eye(size(dbc,2));
        u=K\f;
        disp=disp+reshape(u',2,Nld)';
    end
    defPos=defPos+disp;
    sn.p1=snp1.p1;
    sn.p2=snp1.p2;
    sn.p3=snp1.p3;
    sn.p4=snp1.p4;
    e.p1=e.p1+de.p1;
    e.p2=e.p2+de.p2;
    e.p3=e.p3+de.p3;
    e.p4=e.p4+de.p4;
    ep.p1=ep.p1+dep.p1;
    ep.p2=ep.p2+dep.p2;
    ep.p3=ep.p3+dep.p3;
    ep.p4=ep.p4+dep.p4;
    stress(i+1,:)=sn.p3';
    strain(i+1,:)=e.p3';
end

disp=defPos-coord;
display(disp);

plot(strain(:,1),stress(:,1))
xlabel('strain')
ylabel('stress')