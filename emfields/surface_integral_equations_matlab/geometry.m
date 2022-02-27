function [c1,c2] = geometry(string)

    % ------------------------------------------------------------------ %
    %     T:element(triangular) N:number    R:coordinates   V:vertex 
    %     C:center of element   l:length    A:area          D:edge
    % 
    %     INPUT
    %     origin: location of the center of fracture
    %     rinner: inner radius
    %     router: outer radius
    %     lambda: node spacing, radius ratio
    %     dangle: dip angle
    % 
    %     OUTPUT
    %     TtoV VtoR TtoD DtoT
    %     NofT NofD lofD AofT RofC
    %     RofCp RofCm rhocp rhocm
    % ------------------------------------------------------------------ %
    
    clear global
    
    fid = fopen(strcat(string,'fracture.txt'));
    
    origin = str2num(strtok(fgetl(fid),'%'));
    r_frac = str2num(strtok(fgetl(fid),'%'));
    aspect = str2num(strtok(fgetl(fid),'%'));
    dipang = str2num(strtok(fgetl(fid),'%'));
    S_frac = str2num(strtok(fgetl(fid),'%'));
    t_frac = str2num(strtok(fgetl(fid),'%'));
    r_well = str2num(strtok(fgetl(fid),'%'));
    lambda = str2num(strtok(fgetl(fid),'%'));
    
    fclose(fid);
    
    G_well = S_frac(1)*t_frac;
    G_edge = S_frac(2)*t_frac;
    
    global NofT NofD VtoR TtoD TtoD1 TtoV DtoT

    r_frac_major = r_frac;
    r_frac_minor = r_frac/aspect;

    r_well_major = r_well/cosd(dipang);
    r_well_minor = r_well;

    if r_well_minor>=r_frac_minor || r_well_major>=r_frac_major
        error('bad input');
    end

    radii_major = r_dotter([r_well_major r_frac_major],lambda);
    radii_minor = r_dotter([r_well_minor r_frac_minor],lambda);

    Nr1 = length(radii_major);
    Nr2 = length(radii_minor);

    if Nr1>=Nr2
        Nr = Nr1;
        radii_minor = r_dotter([r_well_minor r_frac_minor],lambda,Nr1);
    elseif Nr1<Nr2
        Nr = Nr2;
        radii_major = r_dotter([r_well_major r_frac_major],lambda,Nr2);
    end

    offset = 1/2/lambda;

    theta1 = t_dotter([0,pi/2,pi,3*pi/2,2*pi],lambda);
    theta2 = t_dotter([0,pi/2,pi,3*pi/2,2*pi]-offset,lambda);

    Nt = length(theta2);

    NofT = 2*Nt*(Nr-1);             % number of triangles
    NofD = Nt*(3*Nr-4);             % number of edges

    Vx(1:2:Nr,:) = radii_minor(1:2:end)*cos(theta1)';
    Vx(2:2:Nr,:) = radii_minor(2:2:end)*cos(theta2)';
    Vy(1:2:Nr,:) = radii_major(1:2:end)*sin(theta1)';
    Vy(2:2:Nr,:) = radii_major(2:2:end)*sin(theta2)';

    VtoR(:,1) = reshape(Vx',[],1);
    VtoR(:,2) = reshape(Vy',[],1);
    VtoR(:,3) = origin;

    NofV = size(VtoR,1);

    Rx(1,:) = [1 0 0];
    Rx(2,:) = [0 cosd(dipang) sind(dipang)];
    Rx(3,:) = [0 -sind(dipang) cosd(dipang)];

    for i = 1:NofV
        VtoR(i,:) = VtoR(i,:)*Rx;
    end

    TtoD = zeros(NofT,4);
    TtoD1(1:NofT,1:6) = NofD+1;
    TtoV = zeros(NofT,3);
    DtoT = zeros(NofD,6);  
    
    id1 = (1:1:Nt)';
    id2 = [id1(Nt);id1(1:Nt-1)];
    id3 = [id1(2:Nt);id1(1)];
    id4 = id1(1:2:Nt);
    id5 = id1(2:2:Nt);

    d1(1:1:2*Nt,1) = [id1(2:Nt);id1+Nt;id1(1)];
    d2(1:1:2*Nt,1) = [id1;id1+Nt];
    d3(1:2:2*Nt,1) = id1-4*Nt;
    d3(2:2:2*Nt,1) = id1-Nt;
    d4 = ones(2*Nt,1)*3;
    
    v123_1(1:2:2*Nt,:) = [id1 id1+Nt id3+Nt];
    v123_1(2:2:2*Nt,:) = [id1 id3+Nt id3];
    v123_2(1:2:2*Nt,:) = [id1 id1+Nt id3];
    v123_2(2:2:2*Nt,:) = [id3 id1+Nt id3+Nt];

    topm(1:2*Nt,:) = [[id1(Nt)+Nt;id1;id1(1:Nt-1)+Nt] [id1;id1+Nt]];
    vopmew_1(1:2:2*Nt,:) = [id2 id3+Nt id1 id1+Nt];
    vopmew_1(2:2:2*Nt,:) = [id1+Nt id3 id1 id3+Nt];
    vopmew_2(1:2:2*Nt,:) = [id2+Nt id3 id1 id1+Nt];
    vopmew_2(2:2:2*Nt,:) = [id1 id3+Nt id3 id1+Nt];

    tbpm_1(1:Nt,:) = [[id4;id4+Nt] [id4;id4+Nt]+2*Nt];
    tbpm_2(1:Nt,:) = [[id5;id5+Nt] [id5;id5+Nt]+2*Nt];
    vbpmew_1(1:Nt,:) = [id1 id1+2*Nt id3+Nt id1+Nt];
    vbpmew_2(1:Nt,:) = [id3 id3+2*Nt id3+Nt id1+Nt];

    for j = 1:Nr-1

        TtoD((j-1)*2*Nt+1:j*2*Nt,1:2) = [d1 d2]+(j-1)*3*Nt;
        
        TtoD1((j-1)*2*Nt+1:j*2*Nt,1) = d1+(j-1)*3*Nt;
        TtoD1((j-1)*2*Nt+1:j*2*Nt,5) = d2+(j-1)*3*Nt;
        
        if mod(j,2)==1
            d3(1:2:2*Nt,1) = d3(1:2:2*Nt,1)+6*Nt;
            TtoD((j-1)*2*Nt+1:j*2*Nt,3) = d3;
            TtoD1((j-1)*2*Nt+1:2:j*2*Nt,3) = d3(1:2:2*Nt,1);
            TtoD1((j-1)*2*Nt+2:2:j*2*Nt,6) = d3(2:2:2*Nt,1);
        else
            d3(2:2:2*Nt,1) = d3(2:2:2*Nt,1)+6*Nt;
            TtoD((j-1)*2*Nt+1:j*2*Nt,3) = d3;
            TtoD1((j-1)*2*Nt+2:2:j*2*Nt,3) = d3(2:2:2*Nt,1);
            TtoD1((j-1)*2*Nt+1:2:j*2*Nt,6) = d3(1:2:2*Nt,1);
        end

        TtoD((j-1)*2*Nt+1:j*2*Nt,4) = d4;

        if j == 1
            TtoD(2:2:2*Nt,3) = ones(Nt,1)*(NofD+1);
            TtoD(2:2:2*Nt,4) = ones(Nt,1)*2;
            TtoD1(2:2:2*Nt,6) = ones(Nt,1)*(NofD+1);
        end

        if j == Nr-1
            if mod(Nr-1,2)==1
                TtoD((j-1)*2*Nt+1:2:j*2*Nt,3) = ones(Nt,1)*(NofD+1);
                TtoD((j-1)*2*Nt+1:2:j*2*Nt,4) = ones(Nt,1)*2;
                TtoD1((j-1)*2*Nt+1:2:j*2*Nt,3) = ones(Nt,1)*(NofD+1);
            else
                TtoD((j-1)*2*Nt+2:2:j*2*Nt,3) = ones(Nt,1)*(NofD+1);
                TtoD((j-1)*2*Nt+2:2:j*2*Nt,4) = ones(Nt,1)*2;
                TtoD1((j-1)*2*Nt+2:2:j*2*Nt,3) = ones(Nt,1)*(NofD+1);
            end
        end

        if mod(j,2)==1
            TtoV((j-1)*2*Nt+1:j*2*Nt,:) = v123_1+(j-1)*Nt;
            DtoT((j-1)*3*Nt+1:(3*j-1)*Nt,:) = [topm+(j-1)*2*Nt vopmew_1+(j-1)*Nt];
        else
            TtoV((j-1)*2*Nt+1:j*2*Nt,:) = v123_2+(j-1)*Nt;
            DtoT((j-1)*3*Nt+1:(3*j-1)*Nt,:) = [topm+(j-1)*2*Nt vopmew_2+(j-1)*Nt];
        end

        if j ~= Nr-1
            if mod(j,2)==1
                DtoT((3*j-1)*Nt+1:3*j*Nt,:) = [tbpm_1+(j-1)*2*Nt vbpmew_1+(j-1)*Nt];
            else
                DtoT((3*j-1)*Nt+1:3*j*Nt,:) = [tbpm_2+(j-1)*2*Nt vbpmew_2+(j-1)*Nt];
            end
        end

    end

    global RofC GofT lofD AofT

    RofC = (VtoR(TtoV(:,1),:)+VtoR(TtoV(:,2),:)+VtoR(TtoV(:,3),:))/3;
    
    GofT = G_well-(sqrt(sum(RofC.^2,2))-r_well)/(r_frac-r_well)*(G_well-G_edge);
    
    lofD = sqrt(sum((VtoR(DtoT(:,5),:)-VtoR(DtoT(:,6),:)).^2,2));

    l1 = sqrt(sum((VtoR(TtoV(:,2),:)-VtoR(TtoV(:,1),:)).^2,2));
    l2 = sqrt(sum((VtoR(TtoV(:,3),:)-VtoR(TtoV(:,2),:)).^2,2));
    l3 = sqrt(sum((VtoR(TtoV(:,1),:)-VtoR(TtoV(:,3),:)).^2,2));

    S = (l1+l2+l3)/2;

    AofT = sqrt(S.*(S-l1).*(S-l2).*(S-l3));

    global RofCp RofCm rhocp rhocm

    RofCp = RofC(DtoT(:,1),:);
    RofCm = RofC(DtoT(:,2),:);

    rhocp = RofCp-VtoR(DtoT(:,3),:);
    rhocm = VtoR(DtoT(:,4),:)-RofCm;
    
    c1(1,1) = r_well_minor;
    c1(1,2) = r_well_major*cosd(dipang);

    c2(1,1) = r_frac_minor;
    c2(1,2) = r_frac_major*cosd(dipang);
    
end

function dots = r_dotter(interval,lambda,num)
    
    x1 = interval(1);
    x2 = interval(2);
    
    coeff = 1/lambda+1;
    
    dots = subdotter(x1,x2,coeff);
    
    switch nargin
        case 3
            n = length(dots);
            if n<num
                N = num-n;
                dots = [dots;zeros(N,1)];
                for i = n:num-1
                    dots(i+1,1) = dots(i,1)*coeff;
                end
                dots = x1+(dots-x1)*(x2-x1)/(dots(end)-x1);
            end
    end
    
end

function dots = subdotter(x1,x2,coeff)
    
    dots(1,1) = x1;

    count = 1;
    
    while dots(end)<x2
        dots(count+1,1) = dots(count,1)*coeff;
        count = count+1;
    end
    
    dots = x1+(dots-x1)*(x2-x1)/(dots(end)-x1);
    
end

function dots = t_dotter(interval,lambda)
    
    [a,b] = size(interval);
    
    if a == 1
        interval = interval';
        N = b;
    elseif b == 1
        N = a;
    elseif a ~= 1 || b ~= 1
        error('bad input')
    end
    
    int2 = [interval(2:N);interval(1)];
    darc = floor((int2-interval)*lambda+1);
    darc = darc(1:N-1)-2;
    nofd = sum(darc)+N-1;
    
    dots = zeros(nofd,1);
    
    for i = 1:N-1
        x1 = interval(i);
        x2 = interval(i+1);
        y1 = i*(darc(i)+1)-darc(i);
        y2 = i*(darc(i)+1);
        qd = linspace(x1,x2,darc(i)+2)';
        qd(end) = [];
        dots(y1:y2) = qd;
    end
    
end