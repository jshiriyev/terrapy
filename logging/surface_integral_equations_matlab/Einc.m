function Ec = Einc(k1,R1,R2)
    
    % Given two coordinates: observation (R1) and source (R2) 
    % Orientation of source is on the z direction
    % Es is the electrical field in spherical coordinates
    % Ec is the electrical field in cartesian coordinates [Ex Ey Ez]
    
    global NofD
    
    r(:,1) = R1(:,1)-R2(1,1);
    r(:,2) = R1(:,2)-R2(1,2);
    r(:,3) = R1(:,3)-R2(1,3);
    
    R = sqrt(sum(r.*r,2));
    
    teta = acosd(r(:,3)./R);      % 0<teta<180
    phi = atand(r(:,2)./r(:,1));   % 0<phi<360
    
    v1 = r(:,1)<0;
    v2 = r(:,2)<0;
    
    phi = phi+v1*180;
    phi = phi+(~v1.*v2)*360;
    
    Es = k1*sind(teta)./(4*pi*R).*(1+1./(1j*k1*R)).*exp(-1j*k1*R);
    Ec = [-sind(phi).*Es cosd(phi).*Es zeros(NofD,1)];
    
end