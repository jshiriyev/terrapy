function [k,snp1,de,dep]=computeForce(defpos,dNu,sn,disp)
    
    [B1,detJ1]=computeBandJ(defpos,dNu.p1);
    [snp1.p1,de.p1,dep.p1]=computeStress(sn.p1,disp,B1);
    
    [B2,detJ2]=computeBandJ(defpos,dNu.p2);
    [snp1.p2,de.p2,dep.p2]=computeStress(sn.p2,disp,B2);
    
    [B3,detJ3]=computeBandJ(defpos,dNu.p3);
    [snp1.p3,de.p3,dep.p3]=computeStress(sn.p3,disp,B3);
    
    [B4,detJ4]=computeBandJ(defpos,dNu.p4);
    [snp1.p4,de.p4,dep.p4]=computeStress(sn.p4,disp,B4);
    
    k=B1'*snp1.p1*detJ1+B2'*snp1.p2*detJ2+B3'*snp1.p3*detJ3+B4'*snp1.p4*detJ4;
    
end

