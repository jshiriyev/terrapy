function Green = greenfunc(observer,source,tau,res)

% ---------------------------------------------------------------------- %
%   
%   Ref: Thambynayagam 2011 The Diffusion Handbook.
%   The solutions for a rectangular source, page 376
%   
%   The following solution is for the rectangular source with the
%   coordinates [x0,y01,0],[x0,y02,0],[x0,y01,height],[x0,y02,height]
%
%   !!! There is a typo in the book; integrate the solution for point
%   source on page 370 over the rectangular region !!!
%
%   Green function is structured as [observers]x[sources]x[time_steps]
%
% ---------------------------------------------------------------------- %
    
    Tx1 = elliptictheta((observer(:,1)-source.p1(:,1)')/(2*res.xlength),...
          (res.xdiffusivity*tau)/(res.xlength^2),1);
    Tx2 = elliptictheta((observer(:,1)+source.p1(:,1)')/(2*res.xlength),...
          (res.xdiffusivity*tau)/(res.xlength^2),1);
    
    Ty1 = elliptictheta((observer(:,2)-source.p1(:,2)')/(2*res.ylength),...
          (res.ydiffusivity*tau)/(res.ylength^2),2);
    Ty2 = elliptictheta((observer(:,2)+source.p1(:,2)')/(2*res.ylength),...
          (res.ydiffusivity*tau)/(res.ylength^2),2);
    Ty3 = elliptictheta((observer(:,2)-source.p2(:,2)')/(2*res.ylength),...
          (res.ydiffusivity*tau)/(res.ylength^2),2);
    Ty4 = elliptictheta((observer(:,2)+source.p2(:,2)')/(2*res.ylength),...
          (res.ydiffusivity*tau)/(res.ylength^2),2);
    
    Tz3 = elliptictheta((observer(:,3)-res.zlength)/(2*res.zlength),...
          (res.zdiffusivity*tau)/(res.zlength^2),2);
    Tz4 = elliptictheta((observer(:,3)+res.zlength)/(2*res.zlength),...
          (res.zdiffusivity*tau)/(res.zlength^2),2);
    
    Green = (Tx1+Tx2).*(Ty1-Ty2-Ty3+Ty4).*(-Tz3+Tz4);
    
end