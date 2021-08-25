function [x,y] = bipolartocartesian(tau,sigma,a)

    % The function calculates corresponding x and y values of Cartesian
    % system for the given tau and sigma values of Bipolar system
    
    % Given the bipolar coordinates {tau,sigma}
    % tau changes in between -Inf and Inf
    % sigma changes in between -pi and pi
    
    % the function calculates the cartesian coordinates {x,y}
    % x changes in between -Inf and Inf
    % y changes in between -Inf and Inf
    
    % The foci are taken to lie at (-a,0)&(0,a) in the Cartesian system.
    
    switch nargin
        case 2
            a = 1;
    end
    
    if or(~isvector(tau),~isvector(sigma))
        error('sigma and tau needs to be scalar or vector');
    elseif and(~isscalar(tau),~isscalar(sigma))
        if length(tau)~=length(sigma)
            error('sigma and tau needs to be the same size vectors');
        elseif size(tau,1)~=size(sigma,1)
            sigma = sigma';
        end
    end

    x = (a*sinh(tau))./(cosh(tau)-cos(sigma));
    y = (a*sin(sigma))./(cosh(tau)-cos(sigma));

%     figure(1)
% 
%     plot(-a,0,'k.','MarkerSize',10); hold on
%     plot(a,0,'k.','MarkerSize',10); hold on
% 
%     plot(linspace(-8,8,100),zeros(100,1),'k'); hold on
%     plot(zeros(100,1),linspace(-8,8,100),'k'); hold on
% 
%     xlabel('x-axis');
%     ylabel('y-axis');
% 
%     xlim([-4,4])
%     ylim([-4,4])
% 
%     plot(x,y,'r.'); hold on
% 
%     pbaspect([1,1,1])

end