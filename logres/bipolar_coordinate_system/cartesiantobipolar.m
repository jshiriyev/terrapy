function [tau,sigma] = cartesiantobipolar(x,y,a)

    % The function calculates corresponding tau and sigma values of
    % Bipolar system for the given x and y values of Cartesian system
    
    % Given the cartesian coordinates {x,y}
    % x changes in between -Inf and Inf
    % y changes in between -Inf and Inf
    
    % the function calculates the bipolar coordinates {tau,sigma}
    % tau changes in between -Inf and Inf
    % sigma changes in between -pi and pi
    
    % The foci are taken to lie at (-a,0)&(0,a) in the Cartesian system.
    
    switch nargin
        case 2
            a = 1;
    end
    
    if or(~isvector(x),~isvector(y))
        error('sigma and tau needs to be scalar or vector');
    elseif and(~isscalar(x),~isscalar(y))
        if length(x)~=length(y)
            error('sigma and tau needs to be the same size vectors');
        elseif size(x,1)~=size(y,1)
            y = y';
        end
    end

    tau = log(((x+a).^2+y.^2)./((x-a).^2+y.^2))/2;
    
    upper = 2*a*y;
    lower = x.^2+y.^2-a^2;
    
    sigma = atan(upper./lower);
    
    sigma(and(lower<0,y>0)) = sigma(and(lower<0,y>0))+pi;
    sigma(and(lower<0,y<0)) = sigma(and(lower<0,y<0))-pi;
    
end