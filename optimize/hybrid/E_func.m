function energy = E_func(m)
    
    nop = size(m,1);        % number of parameters
    
    p = 1;
    
    for i = 1:nop
        s = sign(sinc(m(i)))*power(abs(sinc(m(i))),0.25);
        p = p*s;
    end
    
    energy = power(1-p,2);
    
end