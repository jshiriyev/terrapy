function energy = neighbor(m,G_E,m_min,m_max)
    
    nop = size(m,1);        % number of parameters
    noE = size(G_E,1);      % number of previous forward runs
    
    mb = G_E(:,2:1+nop)';
    distance = zeros(noE,1);
    
    si = m_max-m_min;
    
    Cm = diag(1./power(si,2));  % dimensionalize parameter space

    for i = 1:noE
        distance(i) = sqrt((m-mb(:,i))'*Cm*(m-mb(:,i)));
    end
    
    [~,n] = min(distance);
    
    energy = G_E(n,1);
    
end