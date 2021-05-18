function theta = elliptictheta(X,T,order)
    
% ---------------------------------------------------------------------- %
%   
%   Ref: Thambynayagam 2011 The Diffusion Handbook.
%   Elliptic theta function of the third kind, page 6 and 7
%
%   Two forms of the theta function and their integrals are given. Both
%   forms are valid over the whole range of time, but convergence is more
%   rapid in the specified regions of the argument exp(-pi^2*t).
%   
%   order = 1 -> elliptic theta function of third kind
%   order = 2 -> integral of elliptic theta function of third kind
%
%   N: truncation of summation
%
% ---------------------------------------------------------------------- %
    
    % There is a need to add convergence check for the following N
    
    N = 100;
    
    Nx = size(X,1);
    Ny = size(X,2);
    Nt = size(T,1);
    
    T = permute(T,[3 2 1]);
    
    theta = zeros(Nx,Ny,Nt);
    
    % There is an inconsistency between the paper (Zhou et al. 2014) and
    % the book (Thambynayagam). In the paper, the following condition is
    % opposite. In the code, we followed book.
    
    b1 = exp(-pi^2*T)>1/pi;	% to implement rapid convergence conditions
    b2 = ~b1;               % to implement rapid convergence conditions
    
    n1 = permute(1:N,[1 4 3 2]);
    n2 = permute(-N:N,[1 4 3 2]);
    
    if order == 1       % elliptic theta function of the third kind
        theta(:,:,b1) = 1+2*sum(exp(-n1.^2*pi^2.*T(b1)).*cos(2*n1*pi.*X),4);
        theta(:,:,b2) = 1./sqrt(pi*T(b2)).*sum(exp(-(X+n2).^2./T(b2)),4);
    elseif order == 2   % integral of elliptic theta fucntion of the third kind
        theta(:,:,b1) = X+1/pi*sum(exp(-n1.^2*pi^2.*T(b1))./n1.*sin(2*n1*pi.*X),4);
        theta(:,:,b2) = 1/2*sum(erf((X+n2)./sqrt(T(b2)))-erf(n2./sqrt(T(b2))),4);
    end
    
end