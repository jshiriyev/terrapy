function newton_rhapson

    clear
    close all
    clc

    x = [2;3];
    
    tol = 1e-2;
    
    for i = 1:100
        
        residual = resCalculate(x);
%         jacobian = analyticalJacobian(x);
        jacobian = numericalJacobian(x);
        
        if norm(residual)<tol
            disp(x);
            disp(i);
            break
        end
        
        delta_x = jacobian\-residual;
        
        x = x+delta_x;
        
    end

end

function residual = resCalculate(x)

    residual(1,1) = x(1)^2+x(1)*x(2)-10;
    residual(2,1) = x(2)+3*x(1)*x(2)^2-57;

end

function jacobian = analyticalJacobian(x)

    jacobian(1,1) = 2*x(1)+x(2);
    jacobian(2,1) = 3*x(2)^2;
    jacobian(1,2) = x(1);
    jacobian(2,2) = 1+6*x(1)*x(2);

end

function jacobian = numericalJacobian(x)
    
    N = length(x);
    
    jacobian = zeros(N,N);
    
    for i = 1:N
        xp1 = x;
        xm1 = x;
        xp1(i) = x(i)+x(i)/100;
        xm1(i) = x(i)-x(i)/100;
        yp1 = resCalculate(xp1);
        ym1 = resCalculate(xm1);
        jacobian(:,i) = (yp1-ym1)*50/x(i);
    end
    
end