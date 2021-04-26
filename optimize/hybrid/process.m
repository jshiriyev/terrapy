function [avgE,lowE] = process(R,nog,nom)
    
    Emg = reshape(R,nom,nog);
    
    avgE = zeros(nog,1);
    lowE = zeros(nog,1);
    
    for i = 1:nog
        avgE(i) = mean(Emg(:,i));
        lowE(i) = min(Emg(:,i));
    end
    
end
    

