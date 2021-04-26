function [T] = temperature(i,nog)
    
    To = 1;     % initial temperature, popular specification is 1
    Tf = 1e-2;   % final temperature, popular specifications are [0.01 0.1]
    
    T = To*power(Tf/To,(i-1)/(nog-1));
    
%     T = power((nog-i+1)/nog,20);
    
end
    

