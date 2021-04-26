clear
close all
clc

indata = load(strcat(pwd,'\Library\indata_2.txt'));

nog = indata(1);        % number of generations
nom = indata(2);        % number of models in each generation
nop = indata(3);        % number of parameters in each model

m_min = zeros(nop,1);
m_max = zeros(nop,1);

for i = 1:nop
    m_min(i) = indata(3+i);
    m_max(i) = indata(3+nop+i);
end

R2 = vfsa(m_min,m_max,nog,nom,nop);

[avgE2,lowE2] = process(R2(:,1),nog,nom);

RR = reshape(R2(:,1)',nom,nog);

RR = transpose(sort(RR,1));

figure(1)

plot(linspace(1,nog,nog),RR(:,1),'ko','MarkerSize',5); hold on
plot(linspace(1,nog,nog),RR(:,2),'ko','MarkerSize',5); hold on
plot(linspace(1,nog,nog),RR(:,3),'ko','MarkerSize',5); hold on
plot(linspace(1,nog,nog),RR(:,4),'ko','MarkerSize',5); hold on
plot(linspace(1,nog,nog),RR(:,5),'ko','MarkerSize',5); hold on
plot(linspace(1,nog,nog),lowE2,'r.','MarkerSize',12);

set(gca,'FontSize',14)

xlabel('Number of Iteration','FontSize',14)
ylabel('Error','FontSize',14)

xlim([0,nog])
ylim([0,3])
% grid on