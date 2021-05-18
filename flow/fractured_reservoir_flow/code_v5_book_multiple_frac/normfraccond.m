clear all
close all
clc

sigma_hormin = 5345;
pressure_avg = linspace(500,4200);

sigma_closure = sigma_hormin-pressure_avg;

lognorm_fraccond = -0.0004*sigma_closure+0.2191;

norm_fraccond = power(10,lognorm_fraccond);

plot(pressure_avg,norm_fraccond)

ylim([0,1]);
xlim([pressure_avg(1),pressure_avg(end)]);

grid on