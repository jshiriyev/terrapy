clear; close all; clc

addpath('C:\Users\js68897\repos\greensfrac')

res = resControl(pwd,'SI');
frac = fracControl(pwd,'SI');
well = wellControl(pwd,'SI');
time = numControl(pwd,'SI');

Gsol = green.plane(frac.center,frac,time.tau-time.deltaTime/2,res);

sol = solver(res,frac,well,time,Gsol);

vtkwrite(res,frac,well,time,sol);