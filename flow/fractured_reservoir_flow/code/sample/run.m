clear;
close all;
clc;

srcpath = 'C:\Users\js68897\repos\greensfrac';  % source code directory

addpath(srcpath)

setup.structures(pwd);

green.plane(res,frac,time);

main.solver(res,frac,well,time,Gplane);

vtkwrite(frac,time,sol);