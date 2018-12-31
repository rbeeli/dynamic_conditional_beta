function [type] = setType(Nassets,winsize)
% function which sets the type variable. The type variable controls which
% COMFORT model is estimated:
%type.model.
%.Corrmodel = {'CCC', 'Clust', '1stepRSDC', '2stepRSDC', 'VC', 'DCC','cDCC'}... old not checked: {'newDCCcorrY','newDCC_G_gamma_gamma','newDCCmds','newDCCcorrYdiff','newDCCFisherMDS'}
%.distribution = {'MGHyp', 'MALap', 'MLap', 'Normal', 'NIGPsi1', 'SNIGPsi1', 'Stud_t', 'SStud_t', 'NCT'} MLap or S-prefix is for the symmetric version of the distribution (elliptical model)
%.GARCHtype = {'GARCH', 'A-PARCH','GJR'}
%.mixGARCH_SV =  {1,0}
%.SV_ARp = {1,2,3,4,5} if .mixGARCH_SV==1 then .SV_ARp is the AR(p) of SV.
%.FREECOMFORT={0,1} % FREE-COMFORT stands for Fast Reduced Estimation
% COMFORT with all the GARCHes dynamics with the same parameters (see FREECOMFORTestimation function below for the detail descrpition in the comments).
% .FREECOMFORT = 0  the regular comfort
% .FREECOMFORT = 1  the free-comfort


% estimation using ECME method or an adhoc three step approach
%.estimation = {'EM','3step'}
% starting values for the estimation: last window estimates (use in a
% rolling window exercise, or adhoc three step estimates).
%.startingvalue = {'lastEstimate','3step'}


type.model.FREECOMFORT=0; % FREE-COMFORT model vs COMFORT model
type.model.IID=0; % conditional dynamics or IID (1 for IID model)
type.model.estimation='EM'; % 'EM' or '3step' ('3step' works only for COMFORT model)
type.model.mixGARCH_SV =0; % 1 mixGARCH-SV vs. 0 iid G model (FREE-COMFORT works only with mixGARCH_SV=0;
type.model.distribution = 'MALap';  % 'MALap', 'NIGPsi1', 'Stud_t', 'MLap', 'SNIGPsi1', 'SStud_t'
type.model.Corrmodel = 'DCC'; %(FREE-COMFORT works with CCC only); COMFORT: 'CCC', 'DCC', 'cDCC', 'VC'
type.model.GARCHtype = 'GARCH'; %'GARCH' 'GJR'
type.model.for=0;  % boolean variable to use or not for loops in the code.



[type] = LowerLevelParametersAndPrint(type,Nassets,winsize);
end
