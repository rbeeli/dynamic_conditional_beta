function []=RunCOMFORT()
%Program to run the COMFORT estimation

% Y_t = mu + gamma * G_t + sqrt(G_t) * H_t^1/2 * Z_t
% (G_t | Y_1,...,Y_{t-1}) ~ GIG(lambda, chi, psi) (IID)
% Z_t ~ N(0,I_K)
% H_t = S_t * Gamma_t * S_t
% S_t=diag(s_{t,1},...,s_{t,K}); s_{t,k} ~ GARCH-type
tic


N=30; % number of assets
winsize = 1000; %size of the rolling window try DCC-GARCH with 1000 window size too

type = setType(N,winsize);
disp(['  WINDOW SIZE = ',num2str(winsize),';  NUMBER OF ASSETS = ',num2str(N),';'])


%%%%%%%%%%%%%%%%%%%%%%%%%%FILE NAME%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
name=MARCMARSfilename(type);
temp=clock;
disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
disp(['%MODEL : ',name])
disp(['%DATE : ',num2str(temp(3)),'/',num2str(temp(2)),'/',num2str(temp(1)),' ',num2str(temp(4)),':',num2str(temp(5))])
disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')

%%%%%%%%%%%%%%%%%%%%%%%%%%DATA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		load('DJ30_1990_2016_companies_with_complete_price_data_LOGRETMAT_CORRECTED.mat')
		mdata=100.*LOGRETMAT(:, 2:end); 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%RUN ESTIMATION%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if 1==1    
  [COMFORTportfolio, GARCH, GIG, CC, COMFORTparams, name] = COMFORT_All(mdata, winsize,[],[],type)
%  save COMFORTestimates
end