function [COMFORTportfolio, GARCH, GIG, CC, COMFORTparams, name] = COMFORT_All(ret, winsize, tau, xi,type)
% ACTION:
%   Portfolio optimization over moving windows of (log percentage) returns data
%     based on the COMFORT/PSARM model of Paolella and Polak (2015, 2017)
%     and FREE-COMFORT model
%   It estimates COMFORT model with GARCH dynamics, CCC, VC, DCC, cDCC, and with or without the SV extension. All of these models for MALap Stud-t and NIG and symmetric versions of these distributions.
%   It also estiamtes FREE-COMFORT with GARCH-CCC and no SV.
%   Please see the function setType below to choose select the model to run.
%   Default model: COMFORT-MALap-CCC-GARCH-noSV (see setType function below for more options)

%   After estimating the parameters the function estimates two portfolios:
%   1. minimum ES portfolio (weights, VaR, and ES)
%   2. mean-ES portfolio for given level of expected portfolio returns.
%
% SAVE:
% The function saves the intermediate (every 100 rolling windows) and all
% the variables in the end of the run in a mat file.
%
% INPUTS:
% ret - returns matrix
% winsize - length of the rolling window used in estimation (default is 1000)
% tau - the desired expected annual return of the portfolio in mean-ES portfolio (one can use a whole vector to get the portfolio frontier)
% xi - the probability level associated with the ES and VaR of the portfolio (default xi=0.01 for 1% ES and VaR)
%
% OUTPUTS:
% w- minimum ES portfolio weights (matrix: number of rolling windows x number of assets)
% es - ES of the minimum ES portfolio (vector:   number of rolling windows x 1)
% var - VaR of the minimum ES portfolio (vector:  number of rolling windows x 1)
% wEw- mean-ES portfolio weights (matrix: number of rolling windows x number of assets x number of tau's)
% esEw - ES of the mean-ES portfolios (matrix:   number of rolling windows x number of tau's)
% varEw - VaR of the mean-ES portfolio (matrix:  number of rolling windows x number of tau's)
%
% Example
%{
T=1002;N=3;
N1=1;
mu = abs(randn(1,N))./sqrt(95);
gam = sign(-mu).*randn(1,N)./sqrt(100);
Mdl = garch('Constant',0.01,'GARCH',0.95,'ARCH',0.03);
[Vn1,Yn1] = simulate(Mdl,T,'NumPaths',1);
Vn2=mean(Vn1)+[randn(N-N1,1).^2]./10;
Yn2 = randn(T,N-N1).*repmat(Vn2',T,1);
Yn=[Yn1,Yn2];
G=gamrnd(1,1,[T,1]);
ret=repmat(mu,size(Yn,1),1)+repmat(gam,size(Yn,1),1).*repmat(G,1,N)+Yn.*repmat(sqrt(G),1,N);
%ret=repmat(mu,size(Yn,1),1)+Yn.*repmat(sqrt(G),1,N);


 
[w, es, var,wEw,esEw,varEw]=COMFORT_All(ret, 1000,[],[],[]);
%[w, es, var,wEw,esEw,varEw]=COMFORT_All([], 1000,[],[],[]);

%}


if isempty(winsize)
    winsize = 1000;
end

if isempty(tau)
    tau=0; % i.e., 0% annualy
end
if isempty(xi)
    xi=0.01;
end

dosave=1;

winstart=winsize;




if isempty(ret)
    if isempty(type)
        type = setType(30,winsize);
    end
    %file = 'DJcomponents2011';
    %file = 'DJcomponents2012';
    file = 'DJ30_returns_2001_01_02_2016_07_21.mat';
    type.data.filename=file;
    if strcmpi(file, 'DJcomponents2011')
        load returnsDJ30_2001_2011
        mdata=myRet;
        tickers=ticker;
        dates=myTime;
    elseif strcmpi(file, 'DJcomponents2012')
        load('djia_components_apr2013_crsp_07may2013.mat')
        mdata=returns;
        priceMat = prices;
        tickers = ticker;
        dates = pricesdates(2:end);
    elseif strcmpi(file,'DJ30_returns_2001_01_02_2016_07_21.mat')
        load('DJ30_returns_2001_01_02_2016_07_21.mat')
        mdata = returns;
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
else
    mdata = ret;
    tickers=[];
    if isempty(type)
        type = setType(size(mdata,2),winsize);
    end
    type.data.filename = 'UserData';
end

%%%%%%%%%%%%%%%%%%%%%%%%%%FILE NAME%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
name=MARCMARSfilename(type);
temp=clock;
disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
disp(['%MODEL : ',name])
disp(['%DATE : ',num2str(temp(3)),'/',num2str(temp(2)),'/',num2str(temp(1)),' ',num2str(temp(4)),':',num2str(temp(5))])
disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%mdata = mdata(1:1002,:);
[T,N]=size(mdata);
disp(['  WINDOW SIZE = ',num2str(winsize),';  NUMBER OF ASSETS = ',num2str(N),';'])
if 1==1
    [GARCH, GIG, CC, COMFORTparams] = COMFORTestimation(winsize,mdata,type,tickers); %All assets case
else
    MARCMARSCorrelations(type,mdata,winsize,h,winstart)
end

% 
% %convert daily tau into yearly
% tau_y = @(tau_d) 100*((1+tau_d/100).^252-1);
% 
% %convert yearly tau into daily
% tau_d = @(tau_y) 100*( (1 + tau_y/100).^(1/252) - 1);
% 
% tau_use = tau_d(tau);
% w00 = ones(N,1);
% w00 = w00./sum(w00);
% 
% w=zeros(T-winsize+1,N);
% es=zeros(T-winsize+1,1);
% var=zeros(T-winsize+1,1);
% 
% 
% wEw=zeros(T-winsize+1,N,length(tau_use));
% esEw=zeros(T-winsize+1,length(tau_use));
% varEw=zeros(T-winsize+1,length(tau_use));
% for t = 1 : (T-winsize+1)
%     if strcmpi(type.model.distribution,'Normal')
%         mu    = COMFORTparams{t}.mu;
%         %gam   = COMFORTparams{t}.gam;
%         GammaNext = COMFORTparams{t}.Gamma;
%         Snext = COMFORTparams{t}.Snext;
%         Sig   = diag(Snext)*GammaNext*diag(Snext); # forecasted Sigma
%         
%         [w(t,:), var(t)] = minvariance01(mu(:), Sig,3);
%         es(t) = w(t,:)*mu(:) - sqrt(w(t,:)*Sig*w(t,:)')*normpdf(norminv(xi))/(1-xi);
%         for it=1:length(tau_use)
%             [wEw(t,:,it), varEw(t,it)] = meanvariance01(mu(:), Sig, tau_use(it), 3);
%             esEw(t,it)=squeeze(wEw(t,:,it))*mu(:) - sqrt(squeeze(wEw(t,:,it))*Sig*squeeze(wEw(t,:,it))')*normpdf(norminv(xi))/(1-xi);
%         end
%     else
%         mu    = COMFORTparams{t}.mu;
%         gam   = COMFORTparams{t}.gam;
%         GammaNext = COMFORTparams{t}.Gamma;
%         Snext = COMFORTparams{t}.Snext;
%         Sig   = diag(Snext)*GammaNext*diag(Snext);
%         
%         lam   = COMFORTparams{t}.GIGparNext(1);
%         chi   = COMFORTparams{t}.GIGparNext(2);
%         psi   = COMFORTparams{t}.GIGparNext(3);
%         
%         [w(t,:), es(t), var(t)] = cvarlimghyp(xi,[], mu, gam, Sig, lam, chi, psi);
%         for it=1:length(tau_use)
%             [wEw(t,:,it), esEw(t,it), varEw(t,it)] = cvarlimghyp(xi,tau_use(it), mu, gam, Sig, lam, chi, psi);
%         end
%     end
%     disp(['Portfolio optimization for the rolling window ',num2str(t),' out of ',num2str(T-winsize+1), ', i.e., ',num2str(round(100*t/(T-winsize+1))),'% finished'])
% end
% COMFORTportfolio.w = w;
% COMFORTportfolio.es = es;
% COMFORTportfolio.var = var;
% COMFORTportfolio.wEw = wEw;
% COMFORTportfolio.esEw = esEw;
% COMFORTportfolio.varEw = varEw;
COMFORTportfolio = 0;


if dosave==1
    try
        save(name,'-v7.3')
        temp=clock;
        disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
        disp(['MODEL : ',name])
        disp(['%DATE : ',num2str(temp(3)),'/',num2str(temp(2)),'/',num2str(temp(1)),' ',num2str(temp(4)),':',num2str(temp(5))])
        disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
    catch
        N2=randi(999999);
        temp=clock;
        save(['tempname',num2str(N2),'_see_name_value'],'-v7.3');
        name=MARCMARSfilename(type);
        disp(['%DATE : ',num2str(temp(3)),'/',num2str(temp(2)),'/',num2str(temp(1)),' ',num2str(temp(4)),':',num2str(temp(5))])
        disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
        disp([' temporal name MODEL : ',name])
        disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
    end
end
end
