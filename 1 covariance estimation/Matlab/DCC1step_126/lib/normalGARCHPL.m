function [param,stderr,loglik,resmat]=normalGARCHPL(ret,initvec,profile,B)
% normal-GARCH(1,1) estimation of a mean-zero process
% INPUT
% ret is the T X 1 zero-mean time series.
%   or T X d matrix of returns. If d>1, the log liks are pooled!
%   This could be used as a shrinkage target. 
%
% initvec is starting values of the 3 GARCH parameters,
%   pass [] to use default.
%
% Call with profile = 0 (default) for usual normal GARCH estimation.
%   Call with profile>=1 to do profile likelihood in c_0, with
%   the size of the grid for c_0 equal to *profile* parameter.
%   Then, loglik is a vector, and resmat a matrix of the 3 GARCH coef
%
% B is number of bootstrap replications to determine the std error
%   of the parameters. Pass B=0 (default) to use approximation from Hessian.
%   Current set up: Only use if d=1 and profile>0
% 
% To set the exponent of the GARCH equation, manually do so below
%   via parameter delta. Default is 2.
%
% Example. If ret is the vector of percentage log returns, use:
%   [param,stderr,loglik,resmat] = normalGARCHPL(ret,[],10);
%    param, stderr
%    % optionally can look at: [loglik,resmat]
%  Use [param,stderr,loglik,resmat] = normalGARCHPL(ret,[],10,200);
%    to get better std errors via the (parametric) bootstrap.
global y bound delta;
delta=2; y=ret; [T,d]=size(y);
if nargin<4 || isempty(B), B=0; end
if d==1, B=0; end % could easily relax this later...
UseBootstrap=B>0;
if nargin<2, initvec=[]; end
if or(nargin<3,isempty(profile)), profile=0; end
fmintol=1e-5; maxit=200;
opt = optimset('LargeScale','off','display','off', 'Maxiter',...
    maxit,'TolFun',fmintol,'TolX',fmintol);
if profile>0
  if isempty(initvec), initvec=[0.02 0.9]; end
  lb=1e-5; ub=1-lb;
  %            c_1  d_1
  bound.lo=    [lb   0 ];
  bound.hi=    [ub   ub ];
  bound.which= [1    1 ];
  clen=profile; % 100 is what I usually use
  resmat=zeros(clen,3);
  loglik=zeros(clen,1); best=-Inf;
  cvec=linspace(0,1.2*max(var(y)),clen);
  for i=1:clen
    c=cvec(i);
    [pout,fval]=fminunc(@(param) proflik(param,c),einschrk(initvec,bound),opt);
    hess=eye(length(pout)); V=pinv(hess)/(T*d);
    [parami,~]=einschrk(pout,bound,V);
    loglik(i)=-fval*(T*d);
    resmat(i,1)=c; resmat(i,2)=parami(1); resmat(i,3)=parami(2);
    if loglik(i)>best, best=loglik(i); param=[c parami]; end
  end
  thebestinitvec=param;
  [param,stderrHESS] = normalGARCHPL(y,thebestinitvec,0);
  if any(abs(imag(stderrHESS))>0), stderrHESS=NaN(1,3); end
  if nargout>1, stderr=stderrHESS; end
  if nargout>1 && UseBootstrap
    Pmat=NaN(B,3); qq=param(1:2); pp=param(3);
    for b=1:B
      YY=simg(T,qq,pp,delta); Pmat(b,:) = normalGARCHPL(YY,param,0);
    end
    stderr=std(Pmat);
  end
else  % no use of profile (and also not bootstrap)
  if nargout>=4, resmat=[]; end
  if isempty(initvec), initvec=[0.02 0.02 0.9]; end
  lb=1e-5; ub=1-lb;
  if initvec(1)<=lb; initvec(1)=0.001; end
  if initvec(2)<=lb; initvec(2)=0.001; end
  if initvec(2)>=ub; initvec(2)=0.99; end
  if initvec(3)<=lb; initvec(3)=0.001; end
  if initvec(3)>=ub; initvec(3)=0.99; end
  %             c_0       c_1   d_1
  bound.lo=    [lb         lb   0 ];
  bound.hi=    [1.2*max(var(y)) ub   ub  ];
  bound.which= [1          1    1  ];
  [pout,fval,~,~,~,hess]=fminunc(@(param) like2005(param),einschrk(initvec,bound),opt);
  V=pinv(hess)/(T*d); [param,V]=einschrk(pout,bound,V); 
  stderr=sqrt(diag(V))'; loglik=-fval*(T*d);
end

function loglik=proflik(param,c)
global y bound zvec sigvec
theparam = einschrk(real(param),bound,999);
param=[c theparam]; d=size(y,2); partloglik=zeros(1,d);
logK=log(sqrt(2*pi));
for i=1:d
  [zvec,sigvec]=ungarch(y(:,i),param);
  ll=-0.5*(zvec.^2)-logK-log(sigvec); partloglik(i)=-mean(ll);
end
loglik=mean(partloglik);

function loglik=like2005(param)
global y bound zvec sigvec
param = einschrk(real(param),bound,999);
d=size(y,2); partloglik=zeros(1,d); logK=log(sqrt(2*pi));
for i=1:d
  [zvec,sigvec]=ungarch(y(:,i),param);
  ll =-0.5*(zvec.^2)-logK-log(sigvec); partloglik(i)=-mean(ll);
end
loglik=mean(partloglik);

function y=simg(nobs,qq,pp,delta)
% Input 
%   qq is the ARCH component, pp is GARCH part, i.e.,
%   qq= [omega alpha]; pp = [beta] 
%   delta is the exponent in the GARCH equation, default of 2.
% Output y is a Gaussian garch(1,1) series
if nargin<6, delta=2; end
warm=500; use=nobs+warm;
s = rng; rng('shuffle'); z=randn(use+1,1); % get the z vector
rng(s);
h=zeros(use+1,1); e=zeros(use+1,1); omega=qq(1); qv=qq(2); pv=pp;
h(1)=omega; e(1)=h(1)*z(1);
for i=2:use+1
    h(i)=omega+ qv*( abs(e(i-1)) )^delta + pv*h(i-1);
    e(i)=(  (h(i))^(1/delta) ) * z(i);
end
e=e(2:end); y=e(warm+1:use);







