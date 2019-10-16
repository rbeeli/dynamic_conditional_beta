function [mu, Sigma] = DCC1step(data, profile)
% INPUT: data is a T X d matrix of asset log returns
% Output: 
%   mu: the mean of returns, 1 x d vector
%   sigma: one step ahead prediction of the asset covariance matrix
%     computed using the Gaussian DCC-GARCH model

%profile = 10;
[T,d] = size(data); 

mu = mean(data);
data_demean = data - repmat(mu,T,1);
garchP = zeros(3,d);
sigmamat = zeros(T,d);
GARCHresid = zeros(T,d);

for n = 1:d
  garchP(:,n) = normalGARCHPL(data_demean(:,n),[],profile);
  [GARCHresid(:,n),sigmamat(:,n)] = ungarch(data_demean(:,n), garchP(:,n));
end

%DCC: Q_t=S*(1-a-b)+a*eps_{t-1}eps'_{t-1} +b*Q_{t-1}
initvec = [];
S = [];
Gamma0 = []; 
[a,b,Smat,~,Gamma0] = DCCestimate(GARCHresid,initvec,S,Gamma0);
Rmat = zeros(d,d,T);
S = Smat;
Q = Gamma0;

for t=1:T
    if t>1
      Q = DCCengine(GARCHresid(t-1,:),a,b,S,Q);
      Corrmat = diag(sqrt(diag(Q)).^(-1))*Q*diag(sqrt(diag(Q)).^(-1));
      Corrmat = SymPDcorrmatrix(Corrmat);
    else
      Corrmat = S; Q=S; Corrmat=SymPDcorrmatrix(Corrmat);
    end
    
    Rmat(:,:,t)=Corrmat;
end

Q_f = (1-a-b)*Rmat(:,:,1)+a*GARCHresid(end,:)'*GARCHresid(end,:)+b*Rmat(:,:,end);
R_f = diag(diag(Q_f).^(-0.5))*Q_f*diag(diag(Q_f).^(-0.5));
H_f = diag(sigmamat(end,:))*Rmat(:,:,end)*diag(sigmamat(end,:));

% Recombine GARCH11 and Correlation matrix forecast
D_f = diag(sqrt(garchP(1,:)+garchP(2,:).*GARCHresid(end,:).^2+garchP(3,:).*diag(H_f)')); 
Sigma = D_f * R_f * D_f;
Sigma = SymPDcovmatrix(Sigma);
