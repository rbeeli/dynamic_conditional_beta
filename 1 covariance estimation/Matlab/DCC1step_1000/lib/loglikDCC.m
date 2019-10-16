function loglik=loglikDCC(param,resid,S,Q,bound)

if ~isempty(bound), param=einschrk(real(param),bound,999); end

a = param(1) * param(2);
b = param(2) - param(1) * param(2);

[T,d] = size(resid); ll=zeros(T,1);
for t=1:T
  if t>1
   Q = DCCengine(resid(t-1,:),a,b,S,Q);
   Corrmat = diag(sqrt(diag(Q)).^(-1))*Q*diag(sqrt(diag(Q)).^(-1));
   Corrmat = SymPDcorrmatrix(Corrmat);
  else
   Corrmat=S; Q=S; Corrmat=SymPDcorrmatrix(Corrmat);
  end
  
  detGamma=det(Corrmat); Gammainv=Corrmat\eye(d);
  ll(t) = - 0.5 *(log(abs(detGamma)) + resid(t,:) * Gammainv * resid(t,:)');
end
loglik= - sum(ll);