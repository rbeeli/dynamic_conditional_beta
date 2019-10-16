function [eout,sigvec]=ungarch(epsi,garchin)
global delta
int=garchin(1); qterms=garchin(2); pterms=garchin(3);
e=(abs(epsi)).^delta; 
lambda=( ( 2^(delta/2) ) / sqrt(pi) ) * gamma((delta+1)/2);

% sinit =  E[sigvec_0^delta], einit = E|e_0^delta| = lambda * sinit.
sinit=mean((abs(epsi)).^delta); einit=lambda*sinit;

% do the recursion in sigvec^delta
sigvec=zeros(length(e),1); sigvec(1)=int+qterms*einit+pterms*sinit;

for i=2:length(e), sigvec(i)=int + qterms*e(i-1) + pterms*sigvec(i-1); end
if any(sigvec<=0), error('hello'), end

sigvec=sigvec.^(1/delta);
eout=epsi./sigvec;
