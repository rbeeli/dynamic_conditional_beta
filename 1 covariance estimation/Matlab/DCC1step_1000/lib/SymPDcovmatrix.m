function A=SymPDcovmatrix(A,tol)
[n,m]=size(A);
if ~(n==m), error('Input matrix has to be a square matrix '), end
if nargin<2, tol=1e-04; end
A=(A+A')/2;
try
  [V,D]=eig(A); seig=diag(D); bad=find(seig<tol);
catch %#ok<CTCH>
  %bp=1; 
end
if ~isempty(bad), seig(bad)=tol; D=diag(seig); A=V*D*V';end