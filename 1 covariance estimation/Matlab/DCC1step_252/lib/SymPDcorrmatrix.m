function A=SymPDcorrmatrix(A,tol)
[n,m]=size(A);
if ~(n==m), error('Input matrix has to be a square matrix '), end
if nargin<2, tol=1e-04; end
if sum(any(abs((A)-eye(n))>=1+(1e-02)))>0, warning('This is not a correlation matrix'); end
numCol = find( any( abs((A)-eye(n)) >= ( 1-(1e-16) ) ) );
numRow = find( any( abs((A)-eye(n))' >= ( 1-(1e-16) ) ) );
if ~isempty(numCol) || ~isempty(numRow)
  A(numRow,numCol) = sign(A(numRow,numCol)) * ( 1-(1e-16) ); 
  % the off-diagonal entries in (-1 1)
  warning('Some of the correlations were corrected');
end
A = A - diag(diag(A)) + eye(n,m); % ones on the diagonal
A = ( A + A' )/2;                 % symmetric matrix
[V,D] = eig(A); seig = diag(D); bad = find(seig < tol); % PD
if ~isempty(bad), seig(bad) = tol; D = diag(seig); A = V * D * V';end