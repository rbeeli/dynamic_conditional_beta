function [CD, Vout] =ab2CD(ab,Vin)
%C=a/(a+b);D=a+b so 0<a<1, 0<b<1 and 0<a+b<1 can be achived by 0<C<1 and 0<D<1
[n1, n2]=size(ab);
if n1>2 || n2>2, warning('ab2CD programed wrong'), end %#ok<*WNTAG>
CD(1) = ab(1) ./ (ab(1) + ab(2)); CD(2) = ab(1) + ab(2);
if nargout>1  % now adjust the standard errors
  J=[1./(ab(1) + ab(2)) - ab(1)./(ab(1) + ab(2)).^2, -ab(1)/(ab(1) + ab(2)).^2 ; 1,1];
  Vout=J * Vin * J;
end