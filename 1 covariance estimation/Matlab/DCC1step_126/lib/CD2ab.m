function [ab,Vout] = CD2ab(CD,Vin)
%a=C*D;b=D-CD so 0<a<1, 0<b<1 and 0<a+b<1 can be achived by 0<C<1 and 0<D<1
[n1, n2]=size(CD);
if n1>2 || n2>2, warning('CD2ab programmed wrong'), end
ab(1) = CD(1).*CD(2); ab(2) = CD(2)-CD(1).*CD(2);
if nargout>1  % now adjust the standard errors
  J=[CD(2), CD(1); -CD(2), (1-CD(1))]; Vout=J * Vin *J;
end