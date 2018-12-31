function [pout,Vout]=einschrk(pin,bound,Vin)
% [pout,Vout]=einschrk(pin,bound,Vin)
% if Vin specified, then pout is untransformed, otherwise pout is transformed
% M. Paolella, 1997

welche=bound.which;
if all(welche==0) % no bounds!
  pout=pin; 
  if nargin==3, Vout=Vin; end
  return
end

lo=bound.lo; hi=bound.hi;
if nargin < 3
  trans=sqrt((hi-pin) ./ (pin-lo));
  pout=(1-welche).* pin + welche .* trans;
  Vout=[];
else
  trans=(hi+lo.*pin.^2) ./ (1+pin.^2);
  pout=(1-welche).* pin + welche .* trans;
  % now adjust the standard errors
  trans=2*pin.*(lo-hi) ./ (1+pin.^2).^2;
  d=(1-welche) + welche .* trans; % either unity or delta method.
  J=diag(d);
  Vout = J * Vin * J;
end
