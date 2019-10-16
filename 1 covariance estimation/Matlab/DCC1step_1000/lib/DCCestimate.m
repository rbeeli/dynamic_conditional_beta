function [a,b,Smat,ll,Gamma0] = DCCestimate(resid,initvec,S,Gamma0)

%             C             D
bound.lo    = [0.001        0         ];
bound.hi    = [1-(1e-04)    1-(1e-04) ];
bound.which = [1            1         ];

if isempty(initvec), initvec=[0.05 0.93]; end
if isempty(S), S=corr(resid); end

S = SymPDcorrmatrix(S);

if isempty(Gamma0), Gamma0=S; end

Gamma0 = SymPDcorrmatrix(Gamma0);

opt = optimset('Display', 'off', 'TolFun', 1e-6, 'TolX', 1e-6, 'MaxFunEvals', 5000, 'LargeScale', 'Off');
[pout,~] = fminunc(@(param) loglikDCC(param,resid,S,Gamma0,bound), einschrk(ab2CD(initvec), bound), opt);

param = CD2ab(einschrk(real(pout), bound, 1));
a = param(1);
b = param(2);
Smat = S;

ll = -1 * loglikDCC(ab2CD(param), resid, S, Gamma0, []);
