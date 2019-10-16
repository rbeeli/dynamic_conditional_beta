function Q=DCCengine(resid_t,a,b,S,Qin)
Mmat = resid_t'*resid_t; Mmat=SymPDcovmatrix(Mmat);
Q=(1-a-b) * S + a * Mmat + b * Qin;
