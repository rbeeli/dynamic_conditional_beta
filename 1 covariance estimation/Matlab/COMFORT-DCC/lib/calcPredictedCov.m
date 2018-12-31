function [predictedCov] = calcPredictedCov(COMFORTparams)
    % Based on paper:
    % COMFORT: A common market factor non-Gaussian returns model 
    % Paolella, Polak (2015)
    % Journal of Econometrics
    
    lambda = COMFORTparams.GIGparNext(1);
    chi = COMFORTparams.GIGparNext(2);
    psi = COMFORTparams.GIGparNext(3);
    gamma = COMFORTparams.gam;
    
    % Equation (43): E[G] (alpha=1)
    [E_G] = GIGmoment(1, lambda, chi, psi);
    
    % Equation (43): E[G^2] (alpha=2)
    [E_G2] = GIGmoment(2, lambda, chi, psi);
    
    % Var(G) = E[G^2] - (E[G])^2
    Var_G = E_G2 - E_G^2;
    
    % Equation (10): Cov[Y] = E[G]*H + Var(G)*(gamma*gamma')
    predictedCov = E_G * COMFORTparams.Hnext + Var_G * (gamma * gamma');
end