function [EXalpha EXalphaprime]=GIGmoment(alpha,lambda,chi,psi)
%function computes alpha moment of the GIG(lambda,chi,psi) r.v. and the
%derivative of alpha moment w.r.t. alpha 
%USES: derivest.m to get EXalphaprime

if lambda>0
   if chi>=0 && psi>0
       paramsOK=1;
   else
       paramsOK=0;
   end
elseif lambda==0
   if chi>0 && psi>0
       paramsOK=1;
   else
       paramsOK=0;
   end
elseif lambda<0
   if chi>0 && psi>=0
       paramsOK=1;
   else
       paramsOK=0;
   end
end
if paramsOK==1
    if abs(chi)<1e-12 && abs(psi-2)<1e-12 %MALap limiting case
        if alpha==1
                EXalpha=lambda;
        elseif alpha==2
                EXalpha=lambda.^2+lambda;
        elseif alpha==-1
            if lambda>1
                EXalpha=1./(lambda-1);
            else
               EXalpha=realmax; %moment does not exist.
            end
        else
            error('the moment is not supported for this distribution MALap')
        end
    elseif abs(chi+2*lambda)<1e-12 &&  abs(psi)<1e-12 %limiting case: Stud_t or NCT
        if alpha==1 %v/2 /(v/2)
           if lambda<-1 && chi>0
              EXalpha=-0.5*chi./(lambda+1);
           elseif lambda>-1 && chi<0 %never the case b/c chi>=0 for any GIG r.v.
              EXalpha=-0.5*chi./(lambda+1);
           else
              EXalpha=NaN; 
           end
        elseif alpha==2
            if lambda < -2
                a = -lambda; b = 0.5*chi;
                %EXalpha=(b.^2 + (a-2).* b.^2 )./( (a-1).^(2) .* (a-2) );
                EXalpha=(b.^2)./( (a-1) .* (a-2) ); %E(X^2)
                %EXalpha=(b.^2)./( (a-1).^2 .* (a-2) ); %Var(X) = E(X^2)-(EX)^2
            else
                EXalpha=realmax;
            end
        elseif alpha==-1
            EXalpha = -0.5 .* lambda .* chi;
        elseif alpha==0.5
            if lambda < -0.5
            a = -lambda; b = 0.5*chi;
            EXalpha = b.^(0.5)./(a-0.5);
            else
               EXalpha=realmax;
            end 
        else
             error('the moment is not supported for this distribution Stud_t')
        end
    elseif abs(lambda+0.5)<1e-12 %special case NIG!
        if alpha==1
            EXalpha=sqrt(chi./psi);
        elseif alpha==-1
            EXalpha=sqrt(psi./chi)+1./chi;
        elseif alpha==2
            EXalpha=(chi./psi).*(1+1./sqrt(chi.*psi));
        else
            error('the moment is not supported for this distribution NIG')
        end
    elseif abs(psi)<1e-21
        EXalpha = 0.5.^alpha * chi.^alpha * gamma(-lambda-alpha)/gamma(-lambda);
    else
        %EXalpha=(chi/psi).^(alpha/2).*(quickbesselk(lambda+alpha,sqrt(chi*psi))/(quickbesselk(lambda,sqrt(chi*psi))));
        EXalpha=(chi/psi).^(alpha/2).*exp(mylogbesselkApprox(lambda+alpha,sqrt(chi*psi))-mylogbesselkApprox(lambda,sqrt(chi*psi)));
        nanID=isnan(EXalpha); infID=abs(EXalpha)==Inf;

        if sum(nanID)>0 
            %EXalpha(logical(nanID))=(chi(nanID)/psi(nanID)).^(alpha(nanID)/2).*besselratio(lambda(nanID),sqrt(chi(nanID)*psi(nanID)),3,alpha(nanID));
            EXalpha(logical(nanID))=(chi(nanID)/psi(nanID)).^(alpha(nanID)/2).*besselk_ratio(lambda(nanID),sqrt(chi(nanID)*psi(nanID)),3,alpha(nanID));
            nanID=isnan(EXalpha);
            if sum(nanID)>0
               EXalpha(logical(nanID))=mean(EXalpha(logical(1-nanID)));
            end
        end

        if sum(infID)>0 
            %EXalpha(logical(infID))=(chi(infID)/psi(infID)).^(alpha(infID)/2).*besselratio(lambda(infID),sqrt(chi(infID)*psi(infID)),3,alpha(infID));
            EXalpha(logical(infID))=(chi(infID)/psi(infID)).^(alpha(infID)/2).*besselk_ratio(lambda(infID),sqrt(chi(infID)*psi(infID)),3,alpha(infID));
            infID=abs(EXalpha)==Inf;
            if sum(infID)>0
               EXalpha(logical(infID))=mean(EXalpha(logical(1-infID)));
            end
        end
    end
else
   error('Wrong parametrization of the GIG r.v.')
end
if nargout>1
   [der,errest,finaldelta] = derivest(@(alpha1)GIGmoment(alpha1,lambda,chi,psi),alpha);
   tol=1e-12;
    if errest>tol
        [derf,errestf,finaldelta] = derivest(@(alpha1)GIGmoment(alpha1,lambda,chi,psi),alpha,'Style','forward');
    end
    if errest>tol
        [derb,errestb,finaldelta] = derivest(@(alpha1)GIGmoment(alpha1,lambda,chi,psi),alpha,'Style','backward');
    end
    if errest>tol
        if errestb<errestf
             der=derb; errest=errestb;
        else
             der=derf; errest=errestf;
        end
    end
    if errest>tol
        [derU,errestU,finaldeltaU] = derivest(@(alpha1)GIGmoment(alpha1,lambda,chi,psi),alpha+0.1);
        [derL,errestL,finaldeltaL] = derivest(@(alpha1)GIGmoment(alpha1,lambda,chi,psi),alpha-0.1);
        derUL=(derU+derL)./2;
        if sum(abs(derUL-der)>0.001)>0
            breakpoint=1;
        end
    end
   EXalphaprime(1)=der;
   EXalphaprime(2)=errest;
   EXalphaprime(3)=finaldelta;
  
end

function[logK] = mylogbesselkApprox(v,z)
    if abs(z) < 1e-24 && abs(v) < 1e-24
       logK =  log(-log(abs(z)));  %see eq. 9.5 in Paolella (2007)
    elseif abs(z) < 1e-24 && abs(v) >= 1e-24
       logK = log(gamma(v))  + (abs(v)-1) * log(2) + ( -abs(v) ) * log(abs(z)) ; %see eq. 9.6 in Paolella (2007)
    else
        logK=log(quickbesselk(v,z));
        if (~isreal(logK) || sum(isnan(logK))>0 || sum(isinf(logK))>0 ), %try Watson expansion
            logK=0.5*log(pi)-0.5*log(2*z)-z + log(Evz(v,z,[],[]));
        end
    end
