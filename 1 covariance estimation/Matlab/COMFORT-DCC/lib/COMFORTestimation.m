function [GARCH, GIG, CC, COMFORTparams] = COMFORTestimation(winsize,use,type,tickers,memoryless)

type.model.tickers=tickers;

[TT,NN]=size(use);
if type.model.IID==1
    parmatrices=[]; stddevamtrix=[]; sigmat_next=[]; zvecmatrices=[]; GIGpars=[];
    GIGpars_next=[]; meanllvec=[]; EGmomnetsmat=[];  Corrmats=[];  GIG_SV_pars=[];
    COMFORTparams=cell(TT-winsize+1,1);
else
    [parmatrices stddevmatrix sigmat_next zvecmatrices GIGpars GIGpars_next meanllvec EGmomnetsmat ...
        Corrmats GIG_SV_pars]=initparams(TT,NN,winsize,type);
    if strcmpi(type.model.Corrmodel,'VC') || strcmpi(type.model.Corrmodel,'DCC') || strcmpi(type.model.Corrmodel,'cDCC') || strcmpi(type.model.Corrmodel,'M-DCC')...
            || strcmpi(type.model.Corrmodel,'newDCCmds') || strcmpi(type.model.Corrmodel,'newDCCcorrY') ...
            || strcmpi(type.model.Corrmodel,'newDCCcorrYdiff')  || strcmpi(type.model.Corrmodel,'newDCCFisherMDS')...
            || strcmpi(type.model.Corrmodel,'newDCC_G_gamma_gamma')
        CCpars_a=zeros(TT-winsize+1,1);
        CCpars_b=zeros(TT-winsize+1,1);
        CCpars_S=zeros(TT-winsize+1,NN,NN);
        CCpars_Gamma0=zeros(TT-winsize+1,NN,NN);
    end
    
    COMFORTparams=cell(TT-winsize+1,1);
end
window_id=0;

steps = TT-winsize+1;

for tt=1:(TT-winsize+1)
    tStart = tic;
    
    
    window_id=window_id+1;
    
    
    data=use(tt:winsize+tt-1,:);
    
    
    if tt<TT-winsize+1
        type.dataOut=use(winsize+tt,:);
    else
        type.dataOut=use(winsize+tt-1,:);
    end
    if memoryless || tt==1
        maxit=30;
        initGARCH=[];
        type.window=tt;
        if strcmpi(type.model.distribution,'Stud_t') || strcmpi(type.model.distribution,'NCT')...
                || strcmpi(type.model.distribution,'SStud_t') || strcmpi(type.model.distribution,'SNCT')
            EGmoments=1.01*ones(winsize,length(type.model.whichEGmoments)); %Has to be EG>1 to get the df. v > 0
            EGmoments(:,type.model.whichEGmoments==-1)=1./EGmoments(:,type.model.whichEGmoments==1);
            EGvec=EGmoments;
        elseif ~strcmpi(type.model.distribution,'Normal')
            EGmoments=ones(winsize,length(type.model.whichEGmoments));
            EGvec=EGmoments;
        else
            EGmoments=[];
            EGvec=[];
        end
        if type.model.IID==0 && (strcmpi(type.model.Corrmodel,'VC') || strcmpi(type.model.Corrmodel,'DCC') || strcmpi(type.model.Corrmodel,'cDCC')|| strcmpi(type.model.Corrmodel,'M-DCC')...
                || strcmpi(type.model.Corrmodel,'newDCCmds') || strcmpi(type.model.Corrmodel,'newDCCcorrY') ...
                || strcmpi(type.model.Corrmodel,'newDCCcorrYdiff')  || strcmpi(type.model.Corrmodel,'newDCCFisherMDS') || strcmpi(type.model.Corrmodel,'newDCC_G_gamma_gamma'))
            initCC.a=type.model.initCC.a;
            initCC.b=type.model.initCC.b;
            initCC.S=type.model.initCC.S;
            initCC.Gamma0=type.model.initCC.Gamma0;
        else
            initCC.a=[];initCC.b=[];initCC.S=[];initCC.Gamma0=[];
        end
        MDS=zeros(winsize,1);
    else
        maxit=10;
        initGARCH=parmatrix;
        type.window=tt;
        if ~strcmpi(type.model.distribution,'Normal')
            type.GIGparam  = [EG_r(2:winsize,:) ; mean(EG_r)];
            type.SVparinit = EG_par;
            EGmoments=[EGmoments(2:end,:) ; mean(EGmoments)];
            EGvec=[EGvec(2:end,:); mean(EGvec)];
            MDS = [MDS(2:end,:) ; mean(MDS)];
        else
            EGmoments      = [];
            EGvec=[];
            type.GIGparam  = [];
            type.SVparinit = [];
        end
        
        if ~type.model.IID==1 &&(strcmpi(type.model.Corrmodel,'VC') || strcmpi(type.model.Corrmodel,'DCC') || strcmpi(type.model.Corrmodel,'cDCC') || strcmpi(type.model.Corrmodel,'M-DCC') ...
                || strcmpi(type.model.Corrmodel,'newDCCmds') || strcmpi(type.model.Corrmodel,'newDCCcorrY') ...
                || strcmpi(type.model.Corrmodel,'newDCCcorrYdiff')  || strcmpi(type.model.Corrmodel,'newDCCFisherMDS') || strcmpi(type.model.Corrmodel,'newDCC_G_gamma_gamma'))
            initCC=CCpars;
        else
            initCC.a=[];initCC.b=[];initCC.S=[];initCC.Gamma0=[];
        end
    end
    
    if type.model.FREECOMFORT==0 %&& ~strcmpi(type.model.distribution,'Normal')
        [parmatrix, stddev, Corrmat, sigvec_next, zvecmatrix, EGmoments, EG_r, EG_par,EGvec, CCpars, MDS,COMFORTp] = ...
            PLaplaceEM(data,EGmoments,EGvec,MDS,initGARCH,initCC,maxit,type);
        COMFORTparams{tt}=COMFORTp;
    elseif type.model.FREECOMFORT==1
        if tt==1
            COMFORTparams{tt} = FREECOMFORTestimation(data,type);
        else
            COMFORTparams{tt} = FREECOMFORTestimation(data,type,COMFORTparams{tt-1});
        end
        %{
        COMFORTparams.mu         = paramGARCH(:,1);
        COMFORTparams.gam        = paramGARCH(:,end);
        COMFORTparams.Hnext      = diag(Signext)*CorrNext*diag(Signext);
        COMFORTparams.GIGparNext = EG_r(end,:);
        COMFORTparams.GARCHpar   = paramGARCH(:,2:end-1);
        COMFORTparams.GIGpar     = EG_r;
        COMFORTparams.Snext      = Signext;
        COMFORTparams.S          = stddev;
        COMFORTparams.Gamma      = CorrNext;
        COMFORTparams.logL       = mean(ll);
        COMFORTparams.EG         = EGmoments(:,type.model.whichEGmoments==1);
        COMFORTparams.EGinv      = EGmoments(:,type.model.whichEGmoments==-1);
        COMFORTparams.IIDsignal  = [];
        %}
        parmatrix=[COMFORTparams{tt}.mu',repmat(COMFORTparams{tt}.GARCHpar,NN,1),COMFORTparams{tt}.gam'];
        stddev=COMFORTparams{tt}.S'; Corrmat=COMFORTparams{tt}.Gamma; sigvec_next=COMFORTparams{tt}.Snext; zvecmatrix=nan(winsize,NN);
        EGmoments=[COMFORTparams{tt}.EG,COMFORTparams{tt}.EGinv]; EG_r=COMFORTparams{tt}.GIGpar;
        EG_par=[];EGvec=[]; CCpars=[]; MDS=[];
    elseif 1==2
        disp('Normal CCC based on matlab build-in functions')
        [parmatrix, stddev, Corrmat, sigvec_next, zvecmatrix, EGmoments, EG_r, EG_par,EGvec, CCpars, MDS] = ...
            GaussianCCModel(data,initGARCH,initCC,type);
    else
        disp('The FREECOMFORT can have only 0 or 1 value')
    end
    if size(stddev,1)==winsize
        stddev=stddev';
    end
    parmatrices(tt,:,:)=parmatrix(:,1:size(parmatrices,3));
    stddevmatrix(tt,:,:)=stddev;
    sigmat_next(tt,:)=sigvec_next;
    zvecmatrices(tt,:,:)=zvecmatrix;
    if ~strcmpi(type.model.distribution,'Normal')
        GIGpars(tt,:,:)=EG_r(1:winsize,:);
        GIGpars_next(tt,:)=EG_r(end,:);
        EGmomnetsmat(tt,:,:) = EGmoments;
    end
    if ~type.model.IID==1 && type.model.mixGARCH_SV==1
        GIG_SV_pars(tt,:,:)=EG_par;
    end
    meanllvec(tt)=1;
    if ~type.model.IID==1 && (strcmpi(type.model.Corrmodel,'VC') || strcmpi(type.model.Corrmodel,'DCC') || strcmpi(type.model.Corrmodel,'cDCC') || strcmpi(type.model.Corrmodel,'M-DCC') ...
            || strcmpi(type.model.Corrmodel,'newDCCmds') || strcmpi(type.model.Corrmodel,'newDCCcorrY') ...
            || strcmpi(type.model.Corrmodel,'newDCCcorrYdiff')  || strcmpi(type.model.Corrmodel,'newDCCFisherMDS') || strcmpi(type.model.Corrmodel,'newDCC_G_gamma_gamma'))
        CCpars_a(tt)=CCpars.a;
        CCpars_b(tt)=CCpars.b;
        CCpars_S(tt,:,:)=CCpars.S;
        CCpars_Gamma0(tt,:,:)=CCpars.Gamma0;
    elseif ~type.model.IID == 1 && strcmpi(type.model.Corrmodel,'PCA')
        Corrmats(tt,:,:)=Corrmat;
    else
        Corrmats(tt,:,:)=Corrmat;
    end
    
    tEnd = toc(tStart);
    fprintf(2, 'COMFORT> Step %i of %i (%.2f%%) estimated in %d min %.0f sec \n', tt, steps, tt / steps * 100, floor(tEnd/60), rem(tEnd,60));
    
%     disp(['% tt = ',num2str(tt)]);
%     if mod(tt,100)==0
%         temp=clock;
%         name=MARCMARSfilename(type);
%         save('BACKUP')
%         disp('%%%%%%%%%%BACKUP saved%%%%%%%%%%%%%%%%%')
%         disp(['%MODEL : ',name])
%         disp(['%DATE : ',num2str(temp(3)),'/',num2str(temp(2)),'/',num2str(temp(1)),' ',num2str(temp(4)),':',num2str(temp(5))])
%         disp(['% tt = ',num2str(tt)]);
%         disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
%     end
end
% name = MARCMARSfilename(type);



GARCH.parmatrices=parmatrices;
GARCH.stddevmatrix=stddevmatrix;
GARCH.sigmat_next=sigmat_next;
GARCH.zvecmatrices=zvecmatrices;
if ~strcmpi(type.model.distribution,'Normal')
    GIG.GIGpars=GIGpars;
    GIG.GIGpars_next=GIGpars_next;
    if ~type.model.IID==1 && type.model.mixGARCH_SV==1
        GIG.GIG_SV_pars=GIG_SV_pars;
    end
    GIG.EGmomnetsmat=EGmomnetsmat;
else
    GIG=[];
    
end
if ~type.model.IID==1 && (strcmpi(type.model.Corrmodel,'VC') || strcmpi(type.model.Corrmodel,'DCC') || strcmpi(type.model.Corrmodel,'cDCC') || strcmpi(type.model.Corrmodel,'M-DCC') ...
        || strcmpi(type.model.Corrmodel,'newDCCmds') || strcmpi(type.model.Corrmodel,'newDCCcorrY') ...
        || strcmpi(type.model.Corrmodel,'newDCCcorrYdiff')  || strcmpi(type.model.Corrmodel,'newDCCFisherMDS') || strcmpi(type.model.Corrmodel,'newDCC_G_gamma_gamma'))
    CC.CCpars_a=CCpars_a;
    CC.CCpars_b=CCpars_b;
    CC.CCpars_S=CCpars_S;
    CC.CCpars_Gamma0=CCpars_Gamma0;
else
    CC.Corrmats=Corrmats;
end

% try
%     save(name,'-v7.3')
%     temp=clock;
%     disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
%     disp(['MODEL : ',name])
%     disp(['%DATE : ',num2str(temp(3)),'/',num2str(temp(2)),'/',num2str(temp(1)),' ',num2str(temp(4)),':',num2str(temp(5))])
%     disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
% catch
%     N2=randi(999999);
%     temp=clock;
%     save(['tempname',num2str(N2),'_see_name_value'],'-v7.3');
%     name=MARCMARSfilename(type);
%     disp(['%DATE : ',num2str(temp(3)),'/',num2str(temp(2)),'/',num2str(temp(1)),' ',num2str(temp(4)),':',num2str(temp(5))])
%     disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
%     disp([' temporal name MODEL : ',name])
%     disp('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
% end

end
