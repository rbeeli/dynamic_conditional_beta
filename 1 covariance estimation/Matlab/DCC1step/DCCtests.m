e = matlab.desktop.editor.getActive; 
cd(fileparts(e.Filename));           % Set working directory to current file dir
clear;                               % Reset workspace variables
close all;                           % Close all figures
addpath(genpath('DCC'));             % Import DCC-GARCH functions for 1-steap ahead prediction

data = readtable('DCCtest_data.csv', 'ReadVariableNames',true);

data = data{:, :};

% % scale returns to avoid convergence issures
% data = data * 1000;



% complete
profile = 10;
[~, Sigma, H_f, D_f, sigmamat] = DCC1step(data, profile);
covariancesFull = Sigma(:, 1);




% pair-wise 
covariancesPairwise = zeros(size(data, 2), 1);
pairSteps = size(data, 2);
for i=2:pairSteps
    [~, SigmaPair, Hf_pair] = DCC1step(data(:, [1, i]));
    covariancesPairwise(1, 1) = SigmaPair(1, 1);
    covariancesPairwise(i, 1) = SigmaPair(1, 2);
    
    fprintf('pair=%i of %i \n', i, pairSteps)
end


% batch-wise with 24 columns each
covariancesBatch = nan(size(data, 2), 1);

stockCols = size(data, 2) - 1;
fitStepSize = 24;
fitSteps = ceil(stockCols / fitStepSize);
for stepIdx = 1:fitSteps
  fprintf('batch=%i of %i \n', stepIdx, fitSteps)
  
  stepFrom = (stepIdx - 1) * fitStepSize + 1;
  stepTo = min(stepFrom + fitStepSize - 1, stockCols);

  stepData = [data(:, 1) data(:, (stepFrom + 1):(stepTo + 1))];
  
  [~, SigmaT] = DCC1step(stepData);

  covariancesBatch(1, 1) = SigmaT(1, 1);
  covariancesBatch((stepFrom + 1):(stepTo + 1), 1) = SigmaT(1, 2:size(stepData, 2));
end

plot(covariancesFull); hold on;
plot(covariancesBatch); hold on;
plot(covariancesPairwise); hold off;
legend('full', 'batch', 'pairwise');



% % find optimal batch size
% covTemp = zeros(size(data, 2), 1);
% stockCols = size(data, 2) - 1;
% for batchSize=[2 4 8 16 24 32]
%     fprintf('batch size %i ', batchSize);
%     
%     tic;
%     
%     fitSteps = ceil(stockCols / batchSize);
%     for stepIdx = 1:fitSteps
%       fprintf('batch=%i of %i \n', stepIdx, fitSteps)
% 
%       stepFrom = (stepIdx - 1) * batchSize + 1;
%       stepTo = min(stepFrom + batchSize - 1, stockCols - 1);
% 
%       stepData = [data(:, 1) data(:, (stepFrom + 1):(stepTo + 1))];
% 
%       [~, SigmaT] = DCC1step(stepData);
% 
%       covTemp(1, 1) = SigmaT(1, 1);
%       covTemp((stepFrom + 1):(stepTo + 1), 1) = SigmaT(1, 2:size(stepData, 2));
%     end
%     
%     toc
% end




% retrieval of GARCH-variance
rng default; % For reproducibility

Mdl1 = garch('Constant',2.0, 'GARCH',0.95, 'ARCH',0.02)
Mdl2 = garch('Constant',0.1, 'GARCH',0.8, 'ARCH',0.1)
[Vn1,Yn1] = simulate(Mdl1, 1000, 'NumPaths',2);
[Vn2,Yn2] = simulate(Mdl2, 1000, 'NumPaths',2);

plot(Vn1(:,1)); hold on;
plot(Vn2(:,1)); hold off;

mean(Vn1(:,1))
mean(Vn2(:,1))
var(Vn1(:,1))

% for profile=[0 2 10 20 50 100]
%     fprintf("profile %i \n", profile);
%     [~, Sigma] = DCC1step([Vn1 Vn2], profile);
%     
%     disp(Sigma)
% end

[~, Sigma, D_f] = DCC1step([Vn1 Vn2]);
    
Sigma
D_f

% https://core.ac.uk/download/pdf/52106361.pdf
% physical page 26
diag(Sigma)
diag(D_f).^2

% variance of market (sigma_i^2) should come directly from GARCH model, not
% from DCC forecast!























