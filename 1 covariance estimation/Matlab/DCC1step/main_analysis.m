e = matlab.desktop.editor.getActive; 
cd(fileparts(e.Filename));           % Set working directory to current file dir
clear;                               % Reset workspace variables
close all;                           % Close all figures
addpath(genpath('DCC'));             % Import DCC-GARCH functions for 1-steap ahead prediction

% date, market, stock 1, stock 2, stock 3, ...
returns = readtable('../0 data/CRSP/log_excess_returns.csv', 'ReadVariableNames',true);

ncol = size(returns, 2);

% Scale log-returns to avoid numerical (precision) issues (important!)
scaledReturns = returns;
scaledReturns{:, 2:ncol} = returns{:, 2:ncol} * 1000;

windowSize = 252;
from = 12000;
to = 12001; % Cannot be very last record of dataset to be able to extract date of forecast (=following trading day)
steps = to - from + 1;

covTable = array2table(NaN(steps, ncol));
covTable.Properties.VariableNames = scaledReturns.Properties.VariableNames;
covTable.date = repmat(NaT, steps, 1);
covTable.date.Format = 'yyyy-MM-dd';

% realCovTable = array2table(NaN(steps, ncol));
% realCovTable.Properties.VariableNames = scaledReturns.Properties.VariableNames;
% realCovTable.date = repmat(NaT, steps, 1);
% realCovTable.date.Format = 'yyyy-MM-dd';

templateRow = covTable(1, :);

tic;

for i=1:steps
%parfor i=1:steps
  tic;
  
  fprintf('step %i of %i - ', i, steps); 
  
  idx = from + i - 1;
  
  if true
      % extract window data (+1 row to make sure forecast day has data too)
      windowData = scaledReturns{(idx - windowSize + 1):(idx + 1), 2:size(scaledReturns, 2)};

      % determine which columns have enough data (no NaNs)
      cols = arrayfun(@(x) x == size(windowData, 1), sum(~isnan(windowData), 1));

      % create matrix with columns which have no NaNs
      colIdxs = transpose(1:size(windowData, 2)) .* transpose(cols);
      colIdxs = colIdxs(colIdxs > 0);
      fitData = windowData(:, colIdxs);
      fitData = fitData(1:(size(fitData, 1) - 1), :); % remove last row again

      % fit DCC-GARCH model batch-wise with 32 columns each (highest measured throughput)
      Sigma = NaN(1, size(fitData, 2));

      fitStepSize = 32;
      fitSteps = ceil(size(fitData, 2) / fitStepSize);
      for stepIdx = 1:fitSteps
          stepFrom = (stepIdx - 1) * fitStepSize + 1;
          stepTo = min(stepFrom + fitStepSize - 1, size(fitData, 2));

          % do DCC-GARCH fit
          [~, SigmaT] = DCC1step(fitData(:, stepFrom:stepTo));

          Sigma(1, stepFrom:stepTo) = SigmaT(1, :);
      end

      % make sure all columns were fitted
      assert(sum(isnan(Sigma(1,:))) == 0)

      % expand fit values to fit into full covariance table
      covRow = templateRow;
      covRow{1, 1} = scaledReturns{idx+1, 1}; % forecast date
      covRow{1, 1 + colIdxs} = Sigma(1, :); % predicted covariance
      covTable(i, :) = covRow;
  end
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
%   if true
%       % window for real covariance
%       windowData2 = scaledReturns{(idx - windowSize + 2):(idx + 1), 2:size(scaledReturns, 2)};
% 
%       % determine which columns have enough data (no NaNs)
%       cols2 = arrayfun(@(x) x == windowSize, sum(~isnan(windowData2), 1));
% 
%       % create matrix with columns which have no NaNs
%       colIdxs2 = transpose(1:size(windowData2, 2)) .* transpose(cols2);
%       colIdxs2 = colIdxs2(colIdxs2 > 0);
%       fitData2 = windowData2(:, colIdxs2);
% 
%       Sigma2 = cov(fitData2);
% 
%       % make sure all columns were fitted
%       assert(sum(isnan(Sigma2(1,:))) == 0)
% 
%       % expand fit values to fit into full covariance table
%       covRow2 = templateRow;
%       covRow2{1, 1} = scaledReturns{idx+1, 1}; % last date
%       covRow2{1, 1 + colIdxs2} = Sigma2(1, :); % calculated covariance
%       realCovTable(i, :) = covRow2;
%   end
  
  toc
end

fprintf('Total ');
toc

writetable(covTable, sprintf("predicted_covariance_%i_%i.csv", from, to), 'Delimiter', ';');
% writetable(realCovTable, "real_covariance_12000-12500.csv", 'Delimiter', ';');


% plot(covTable{:, 5}); hold on;
% plot(realCovTable{:, 5}); hold off;
% legend('predicted', 'real');
% 
% corrcoef(covTable{:, 5}, realCovTable{:, 5})







