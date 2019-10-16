rng shuffle

addpath(genpath('lib')); % import DCC-GARCH estimation and prediction functions

isEuler = false;

% load returns data
if (isfile('sp500_stock_ex_returns_demeaned_scaled.csv'))
    isEuler = true;
    returns = readtable('sp500_stock_ex_returns_demeaned_scaled.csv', 'ReadVariableNames',true);
else
    returns = readtable('../../../0 data/CRSP/sp500_stock_ex_returns_demeaned_scaled.csv', 'ReadVariableNames',true);
end

% extract data
dates = returns{:, 1};
marketData = returns{:, 2};
stockData = returns(:, 3:end);

windowSize = 1000;
subwindowSize = 1;

if (isEuler)
    from = find(returns.date == '2003-12-26');
    to = find(returns.date == '2013-12-31');
else
    from = find(returns.date == '1996-01-02');
    to = find(returns.date == '2003-12-24');
end

steps = to - from + 1;
profile = 10;

increments = from:to;
for i=increments(randperm(length(increments)))
    tic;

    date = dates(i);
    
    fprintf('step %i of %i (%s) - ', i - from + 1, steps, date); 
    
    % output directories
    outDir1 = sprintf('out/%s_pending', date);
    outDir2 = sprintf('out/%s', date);
  
    % delete _pending folder if it exists already
    if 7 == exist(outDir1, 'dir')
        rmdir(outDir1, 's');
    end
  
    % skip if final output directory already exists
    if 7 == exist(outDir2, 'dir')
        % already computed - ignore this step
        fprintf('ignoring %s - already computed \n', date);

        if 7 == exist(outDir1, 'dir')
            rmdir(outDir1, 's');
        end
        
        continue;
    end
  
    % create `pending` output directory
    [status,msg] = mkdir(outDir1);
    if status ~= 1
        fprintf('ERROR: Failed to create directory %s with message: %s \n', outDir1, msg);
        continue;
    end
    
    % extract window data
    windowStocksData = stockData((i - windowSize + 1):i, :);
    windowMarketData = marketData((i - windowSize + 1):i, :);
    
    % determine which columns have enough data (no NaNs)
    cols = arrayfun(@(x) x == size(windowStocksData, 1), sum(~isnan(windowStocksData{:,:}), 1));

    % create matrix with columns which have no NaNs
    colIdxs = transpose(1:size(windowStocksData, 2)) .* transpose(cols);
    colIdxs = colIdxs(colIdxs > 0);
    windowStocksData = windowStocksData(:, colIdxs);
    stockCols = size(windowStocksData, 2);

    % fit DCC-GARCH in sub-samples
    fitSteps = ceil(stockCols / subwindowSize);
    
    varsMarket = array2table(nan(2, fitSteps), 'RowNames', cellstr({"estimated" "1stepahead"}));
    covsStocks = array2table(nan(2, stockCols), 'RowNames', cellstr({"estimated" "1stepahead"}));
    covsStocks.Properties.VariableNames = windowStocksData.Properties.VariableNames;
    
    for stepIdx = 1:fitSteps
      stepFrom = (stepIdx - 1) * subwindowSize + 1;
      stepTo = min(stepFrom + subwindowSize - 1, stockCols);
      stepData = [windowMarketData windowStocksData{:, stepFrom:stepTo}];

      [~, SigmaT] = DCC1step(stepData, profile);

      % variance of univariate GARCH (sigma_m^2)
      varsMarket{2, stepIdx} = SigmaT(1, 1);
      
      % DCC conditional covariance (sigma_im^2)
      covsStocks{2, stepFrom:stepTo} = SigmaT(1, 2:end);
      
      if mod(stepIdx, 10) == 0
          fprintf('    %i of %i steps \n', stepIdx, fitSteps);
      end
    end

    % make sure all columns were fitted
    assert(sum(isnan(varsMarket{2, :})) == 0)
    assert(sum(isnan(covsStocks{2, :})) == 0)

    % save covariance matrix to output directory
    writetable(varsMarket, sprintf('%s/vars.market.csv', outDir1), 'Delimiter', ';', 'WriteRowNames', true);
    writetable(covsStocks, sprintf('%s/covs.stocks.csv', outDir1), 'Delimiter', ';', 'WriteRowNames', true);

    % rename directory (remove `_pending`)
    if 7 == exist(outDir2, 'dir')
        rmdir(outDir2, 's');
    end
    
    % remove `_pending` suffix from output directory
    movefile(outDir1, outDir2);
  
    toc
end























