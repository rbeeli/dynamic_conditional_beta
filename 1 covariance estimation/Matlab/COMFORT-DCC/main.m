fprintf(2, 'MAIN> Started at %s \n', datestr(datetime(), 'dd.mm.yyyy HH:MM:ss')); 

if ~exist('workerId', 'var')
    error('Input variable `workerId` not defined.');
end

fprintf(2, 'MAIN> Worker-ID: %s \n', workerId);

if exist('isShellMode', 'var')
    fprintf(2, 'MAIN> Starting program in Shell mode... \n');
    
    % load stock excess returns data
    returns = readtable('sp100_stock_ex_returns_demeaned_scaled.csv', 'ReadVariableNames',true);
    fprintf(2, 'MAIN> Returns data file read. \n');
else
    fprintf(2, 'MAIN> Starting program in IDE mode... \n');
    
    % load stock excess returns data
	returns = readtable('../../../0 data/CRSP/sp100_stock_ex_returns_demeaned_scaled.csv', 'ReadVariableNames',true);
    fprintf(2, 'MAIN> Returns data file read. \n');
end

% import COMFORT codes
addpath(genpath('lib'));

% estimation range
windowSize = 1000;
from = find(returns.date == '2013-12-31') + 1;
to = size(returns, 1);
steps = to - from + 1;
memoryless = true;

% extract data
range = (from - windowSize + 1):to;
dates = returns{range, 'date'};
marketData = returns{range, 'market'};
stocksData = returns(range, 3:size(returns, 2));
dateFrom = returns{from, 'date'};
dateTo = returns{to, 'date'};

% free memory
clear returns;

% get stocks assigned to this worker
worklist = readtable('worklist.csv', 'Delimiter', ';');
worklist = worklist(strcmp(worklist.Var2, workerId), 'Var1');

% estimate covariance of stock and market
for row=1:size(worklist, 1)
    sStart = tic;

    % returns series of stock
    stockName = strcat('x', int2str(worklist{row, 1}));
    stockIdx = find(strcmp(stockName, stocksData.Properties.VariableNames'));
    stockData = stocksData{:, stockIdx};
    
    % find ranges of non NaN values
    [ranges] = runRanges(~isnan(stockData'), true);

    % estimate covariance of every run range of stock
    noRanges = size(ranges, 1);
    for i=1:noRanges
        fprintf(2, 'MAIN> Estimating stock %s range %i/%i... \n', stockName, i, noRanges);

        % range of non NaN values
        from = ranges(i, 1);
        to = ranges(i, 2);
        rangeValues = from:to;

        if length(rangeValues) < windowSize
            fprintf(2, 'MAIN> Range %i to %i skipped (smaller than window size %i) \n', from, to, windowSize);
            continue;
        end

        % create output file to block other workers of calculating the same range
        outfile = sprintf('out/covs_%s_%s_%s_range_%i_%i.csv', stockName, dateFrom, dateTo, from, to);
    
        if exist(outfile, 'file') == 2
            fprintf(2, 'MAIN> Range %i to %i skipped (output file exists already) \n', from, to);
            continue;
        end
        
        % create empty output file
        fclose(fopen(outfile, 'w'));
        
        fprintf(2, 'MAIN> Output file `%s` created. \n', outfile);
        fprintf(2, 'MAIN> Range %i to %i \n', from, to);

        % create [Tx2] matrix with market and stock returns for bivariate estimation
        COMFORTdata = [marketData(rangeValues, :) stockData(rangeValues, :)];

        % estimate bivariate COMFORT model
        Nassets = size(COMFORTdata, 2);
        COMFORTtype = setType(Nassets, windowSize);
        [GARCH, GIG, CC, COMFORTparams] = COMFORTestimation(windowSize, COMFORTdata, COMFORTtype, [], memoryless);

        predictedCovs = cellfun(@(x) calcPredictedCov(x), COMFORTparams, 'UniformOutput', false);

        wndDates = dates(from + windowSize - 1:to);
        wndVarMarket = cellfun(@(x) x(1,1), predictedCovs);
        wndVarStock = cellfun(@(x) x(2,2), predictedCovs);
        wndCovStock = cellfun(@(x) x(1,2), predictedCovs);

        % write to CSV file
        outputTbl = table(wndDates, wndVarMarket, wndVarStock, wndCovStock);
        outputTbl.Properties.VariableNames = { 'date' 'var_market' 'var_stock' 'cov' };
        writetable(outputTbl, outfile, 'Delimiter', ';');
    end

    sEnd = toc(sStart);
    sEndStr = datestr(datenum(0,0,0,0,0,sEnd), 'HH:MM:SS');
    
    fprintf(2, 'MAIN> Stock %s estimated in %s \n', stockName, sEndStr);
    fprintf(2, 'MAIN> ----------------------------------------- \n');
end

fprintf(2, 'MAIN> Finished at %s \n', datestr(datetime(), 'dd.mm.yyyy HH:MM:ss')); 

















