function [intervals] = splitIntoIntervals(from, toInc, intervalSize)

numberOfIntervals = ceil((toInc - from + 1) / intervalSize);
intervals = NaN(numberOfIntervals, 2);

for i=1:numberOfIntervals
    intervals(i, 1) = from + (i - 1) * intervalSize;
    intervals(i, 2) = min(from + i * intervalSize - 1, toInc);
end

end
