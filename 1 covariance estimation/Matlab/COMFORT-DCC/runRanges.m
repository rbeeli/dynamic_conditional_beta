% Returns a [Tx2] matrix with run ranges, whereas
% every row consists of the start and end indices.
% The passed runs vector needs to consist of 0s and 1s.
function [ranges] = runRanges(runs, ignoreZeroes)

changes = diff(runs) ~= 0;

starts = [1 find(changes)+1];
ends = [find(changes) length(runs)];

ranges = [starts; ends]';

% remove ranges of zero runs
if ignoreZeroes
    if runs(1) == 0
        ranges(1:2:end, :) = [];
    else
        ranges(2:2:end, :) = [];
    end
end

end