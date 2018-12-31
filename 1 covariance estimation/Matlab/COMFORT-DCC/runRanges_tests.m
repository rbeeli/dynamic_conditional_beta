disp('----------- testAllRuns -------------');

testAllRuns(isnan([NaN NaN NaN]));
testAllRuns(isnan([1 2 3]));
testAllRuns(isnan([NaN 1 2 3]));
testAllRuns(isnan([NaN NaN 1 2 3]));
testAllRuns(isnan([NaN 1 2 3 NaN]));
testAllRuns(isnan([NaN NaN 1 2 3 NaN NaN]));
testAllRuns(isnan([1 2 3 NaN NaN NaN 1 2 3]));

disp('----------- testOneRuns -------------');

testOneRuns(isnan([NaN NaN NaN]));
testOneRuns(isnan([1 2 3]));
testOneRuns(isnan([NaN 1 2 3]));
testOneRuns(isnan([NaN NaN 1 2 3]));
testOneRuns(isnan([NaN 1 2 3 NaN]));
testOneRuns(isnan([NaN NaN 1 2 3 NaN NaN]));
testOneRuns(isnan([1 2 3 NaN NaN NaN 1 2 3]));

    
function testAllRuns(vec)
    [ranges] = runRanges(vec, false);
    disp(vec);
    disp(ranges);
    disp('------------------------');
end

function testOneRuns(vec)
    [ranges] = runRanges(vec, true);
    disp(vec);
    disp(ranges);
    disp('------------------------');
end