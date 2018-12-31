library(readr)
library(stringr)
library(stargazer)


log <- read_file('log_COMFORT-DCC.txt')
matches <- str_match_all(log, 'estimated in ([0-9]+) min ([0-9]+) sec')[[1]]

mins <- as.numeric(matches[, 2])
secs <- as.numeric(matches[, 3])

total_secs <- data.frame(runtime=secs + mins * 60)
stargazer(total_secs, type='text')
stargazer(total_secs)




log <- read_file('log_DCC1step_sp100.txt')
matches <- str_match_all(log, 'Elapsed time is ([0-9]+)\\.[0-9]+ seconds')[[1]]

secs <- as.numeric(matches[, 2])

total_secs <- data.frame(runtime=secs <- secs / 98) # per stock


stargazer(total_secs, type='text')
stargazer(total_secs)
