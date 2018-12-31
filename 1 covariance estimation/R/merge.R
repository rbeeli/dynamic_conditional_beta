out1.path <- 'out'
out2.path <- 'out.euler/out'
merged.path <- 'out.merged'


# extract all computed dates
out1.dirs <- list.dirs(path=out1.path, full.names=F, recursive=F)
out2.dirs <- list.dirs(path=out2.path, full.names=F, recursive=F)

# ignore pending entries
out1.dirs <- Filter(function(dir) nchar(dir) == 10, out1.dirs)
out2.dirs <- Filter(function(dir) nchar(dir) == 10, out2.dirs)

# ignore empty folders
out1.dirs <- Filter(function(dir) length(list.files(paste0(out1.path, '/', dir), all.files=T, full.names=F, no..=T)) > 0, out1.dirs)
out2.dirs <- Filter(function(dir) length(list.files(paste0(out2.path, '/', dir), all.files=T, full.names=F, no..=T)) > 0, out2.dirs)
out1.dirs <- out1.dirs[!out1.dirs %in% out2.dirs] # no overlapping dates

# create output directory
if (dir.exists(merged.path))
  unlink(merged.path, recursive=T, force=T)
dir.create(merged.path, recursive=T, showWarnings=T)

# start merge process...
cat(length(out1.dirs), 'entries in', out1.path, '\n')
cat(length(out2.dirs), 'entries in', out2.path, '\n')
cat('Merging to', merged.path, '...\n')


cat('Processing folder', out1.path, '... \n')
for (i in 1:length(out1.dirs)) {
  out1.dir <- out1.dirs[i]
  
  from <- paste0(out1.path, '/', out1.dir)
  to <- paste0(merged.path, '/', out1.dir)
  
  file.rename(from, to)
  
  # log progress
  if (i %% 200 == 0)
    cat(format(round(100.0 * i / length(out1.dirs), 1), nsmall=1), '% \n')
}

rm(out1.dir)
rm(from)
rm(to)


cat('Processing folder', out2.path, '... \n')
for (i in 1:length(out2.dirs)) {
  out2.dir <- out2.dirs[i]
  
  from <- paste0(out2.path, '/', out2.dir)
  to <- paste0(merged.path, '/', out2.dir)
  
  file.rename(from, to)
  
  # log progress
  if (i %% 200 == 0)
    cat(format(round(100.0 * i / length(out2.dirs), 1), nsmall=1), '% \n')
}

rm(out2.dir)
rm(from)
rm(to)


cat('Finished.\n')

