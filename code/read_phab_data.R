# 04 read phab data

# devtools::install_github("jimhester/archive")
library(archive)
library(readr)
library(tictoc)

# read a zip
# tic("reading zip")
# dat <- read_csv("data/phab-rafi-15apr19.csv.zip")
# toc() # reading zip: 154.748 sec elapsed

# read a bz2
# tic("read bz2")
# dat2 <- read_csv(file_read("data/phab-rafi-15apr19.csv.bz2"), col_types = cols())
# toc() # read bz2: 218.258 sec elapsed

# read a 7z
# tic("read 7z")
# dat3 <- read_csv(archive_read("data/phab-rafi-15apr19.csv.7z"), col_types=cols())
# toc() # read 7z: 148.52 sec elapsed

# save out:
# save(pdat, file = "data/phab-rafi-15apr19.rda", compress = "gzip")

tic("read rdata")
load("data/phab-rafi-15apr19.rda")
toc() # read rdata: 55.923 sec elapsed
