# simulate colwells testing

library(hydrostats)
library(lubridate)
library(tidyverse)

# full year
dates <- seq.Date(ymd("2019-10-01"),ymd("2021-09-30"), length.out = 365)

# mult years
dates <- seq.Date(ymd("2015-10-01"),ymd("2021-09-30"), by="day")
length(dates)

# Sim 1: Cyclical ---------------------------------------------------

# make a sine seq with perfect consistency
Q <- sin(seq(1, 30*pi, length.out=length(dates))) + 100
# make a tibble
df <- data.frame(Date = dates, Q) 
summary(df)
#View(df)
plot(df,type="l")

# hi repetition within season
hydrostats::Colwells(df)$MP 
# 0.77 for 2 yrs, 0.85 for 1 yr
# 0.85 for 6 yrs

# Sim 2: Cyclical Release -------------------------------------------------------------------

# make a sine seq with perfect consistency
Q <- sin(seq(1, 2.1*pi, length.out=length(dates))) + 100
# make a tibble
df <- data.frame(Date = dates, Q) 
summary(df)
#View(df)
plot(df,type="l")

# more typical seasonal?
hydrostats::Colwells(df, )$MP # 0.76 (multi yr), .86 single yr

# Sim: PEAKING RELEASES -------------------------------------------------------------------

# blocky
Q <- c(rep(c(10,10,10,rep(40, 20)), length.out=length(dates)))
# make a tibble
df <- data.frame(Date = dates, Q) 
summary(df)
plot(df,type="l")

# hydropeaking
hydrostats::Colwells(df)$MP # 0.23

# Sim: Flat GW Release -------------------------------------------------------------------

# make a  seq with perfect consistency
Q <- seq(40,40, length.out=length(dates))
# make a tibble
df <- data.frame(Date = dates, Q) 
summary(df)
#View(df)
plot(df,type="l")


hydrostats::Colwells(df)$MP
# zero

# Sim: SemiFlat GW Release -------------------------------------------------------------------

# make a  seq with perfect consistency
Q <- seq(40,50, length.out=length(dates))
# make a tibble
df <- data.frame(Date = dates, Q) 
summary(df)
#View(df)
plot(df,type="l")

hydrostats::Colwells(df)$MP
# super high: 1!

# Sim: SemiFlat GW Release -------------------------------------------------------------------

# make a  seq with perfect consistency
Q <- sin(seq(1, 1.9*pi, length.out=length(dates))) + 100
# make a tibble
df <- data.frame(Date = dates, Q) 
summary(df)
#View(df)
plot(df,type="l")


hydrostats::Colwells(df)$MP
# multiyrs: .67
