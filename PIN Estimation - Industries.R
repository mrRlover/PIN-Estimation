pkgs <- c("data.table", "forecast", "tseries", "kableExtra", "reshape2", "InfoTrad", "mFilter", "tidyverse", "qrmtools", "tidyquant", 
          "stargazer", "qrmtools", "gridExtra", "tbl2xts", "highfrequency", "pinbasic")

# install.packages(pkgs) #install 

pkgs <- c(pkgs, "timeDate", "readxl")

sapply(pkgs, library, character.only = T)

industries <- read.csv()