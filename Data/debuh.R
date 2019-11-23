pkgs <- c("data.table", "forecast", "tseries", "kableExtra", "reshape2", "InfoTrad", "mFilter", "tidyverse", "qrmtools", "tidyquant", 
          "stargazer", "qrmtools", "gridExtra", "tbl2xts", "highfrequency", "pinbasic")

# install.packages(pkgs) #install 

pkgs <- c(pkgs, "timeDate", "readxl")

sapply(pkgs, library, character.only = T)

pin_names <- file.info(list.files 
                       (path = getwd(), 
                         pattern = "*.csv", 
                         full.names = T))

pin_files <- rownames(pin_names)

buy_list <- vector("list", length = nrow(pin_names))

i <- 1

code_list <- vector("list", length = length(pin_files))

r <- 1

for (pf in pin_files) {
  pin_data <- read.csv(pf)
  
  LBD <- unique(pin_data[, "LogicalBusinessDay", drop = T])
  LBD <- as.character(LBD)
  
  for (dates in LBD) {
    
    buys_only <- pin_data %>%
      filter(BuySellIndicator == "B") %>%
      filter(LogicalBusinessDay == dates)
    
    if (dates==LBD[1]) {
      
      code <- t(buys_only[, 1, drop = F])
      code <- code[1, ]
      
    } else {
      
      temp <- t(buys_only[, 1, drop = F])
      temp <- temp[1,]
      
      if (length(temp) > length(code)) {
        code <- temp
        print(length(code))
      }
    }
    
  }
  
  code_list[[r]] <- code
  r <- r + 1
  
}


max(code_list)
lengths <- lapply(code_list, length)
longest <- which.max(lengths)
code <- code_list[[longest]]

yrs <- as.character(2008:2018)

names(code_list) <- yrs

df <- bind_rows(code_list)

dfs <- ""

for (df in code_list) {
  
  dfs <- c(dfs, df)
  
}

tickers <- unique(dfs)

