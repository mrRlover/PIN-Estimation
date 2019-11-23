pkgs <- c("data.table", "forecast", "tseries", "kableExtra", "reshape2", "InfoTrad", "mFilter", "tidyverse", "qrmtools", "tidyquant", 
          "stargazer", "qrmtools", "gridExtra", "tbl2xts", "highfrequency", "pinbasic")

 install.packages(pkgs) #install 

pkgs <- c(pkgs, "timeDate", "readxl")

sapply(pkgs, library, character.only = T)

pin_names <- file.info(list.files 
                       (path = getwd(), 
                         pattern = "*.csv", 
                         full.names = T))

pin_files <- rownames(pin_names)

buy_list <- vector("list", length = nrow(pin_names))

i <- 1

code_list <- vector("list", length = 11*252)

r <- 1

for (pf in pin_files) {
  pin_data <- read.csv(pf)
  
  LBD <- unique(pin_data[, "LogicalBusinessDay", drop = T])
  LBD <- as.character(LBD)
  
  for (dates in LBD) {
    
    buys_only <- pin_data %>%
      filter(BuySellIndicator == "B") %>%
      filter(LogicalBusinessDay == dates)
    
    code <- t(buys_only[, 1, drop = F])
    code <- code[1, ]
    
    code_list[[r]] <- code
    r <- r + 1
  }
}


dfs <- ""

for (df in code_list) {
  
  dfs <- c(dfs, df)
  
}

tickers <- unique(dfs)

buys <- matrix(NA, nrow = 252 * 11, ncol = length(tickers))
buys <- data.frame(buys)
colnames(buys) <- tickers
trade_dates <- matrix(NA, nrow = 252 * 11, ncol = 1)
colnames(trade_dates) <- "Date"
buys <- cbind(trade_dates, buys)

r <- 1

for (pf in pin_files) {
  pin_data <- read.csv(pf)
  
  LBD <- unique(pin_data[, "LogicalBusinessDay", drop = T])
  LBD <- as.character(LBD)
  
  for (dates in LBD) {
  
    buys_only <- pin_data %>%
      filter(BuySellIndicator == "B") %>%
      filter(LogicalBusinessDay == dates)
    
    temp <- t(buys_only[, 1, drop = F])
    if(nchar(temp[1])==0){
      temp[1] <- colnames(buys)[2]
    }
    
    buys[r, temp] <- t(buys_only[, "CountOfTrades", drop = F])
    buys[r, 1] <- dates
    r <- r + 1
  }

}

code_file <- sub("Annual Pins", "pinestimation", paste0(getwd(), "/", "ALL COMPANIES.xlsx"))
codes <- read_xlsx(code_file, sheet = "Sheet2")[1]
codes <- t(codes)

companies <- codes[test==TRUE]

final_data <- buys[, companies, drop = F]
final_data <- cbind(buys[, 1, drop = FALSE], final_data)

final_data <- final_data[1:2749, , drop = FALSE]

write.csv(final_data, file = "buys.csv", row.names = FALSE)




