pkgs <- c("data.table", "forecast", "tseries", "kableExtra", "reshape2", "InfoTrad", "mFilter", "tidyverse", "qrmtools", "tidyquant", 
          "stargazer", "qrmtools", "gridExtra", "tbl2xts", "highfrequency", "pinbasic")

# install.packages(pkgs) #install 

pkgs <- c(pkgs, "timeDate", "readxl")

sapply(pkgs, library, character.only = T)

pin_names <- file.info(list.files 
                       (path = getwd(), 
                         pattern = "[0-9]{4}.csv", 
                         full.names = T))

pin_files <- rownames(pin_names)

sell_list <- vector("list", length = nrow(pin_names))

i <- 1

code_list <- vector("list", length = 11*252)

r <- 1

for (pf in pin_files) {
  pin_data <- read.csv(pf)
  
  LBD <- unique(pin_data[, "LogicalBusinessDay", drop = T])
  LBD <- as.character(LBD)
  
  for (dates in LBD) {
    
    sells_only <- pin_data %>%
      filter(BuySellIndicator == "S") %>%
      filter(LogicalBusinessDay == dates)
    
    code <- t(sells_only[, 1, drop = F])
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

sells <- matrix(NA, nrow = 252 * 11, ncol = length(tickers))
sells <- data.frame(sells)
colnames(sells) <- tickers
trade_dates <- matrix(NA, nrow = 252 * 11, ncol = 1)
colnames(trade_dates) <- "Date"
sells <- cbind(trade_dates, sells)

r <- 1

for (pf in pin_files) {
  pin_data <- read.csv(pf)
  
  LBD <- unique(pin_data[, "LogicalBusinessDay", drop = T])
  LBD <- as.character(LBD)
  
  for (dates in LBD) {
    
    sells_only <- pin_data %>%
      filter(BuySellIndicator == "S") %>%
      filter(LogicalBusinessDay == dates)
    
    temp <- t(sells_only[, 1, drop = F])
    if(nchar(temp[1])==0){
      temp[1] <- colnames(sells)[2]
    }
    
    sells[r, temp] <- t(sells_only[, "CountOfTrades", drop = F])
    sells[r, 1] <- dates
    r <- r + 1
  }
  
}

code_file <- sub("Annual Pins", "pinestimation", paste0(getwd(), "/", "ALL COMPANIES.xlsx"))
codes <- read_xlsx(code_file, sheet = "Sheet2")[1]
codes <- t(codes)

test <- codes %in% colnames(sells)

companies <- codes[test==TRUE]

final_data <- sells[, companies, drop = F]
final_data <- cbind(sells[, 1, drop = FALSE], final_data)

final_data <- final_data[1:2749, , drop = FALSE]

write.csv(final_data, file = "sells.csv", row.names = FALSE)




