pkgs <- c("data.table", "forecast", "tseries", "kableExtra", "reshape2", "InfoTrad", "mFilter", "tidyverse", "qrmtools", "tidyquant", 
          "stargazer", "qrmtools", "gridExtra", "tbl2xts", "highfrequency", "pinbasic")

# install.packages(pkgs) #install 

pkgs <- c(pkgs, "timeDate", "readxl")

sapply(pkgs, library, character.only = T)

buy_file <- paste0(getwd(), "/", "Data", "/", "buys.csv")

sell_file <- paste0(getwd(), "/", "Data", "/", "sells.csv")

buy_data <- read.csv(buy_file)

sell_data <- read.csv(sell_file)

test <- colnames(buy_data) %in% colnames(sell_data)

cn <- which(test==FALSE)

buy_data <- buy_data[, -cn, drop = FALSE]

buy_data$Date <- ymd(buy_data$Date)

buy_data <- tbl_xts(buy_data)

sell_data$Date <- ymd(sell_data$Date)

sell_data <- tbl_xts(sell_data)

numcols <- 1:ncol(sell_data)

yr_mon <- buy_data %>%
  apply.monthly(., sum) %>%
  index() %>%
  as.character()

pin_res <- matrix(NA, nrow = 6*length(yr_mon), ncol = ncol(buy_data))
pin_res <- data.frame(pin_res)
colnames(pin_res) <- colnames(buy_data)

res_yrs <- matrix(NA, nrow = 6*length(yr_mon), ncol = 1)
colnames(res_yrs) <- "YearMon"

pin_res <- cbind(res_yrs, pin_res)

a <- 1
b <- 6


for (yr in yr_mon) {
  
  pin_res[a:b, 1] <- yr
  
  for (i in numcols) {
    
    ext <- substr(yr, 1, 7)
    
    pindata <- cbind(buy_data[ext, i], sell_data[ext, i])
    colnames(pindata) <- c("Buy", "Sell")
    
    if (all(is.na(pindata))==TRUE){
      
    } else if (sum(is.na(pindata)==FALSE) < 20) {
      
    } else {
      
      pindata[is.na(pindata)] <- 0 
      result <- pin_est(numbuys = pindata[,"Buy"],
                        numsells = pindata[,"Sell"], 
                        confint = TRUE, ci_control = list(n = 1000, seed = 123), 
                        posterior = TRUE)
      
      df <- result[[1]][,1, drop = F]
      
      df <- rbind(df, result[[3]])
      rownames(df)[6] <- "PIN"
      pin_res[a:b, i+1] <- df
      
    }
    
  }
  
  a <- a + 6
  b <- b + 6
  
}


Parameter <- as.data.frame(rownames(df))
colnames(Parameter) <- "Parameter"

pin_df <- as.data.frame("PIN")
colnames(pin_df) <- colnames(Parameter)

Parameter <- rbind(Parameter, pin_df)

Parameter <- Parameter %>% slice(rep(row_number(), length(yr_mon)))

mon_pin <- cbind(Parameter, pin_res)

write.csv(mon_pin, "PIN Estimation Results - Monthly.csv", row.names = FALSE)

saveRDS(mon_pin, file = "PIN Estimation Results - Monthly.rds")
