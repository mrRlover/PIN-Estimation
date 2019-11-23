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

first_year <- substr(index(buy_data[1,]), 1, 4)

last_year <- substr(index(buy_data[nrow(buy_data),]), 1, 4)

yrs <- as.numeric(first_year):as.numeric(last_year) %>%
  as.character()

pin_res <- matrix(NA, nrow = 6*length(yrs), ncol = ncol(buy_data))
pin_res <- data.frame(pin_res)
colnames(pin_res) <- colnames(buy_data)

res_yrs <- matrix(NA, nrow = 6*length(yrs), ncol = 1)
colnames(res_yrs) <- "Year"

temp_data <- data.frame(Buy=c(350,250,500,552,163,345,847,923,123,349),
                        Sell=c(382,500,463,550,200,323,456,342,578,455))

result <- EA(temp_data)

Parameter <- as.data.frame(names(result)[-6])
colnames(Parameter) <- "Parameter"

Parameter <- Parameter %>% slice(rep(row_number(), length(yrs)))

pin_res <- cbind(Parameter, res_yrs, pin_res)

a <- 1
b <- 6

for (yr in yrs) {
  
  pin_res[a:b, 2] <- yr
  
  for (i in numcols) {
    
    pindata <- cbind(buy_data[yr, i], sell_data[yr, i])
    
    if (all(is.na(pindata))==TRUE){

    } else if (sum(is.na(pindata)==F) < 80 * 2) {
      
    } else {
      
      pindata[is.na(pindata)] <- 0
      result=EA(pindata)
      
      result <- result[-6]
      pin_res[a:b, i+2] <- unlist(result) %>% as.numeric() %>% as.data.frame()
      
    }
    
  }
  
  a <- a + 6
  b <- b + 6
  
}







