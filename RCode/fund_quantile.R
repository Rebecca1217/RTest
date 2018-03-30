library(WindR)
library(GCAMCPUB)
library(data.table)
library(dplyr)

quantile_para <- 0.5
# fund_code <- c("000008.OF,000041.OF,000042.OF,000043.OF,000044.OF")
fund_code <- readxl::read_excel("C:/Users/AMC161/Desktop/tmp.xlsx")$Sec_Code %>% unique()

# data reading ------------------------------------------------------------

w.start()
w_wsd_data<-w.wsd(fund_code,
                  "NAV_adj_return1","2017-01-01","2017-12-31","Fill=Previous;PriceAdj=B")
stopifnot(w_wsd_data$ErrorCode == 0L)
data <- w_wsd_data$Data %>% setDT()


f <- function(x) {
  x <- cumprod(1 + x / 100) - 1
}
cum_rtn <- lapply(data[, -"DATETIME", with = FALSE], f) %>% setDT()

cum_rtn <- cbind(data$DATETIME, cum_rtn) %>% setDT()

# find quantile 
quantile <- apply(cum_rtn[, -"V1", with = FALSE], 1, quantile, quantile_para, na.rm = TRUE)
quantile <- data.table(Quantile = quantile)


res <- cbind(cum_rtn, quantile)
tmp <- res[, .(V1, Quantile)]
