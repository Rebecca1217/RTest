library(data.table)
library(dplyr)
library(GCAMCPUB)
# Note: the InputDate cannot be a date during  a 7-day holiday(3-day is fine),e.g. Spring Festival
# this file is set up to check if there is some problem in the PL_LC variable
# with the PL_Type equal '利息收入', except IAS = 'Cash'
# input the query Date, the output will be a matrix containing the past 3 months PL and a plot
# The result in output test matrix: PL_avg_curr, PL_avg_1, PL_avg_2 represent the average interest PL
# of the current month, 1 month before and 2 months before respectively. 
# The percentcurr_1 represents the PL ratio by PL_avg_currentmonth / PL_avg_1monthbefore

db_mo <- GCAMCPUB::activate_conn(db_mo)
InputDate <- GCAMCPUB::to_date('2016-02-15')
# Firstly, set up the function to find the month index under different circumtances
# normally, the InputDate is the current date, n represent the number of months you want to 
# request back, and the result is the month_begin_date n months before
# e.g. fun_month('2016-03-08', 2) returns '2016-01-01'
fun_month <- function(InputDate, n) {
  if (as.numeric(substr(InputDate, 6, 7)) > n) {
    month <- as.numeric(substr(InputDate, 6, 7)) - n
    if(nchar(as.numeric(month)) == 1){
      month <- paste0("0", as.character(month))} else {
        month <- as.character(month)
      }
    date_1 <- to_date(paste0(substr(InputDate, 1, 4), "-", month, '-01'))
  } else {
    month <- as.numeric(substr(InputDate, 6, 7)) - n + 12
    date_1 <- to_date(paste0(as.numeric(substr(InputDate, 1, 4)) - 1, "-", month, '-01'))
  }
  return
  date_1
}

# find the begin date of the past three months, store it as "getdate"
# if you want to see more months, change the following "n"
getdate <- fun_month(InputDate, n = 2)

# get the past 3 months raw data(including the current month)
sql <- "select  Date, HS_Port_Code, HS_Port_Name,Port_Name, IAS, Class_L3, Sec_Code, Sec_Name, PL_LC
        from core_data_PL where Date >= '%s'
        and Port_Type in ('cost based', 'cost based FC')
        and PL_Type = '利息收入'"
sql <- sprintf(sql, getdate)
PL_bck <- dbGetQuery(db_mo, sql) %>%
  data.table::setDT() %>%
  mutate(Date = GCAMCPUB::to_date(Date))

PL <-PL_bck[IAS != 'Cash', ] %>%
    select(Date, HS_Port_Code, HS_Port_Name, Sec_Code, IAS, Class_L3, PL_LC)

# Secondly, set up the function for PL_avg computation of all months
# parameter: date
# output: PL_sum matrix with index and the avg PL of the input month
fun_pl_avg <- function(date){
  if(f_date_end(date, "month") == f_date_end(InputDate, "month")){
    date_end <- InputDate
  } else {
    date_end <- f_date_end(date, "month")
  }
  date_begin <- f_date_begin(date, "month")
  # if the date_begin/end is not working day, find the nearest working day which has data
  if (date_begin %in% PL$Date){date_begin <- date_begin} else{
    if((date_begin +1) %in% PL$Date){
      date_begin <- date_begin + 1
    } else {
      if((date_begin + 2) %in% PL$Date) {
        date_begin <- date_begin + 2
      } else {date_begin <- date_begin +3}
    }
  }
  if (date_end %in% PL$Date){date_end <- date_end} else {
    if((date_end - 1) %in% PL$Date){
      date_end <- date_end - 1
    } else{date_end <- date_end - 2}
  }
  PL_avg_begin <- PL[Date == date_begin, list(PL_avg_begin = mean(PL_LC)),
                     by = c("HS_Port_Code", "Sec_Code", "IAS", "Class_L3")]
  PL_avg_end <- PL[Date == date_end, list(PL_avg_end = mean(PL_LC)),
                   by = c("HS_Port_Code", "Sec_Code", "IAS", "Class_L3")]
  PL_avg <- PL_avg_begin[PL_avg_end, on = c("HS_Port_Code", "Sec_Code", "IAS", "Class_L3")]
  PL_avg$PL_avg_begin <- na_fill(PL_avg$PL_avg_begin, 0.0)
  PL_avg <- PL_avg[, PL_avg := PL_avg_end - PL_avg_begin] %>%
    select_(quote(-PL_avg_begin), quote(-PL_avg_end))
  return 
  PL_avg
}
# use the fun_pl_avg for computation of all months
# naming rules:
# _curr represents the current month
# _1 represents 1 month before
# _2 represents 2 months before
PL_avg_curr <- fun_pl_avg(InputDate)
PL_avg_curr <- rename(PL_avg_curr, PL_avg_curr = PL_avg)
date_1 <- fun_month(InputDate, 1)
PL_avg_1 <- fun_pl_avg(date_1)
PL_avg_1 <- rename(PL_avg_1, PL_avg_1 = PL_avg)
date_2 <- fun_month(InputDate, 2)
PL_avg_2 <- fun_pl_avg(date_2)
PL_avg_2 <- rename(PL_avg_2, PL_avg_2 = PL_avg)

test <- PL_avg_1[PL_avg_curr, on = c("HS_Port_Code", "Sec_Code", "IAS", "Class_L3")]
test <- PL_avg_2[test, on = c("HS_Port_Code", "Sec_Code", "IAS", "Class_L3")]

 
test[, percentcurr_1 := PL_avg_curr / PL_avg_1]
test[, percent_1_2 := PL_avg_1 / PL_avg_2]
test[, percentcurr_2 := PL_avg_curr / PL_avg_2]
  
## match Port_Name
names <- unique(select(PL_bck, HS_Port_Code, HS_Port_Name, Sec_Code, IAS, Class_L3, Port_Name))
test <- names[test, on = c("HS_Port_Code", "Sec_Code", "IAS", "Class_L3")]
  
## plot
par(mfrow = c(2,2))
boxplot(test$PL_avg_2, test$PL_avg_1, test$PL_avg_curr,
          xlab = "month", main = 'boxplot of the PL in recent 3 months')
plot(test$percentcurr_1, xlab = '账户', ylab = NULL, main = 'current month / 1 month before')
plot(test$percent_1_2, xlab = '账户', ylab = NULL, main = '1 month before / 2 months before')
plot(test$percentcurr_2, xlab = '账户', ylab = NULL, main = 'current month / 2 months before')
  
#suspicious_0201 <- test[percent_2_1 > 10, ]
#suspicious_0301 <- test[percentcurr_2 > 10, ]
#suspicious_0302 <- test[percentcurr_1 > 2, ]
#suspicious <- test[percent0201 > 10 | percent0301 > 10 | percent0302 > 2, ]
#write.table(suspicious, file = 'PL_suspicious.csv', row.names = F, sep = ",")


