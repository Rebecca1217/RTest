library(data.table)
library(portanalytics)
library(GCAMCPUB)
library(WindR)

last_pos_date <- local({
  
  db_mo <- activate_conn("db_mo")
  on.exit(DBI::dbDisconnect(db_mo), add = TRUE)
  
  sql <- "Select max(Date) as Max_Date from CORE_Data_Holding"
  max_date <- DBI::dbGetQuery(db_mo, sql)$Max_Date %>% to_date
  max_date
})

tgt_date <- as.Date("2017-12-12")

readFromLdc(last_pos_date, c("All cost based", "Unit linked"), jyDateRange = NULL)

pos <- attachHSPortInfo(dataCenter$pos, "Port_Name")
class_info <- portanalytics::coreInfo$read("ClassInfo")
pos <- class_info[pos, on = "Class_L3"]
pos_bond <- pos[if_Standard_Bond == 1]
pos_bond[, Suffix := str_right(Sec_Code, 2)]
pos_ex_bond <- pos_bond[Suffix %in% c("SH", "SZ")]

pos_now <- pos_ex_bond[Date == last_pos_date]


latest_trade_day <- max(
  GCAMCPUB::get_trading_day(
    date_to = Sys.Date()-1,length = 7,type = "a-shares"))

no_trade_bond <- local({
  
  start_windr()
  on.exit(w.stop(), add = TRUE)
  
  sec_code_seq <- unique(pos_now$Sec_Code)
  
  w_wss_data<-w.wss(sec_code_seq,'lastradeday_s,dirty_cnbd,net_cnbd',
                    paste0('tradeDate=', latest_trade_day),
                    'credibility=1')
  stopifnot(w_wss_data$ErrorCode == 0)
  tmp <- w_wss_data$Data %>% setDT() %>% setnames(c("Sec_Code", "Last_Trade_Day",
                                                    "Dirty_CNBD", "Net_CNBD"))
  tmp <- tmp[, Last_Trade_Day := as.Date(Last_Trade_Day, "1899-12-30")]
  
  tmp <- tmp[Last_Trade_Day < "2017-07-01"]
  
  
})

pos_no_trade_bond <- pos_now[Sec_Code %in% no_trade_bond$Sec_Code,
                             .(Port_Name, Sec_Code, Sec_Name, Class_L3)]

pos_no_trade_bond <- no_trade_bond[pos_no_trade_bond, on = "Sec_Code"]

write_open_xlsx(pos_no_trade_bond[, .(Port_Name, Sec_Code, Sec_Name,
                                      Class_L3, Last_Trade_Day, Dirty_CNBD, Net_CNBD)])

