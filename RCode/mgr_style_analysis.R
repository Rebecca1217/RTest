# 2018.03.09
# Used for investment manager style analysis

# pkgs --------------------------------------------------------------------

library(portanalytics)
library(GCAMCPUB)
library(data.table)

# data prepare ------------------------------------------------------------

ptfs <- c("All cost based", "UL Strategy", "UL Increase", "UL Aggressive",
          "Selection No1", "Growth Return", "Selection Return",
          "Dynamic Factor Return No1", "Mighty Rotation", "Flexible Allocation",
          "HK AnXin")

# ptfs <- "UV Individual"
readFromLdc(c("201701", "201803"), ports = ptfs, jyDateRange = NULL,
            vars = c("pos", "trans", "pl"),
            classL3Def = "Class_L3_FundinDetail") # consitent with class_info

date_current <- max(dataCenter$pos$Date)
date_start <- f_date_begin(date_current, "year")

class_info <- coreInfo$read("ClassInfo")[, .(Class_L3, Class_L1)]
eq_class <- class_info[Class_L1 == "Equity"]$Class_L3

db_mo <- activate_conn("db_mo")
sql <- "Select Date, Port_Name, Stock_Sele_Mgr as Manager from
PortMgt_equity_control_mgr"
mgr_info <- sql2dt(db_mo, sql)
mgr_info[, Date := to_date(Date)]

eq_pos <- dataCenter$pos[Class_L3 %in% eq_class]
attachHSPortInfo(eq_pos, "Port_Name")

eq_trans <- dataCenter$trans[Class_L3 %in% eq_class]
attachHSPortInfo(eq_trans, "Port_Name")

# not sure if pl will be used 
# eq_pl <- dataCenter$pl[Class_L3 %in% eq_class]
# attachHSPortInfo(eq_pl, "Port_Name")

eq_pos <- mgr_info[eq_pos, on = c("Port_Name", "Date"), roll = TRUE]
eq_pos <- eq_pos[(! is.na(Manager)) & Manager != "NA"]
eq_pos[, Year := year(Date)]

eq_trans <- mgr_info[eq_trans, on = c("Port_Name", "Date"), roll = TRUE]
eq_trans <- eq_trans[(! is.na(Manager)) & Manager != "NA"]
eq_trans[, Year := year(Date)]

#select stock only
eq_pos <- eq_pos[Class_L3 == "Stock"]
eq_trans <- eq_trans[Class_L3 == "Stock"]
# turn over rate ----------------------------------------------------------

# min(buy, sell) / avg(AV_Book_LC)
# before use the f_turnover function, have to make sure the eq_trans is provided already.

f_turnover <- function(pos, manager){
  
  if(nrow(pos) == 0){
    return(NA)
  }
  pos <- pos[Class_L3 == "Stock"]
  if(nrow(pos) == 0){
    return(NA)
  }
  
  min_date <- min(pos$Date, na.rm = TRUE)
  max_date <- max(pos$Date, na.rm = TRUE)
  mgr <- manager
  
  eq_trans_i <- eq_trans[Manager == mgr & Class_L3 == "Stock"
                         & Date >= min_date & Date <= max_date]
  if(min(eq_trans_i$Date) < min_date | max(eq_trans_i$Date) > max_date)
    stop("Trans date exceed the pos date. Please check.")
  stopifnot(unique(eq_trans_i$Manager) == unique(pos$Manager))
  
  cf_buy <- - sum(eq_trans_i[Trans_Type == "买入"]$CF_LC)
  cf_sell <- sum(eq_trans_i[Trans_Type == "卖出"]$CF_LC)
  avg_av <- pos[Date >= min_date & Date <= max_date,
                .(AV = sum(AV_Book_LC, na.rm = TRUE)),
                  by = .(Date)]$AV %>%
    mean()
  turn_over_rate <- min(cf_buy, cf_sell) / avg_av
  turn_over_rate
}

turn_over_rate <- eq_pos[, .(Turn_Over_Rate = f_turnover(.SD, Manager)), 
                         by = .(Manager, Year)]
cast_tor <- dcast(turn_over_rate, formula = Manager~Year, fun = sum, value.var = "Turn_Over_Rate")

# sector allocation -----------------------------------------------------

stock_industry <- eq_pos[Date == date_current, .(Manager, Date, Sec_Code, AV_Mix_LC,
                                                 Sec_Name, SW_Sector1, SW_Sector2)]

industry_dom <- local({
  
  indus_dom <- stock_industry[
    stringr::str_sub(Sec_Code, stringr::str_length(Sec_Code) - 1, -1) != "HK"]
  
  indus_dom_mgr <- indus_dom[, .(AV = sum(AV_Mix_LC)), by = .(Manager, SW_Sector1)]
  indus_dom_mgr[, Ratio := AV / sum(AV), by = .(Manager)]
  
  indus_dom_total <- indus_dom[, .(AV = sum(AV_Mix_LC)), by = .(SW_Sector1)]
  indus_dom_total[, Ratio := AV / sum(AV)]
 
  mget(c("indus_dom_mgr", "indus_dom_total"))
})

industry_top3 <- local({
  
  indus_dom <- industry_dom$indus_dom_mgr
  setorder(indus_dom, Manager, -Ratio)
  indus_top3 <- indus_dom[, .SD[1:3], by = .(Manager)]
  indus_top3[, Label := paste0(SW_Sector1, f_fmt_pct(Ratio))]
  indus_top3[, Rank := c("Top1", "Top2", "Top3")]
  res <- dcast(indus_top3, formula = Manager~Rank, value.var = "Label")
  res
})

industry_last3 <- local({
  
  indus_dom <- industry_dom$indus_dom_mgr
  setorder(indus_dom, Manager, Ratio)
  indus_last3 <- indus_dom[, .SD[1:3], by = .(Manager)]
  indus_last3[, Label := paste0(SW_Sector1, f_fmt_pct(Ratio))]
  indus_last3[, Rank := c("Top1", "Top2", "Top3")]
  res <- dcast(indus_last3, formula = Manager~Rank, value.var = "Label")
  res
})


sw_universe <- read.csv("./external_data/sw_sector1_code.csv") %>%
  setDT() %>%
  setnames(c("Sector_Code", "SW_Sector1"))

# industry without coverage
# 
# sector_no_coverage <- setdiff(sw_universe$SW_Sector1,
#                               unique(industry_dom[Ratio > 0.05]$SW_Sector1))
# 
# if(length(sector_no_coverage) > 0) {
#   
#   sw_no_cover <- data.table(SW_Sector1 = sector_no_coverage)
# 
#   sw_no_cover <- sw_universe[sw_no_cover, on = "SW_Sector1"]
#   sw_no_cover[, `:=`(
#     Rtn_5D = w.wss(Sector_Code, 'pct_chg_5d')$Data$PCT_CHG_5D,
#     Rtn_1M = w.wss(Sector_Code, 'pct_chg_1m')$Data$PCT_CHG_1M,
#     Rtn_YTD = w.wss(Sector_Code, 'pct_chg_per', 
#                     paste0('startDate=', date_start),
#                     paste0('endDate=', date_current))$Data$PCT_CHG_PER
#   )]
# }


# combine all ports together

sw_combine_perf <- local({
  
  pos_combine <- industry_dom$indus_dom_total
  pos_combine <- sw_universe[pos_combine, on = "SW_Sector1"]
  pos_combine[, `:=`(
    Rtn_5D = w.wss(Sector_Code, 'pct_chg_5d')$Data$PCT_CHG_5D/100,
    Rtn_1M = w.wss(Sector_Code, 'pct_chg_1m')$Data$PCT_CHG_1M/100,
    Rtn_YTD = w.wss(Sector_Code, 'pct_chg_per', 
                    paste0('startDate=', date_start),
                    paste0('endDate=', Sys.Date()))$Data$PCT_CHG_PER/100
  )]
  setorder(pos_combine, -Ratio)
  pos_combine
})

# sector rotation time series

sw_ts <- eq_pos[stringr::str_sub(Sec_Code, stringr::str_length(Sec_Code) - 1, -1) != "HK",
                .(Date, Manager, Sec_Code, Sec_Name, SW_Sector1, SW_Sector2,
                    AV_Mix_LC, Quantity, UGL_LC)]
sql <- "Select Date from CORE_Data_Dates where if_Month_End = 1
and Date > '{{datefrom}}' and Date <= '{{dateto}}'"
sql <- infuser::infuse(sql, datefrom = min(eq_pos$Date), dateto = date_current)
dateseq <- sql2dt(db_mo, sql)
dateseq[, Date := to_date(Date)]
sw_ts <- local({
  
  sw_ts_ratio <- sw_ts[, .(AV = sum(AV_Mix_LC)), by = .(Date, Manager, SW_Sector1)]
  sw_ts_ratio[, Ratio := AV / sum(AV), by = .(Date, Manager)]
  setorder(sw_ts_ratio, Date, Manager, -Ratio)
  sw_ts_top <- sw_ts_ratio[, .SD[1], by = .(Date, Manager)]  
  sw_ts_top <- sw_ts_top[Date %in% dateseq$Date]
  sw_ts_top[, Label := paste0(SW_Sector1, f_fmt_pct(Ratio))]
  
  sw_ts_top
})
p <- ggplot(sw_ts_top[Manager != "混合产品"], aes(x = Date), size = 10) + 
  geom_tile(aes(y = Manager, fill = Ratio)) + 
  geom_text_tan(aes(y = Manager, label = SW_Sector1), size = 4.5) + 
  scale_fill_continuous(low = excel_colors$yellow, 
                        high = excel_colors$red, 
                        na.value = "lightgrey",
                        guide = "legend") +
  theme(axis.text.y = element_text(size = 14))
plot(p)

cast_sw_ts_top <- dcast(sw_ts_top, Manager~Date, value.var = "Label")

# Style: Market Cap Exposure, growth/value --------------------------------


eq_style <- eq_pos[Class_L3 == "Stock" & Date == date_current, 
                   .(Manager, Port_Name, Sec_Code, Sec_Name, 
                     Quantity, AV_Book_LC, AV_Mix_LC, UGL_LC)]






# stock concentration -----------------------------------------------------

stock_conc <- local({
  
  stock_conc <- eq_pos[Class_L3 == "Stock" & Date == date_current,
                       .(Manager, Port_Name, Sec_Code, Sec_Name, 
                         Quantity, AV_Book_LC, AV_Mix_LC, UGL_LC)]
  tmp <- stock_conc[, .(AV = sum(AV_Mix_LC)), by = .(Manager, Sec_Code, Sec_Name)]
  tmp[, Ratio := AV / sum(AV), by = .(Manager)]
  setorder(tmp, Manager, -Ratio)
  tmp <- tmp[, .SD[1:3], by = .(Manager)]
  tmp[, Rank := c(1, 2, 3)]
  
  tmp
})

p <- ggplot(tmp[Manager != "混合产品"], aes(x = Rank), size = 10) + 
  geom_tile(aes(y = Manager, fill = Ratio)) + 
  geom_text_tan(aes(y = Manager, label = Sec_Name), size = 4.5) + 
  scale_fill_continuous(low = excel_colors$yellow, 
                        high = excel_colors$red, 
                        na.value = "lightgrey",
                        guide = "legend") +
  theme(axis.text.y = element_text(size = 14))
plot(p)

stock_conc[, `:=`(
  Label = paste0(Sec_Name, f_fmt_pct(Ratio))
)]
cast_stock_conc <- dcast(stock_conc, Manager~Rank, value.var = "Label")


















