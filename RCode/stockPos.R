library(GCAMCPUB)
library(portanalytics)
library(data.table)
library(openxlsx)

# tgtDate <- lubridate::ymd("2016-11-28")
n <- 20L
path <- file.path(
  "T:/2_共享区/4_组合管理部_Portforlio_Management/stockHoldingAshare"
)
portName <- c("CNPC", "Par", "Life", "Capital RMB", 
              "UV Individual", "UV Group", 
              "UL Anyi", "UL Stable", "UL Increase", 
              "UL Strategy", "UL Aggressive",
              "Flexible Allocation", "Multi Strategy No1", 
              "Selection No1", "Strategy No2",  "Strategy No3",
              "Strategy No9",  "Strategy No10", "Strategy No11", 
              "Strategy No12", "Strategy No13", "Strategy No54",
              "Strategy No55", "Strategy No56", "Strategy No57", 
              "Strategy No58", "Mighty Rotation", "Stable Increase")
# portNameCh = c("阳光团险", "分红险", "传统险", "人民币资本金", 
#                  "万能个险", "万能团险", 
#                  "投连安逸", "投连稳健", "投连增长", 
#                  "投连策略", "投连积极", 
#                  "灵活配置", "混合多策略1号",
#                  "精选1号", "策略精选2号", "策略精选3号",
#                  "策略精选9号", "策略精选10号", "策略精选11号", 
#                  "策略精选12号", "策略精选13号", "策略精选54号", 
#                  "策略精选55号", "策略精选56号", "策略精选57号",
#                  "策略精选58号", "强势轮动", "稳健添利1号")
portOrder <- coreInfo$read("HSPortInfo")[Port_Name %in% portName, ][, .(
  Port_Name, Port_Name_CN
)] %>%
  unique()

tradingDate <- data.table(
  Date = get_trading_day(
    Sys.Date() - 1,
    length = 50, 
    type = "a-shares",
    incl_month_end = FALSE)
)
# detect if core holding has been updated
lastPosDate <- (function() {
  db_mo <- GCAMCPUB::activate_conn(db_mo)
  on.exit(DBI::dbDisconnect(db_mo))
  sql <- "select max(Date) as Max_Date from CORE_Data_Holding"
  maxDate <- DBI::dbGetQuery(db_mo, sql)$Max_Date %>% to_date
  maxDate
})()
local({
  expectedDate <- last(tradingDate$Date)
  if (expectedDate > lastPosDate) stop("The CORE hasn't updated to ", expectedDate, " yet.")
})
pathExcel <-
  format(lastPosDate, "%Y.%m.%d") %>%
  sprintf("最近20交易日股票平均市值%s.xlsx", .) %>%
  file.path(path, .)
readDate <- c(tradingDate$Date[1], tradingDate$Date[nrow(tradingDate)])
readFromLdc(readDate, ports = portName)
tradingDateN <- tradingDate[(nrow(tradingDate) - n + 1) : nrow(tradingDate)]
local({
  trans <- dataCenter$trans
  attachHSPortInfo(trans, c("HS_Port_Code", "Port_Name"))
  nRow <- nrow(trans[Date %in% tradingDateN & 
                       Sec_Code %in% c("002385.SZ", "000920.SZ", "002007.SZ") &
                 HS_Port_Code %in% c("2022", "2052")])
  if (nRow != 0) stop("There is trans in old UL Aggressive or Incrase ports. Check with O32.")
})

pos <- dataCenter$pos
attachHSPortInfo(pos, c("HS_Port_Name", "Port_Name"))
pos[, Mkt := stringr::str_sub(Sec_Code, 8, 9)]
pos <- pos[!HS_Port_Code %in% c(1022, 1032) &
             Date %in% tradingDateN$Date &
             Class_L3 == 'Stock' &
             Mkt %in% c('SZ', 'SH'), ]
# pos[HS_Port_Name %in% c("投连积极-工行场内", "投连增长-工行场内") & 
#       Sec_Code %in% c("002385.SZ", "000920.SZ", "002007.SZ"), 
#     Port_Name := "UL Stable"]
# JuYuan info
ora_jy <- activate_conn(ora_jy)
jy_mkt_suffix =
  data.table::data.table(
    SecuMarket = c(83, 90, 89),
    Suffix = c(".SH", ".SZ", ".IB"),
    key = "SecuMarket"
  )

sql <- "select a.SecuCode, a.SecuMarket, a.SecuAbbr,
a.InnerCode, b.TradingDay, b.ClosePrice from JYDB.SecuMain a left join
(select InnerCode, TradingDay, ClosePrice from JYDB.QT_DailyQuote) b
on a.InnerCode = b.Innercode
where a.SecuMarket in (83, 90) and
a.SecuCategory = 1 and a.ListedState not in (5)
and b.TradingDay >= date'%s' and b.TradingDay <= date'%s'"
sql <- sprintf(sql, format(lastPosDate - 50, "%Y-%m-%d"), format(lastPosDate, "%Y-%m-%d"))

aShare <-
  DBI::dbGetQuery(ora_jy, sql) %>%
  data.table::setDT() %>%
  data.table::setnames(c(
    "Secu_Code", "Secu_Market", "Secu_Name",
    "Inner_Code", "Trading_Day", 'Close_Price'
  ))
aShare[, c("Sec_Code", "Secu_Code", "Secu_Market", "Trading_Day") := list(
  paste0(Secu_Code, jy_mkt_suffix[J(aShare$Secu_Market), Suffix]),
  NULL, NULL, GCAMCPUB::to_date(Trading_Day)
)]
aShare <- aShare[order(Trading_Day)]
# match the last trading day close price
pos[, Date_Join := Date]
aShare[, Date_Join := Trading_Day]
pos <- aShare[pos, roll = TRUE, on = c("Sec_Code", "Date_Join")]
pos <- pos[, .(Date, Port_Name, Sec_Code, Sec_Name, Quantity, Close_Price, Mkt)]
# tmpPos <- purrr::map(1:2, 
#                      ~pos[Sec_Code %in% c("000920.SZ", "002385.SZ", "002007.SZ") &
#                             Port_Name %in% c("UL Increase", "UL Aggressive")]) %>% 
#   rbindlist
tmpPos <- pos[Sec_Code %in% c("000920.SZ", "002385.SZ", "002007.SZ") &
                Port_Name %in% c("UL Increase", "UL Aggressive")]
oldStock <- data.table(Port_Name = c(rep("UL Aggressive", 3),
                                     rep("UL Increase", 3)),
                       Sec_Code = rep(c("002385.SZ", "000920.SZ", "002007.SZ"), 2),
                       Quantity_Old = c(365000, 199873, 184320, 365900, 370650, 181440))
tmpPos <- oldStock[tmpPos, on = c("Port_Name", "Sec_Code")]
posNew <- tmpPos[, Quantity_New := Quantity - Quantity_Old]
posNew <- posNew[, .(Date, Port_Name, Sec_Code, Sec_Name, Quantity = Quantity_New, 
                     Close_Price, Mkt)]
posOld <- tmpPos[, .(Date, Port_Name, Sec_Code, Sec_Name, Quantity = Quantity_Old,
                     Close_Price, Mkt)]
posOld[, Port_Name := "UL Stable"]
posOthers <- pos[!Sec_Code %in% c("000920.SZ", "002385.SZ", "002007.SZ") |
                   !Port_Name %in% c("UL Increase", "UL Aggressive")]
tmpPos <- rbind(posNew, posOld)
pos <- rbind(posOthers[, .(Date, Port_Name, Sec_Code, Sec_Name, Quantity,
                           Close_Price, Mkt)], tmpPos)
pos[, `:=`(
  Close_Price = GCAMCPUB::na_fill(Close_Price, 0.0),
  AV_Close = Quantity * GCAMCPUB::na_fill(Close_Price, 0.0)
)]
tmp <- dcast(pos, Port_Name + Date ~ Mkt, fun = sum, value.var = 'AV_Close')
tmp[, `:=`(
  SH = GCAMCPUB::na_fill(SH, 0.0),
  SZ = GCAMCPUB::na_fill(SZ, 0.0)
)]
avgPos <- tmp[, .(SH = sum(SH) / n, SZ = sum(SZ) / n), by = .(Port_Name)]
avgPos[, `:=`(
  Start_Date = as.Date(tradingDateN$Date[1]),
  End_Date = as.Date(tradingDateN$Date[n])
)]
avgPos <- avgPos[portOrder, on = "Port_Name"][, .(
  Port_Name, Port_Name_CN, Start_Date, End_Date, SH, SZ
)]
# # old Holding Account
# 
# oldStock <- tradingDateN %>% tidyr::crossing(oldStock)
# oldStock <- unique(pos[, .(Date, Sec_Code, Close_Price)])[oldStock, on = c("Date", "Sec_Code")]
# oldStock[, AV_Close := Close_Price * Quantity]
# oldStockSum <- oldStock[, .(AV_Close = sum(AV_Close) / n), by = .(Port_Name)]
# #
# avgPos[Port_Name == "UL Stable",]$SZ <-  avgPos[Port_Name == "UL Stable",]$SZ +
#   sum(oldStockSum$AV_Close)
# avgPos[Port_Name == "UL Aggressive",]$SZ <-  avgPos[Port_Name == "UL Aggressive",]$SZ -
#   oldStockSum[Port_Name == "UL Aggressive Old"]$AV_Close
# avgPos[Port_Name == "UL Increase",]$SZ <-  avgPos[Port_Name == "UL Increase",]$SZ -
#   oldStockSum[Port_Name == "UL Increase Old"]$AV_Close
# 

# write excel -------------------------------------------------------------

styleHeader <-
  createStyle(valign = "center", halign = "center", wrapText = TRUE,
              border = "TopBottomLeftRight", fgFill = "grey")

wb <- createWorkbook("stockHolding")
addWorksheet(wb, "持仓信息")
writeData(wb, 1, avgPos, borders = "all", headerStyle = styleHeader)
addStyle(wb, 1, createStyle(numFmt = "ACCOUNTING"), rows = seq_len(nrow(pos)) + 1,
         cols = which(colnames(avgPos) %in% c("SH", "SZ")),
         stack = TRUE, gridExpand = TRUE)
setColWidths(wb, sheet = 1,
             cols = which(colnames(avgPos) %in% 
                            c("Port_Name", "Port_Name_CN", "Start_Date", "End_Date")),
             widths = c(20, 20, 15, 15))
setColWidths(wb, sheet = 1,
             cols = which(colnames(avgPos) %in% 
                            c("SH", "SZ")),
             widths = c(20, 20))
showGridLines(wb, 1, showGridLines = FALSE)
# setColWidths(wb, sheet = 1, cols = seq_len(ncol(avgPos)), widths = "auto")
saveWorkbook(wb, pathExcel, overwrite = TRUE)

