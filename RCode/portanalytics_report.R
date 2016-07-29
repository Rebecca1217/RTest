library(GCAMCPUB)
library(data.table)
library(dplyr)
library(portanalytics)
library(xtable)
#library(sca) # to show data in percent style

###for past data testing, the equity_investment of PL_coming part was set to 0.0.
#还需要修改的：+UGL的收益率，是Class_L1 = 'Equity' or if_fund = 1
# 纠正格式的时候data.table要copy才能保证原来的不变
#library(sca) # to show data in percent style

#define the fmt function
fmting <- function(x, unit, escape = FALSE, zero_fmt = "-") {
  if (is.character(x)) return(x)
  x <- switch(
    unit,
    "thousand" = GCAMCPUB::f_fmt(x / 1e3, digits = 0, zero_fmt = zero_fmt),
    "mn" = GCAMCPUB::f_fmt(x / 1e6, digits = 2, zero_fmt = zero_fmt),
    "pct" = GCAMCPUB::f_fmt_pct(x, digits = 2, zero_fmt = zero_fmt),
    "bp" = paste(GCAMCPUB::f_fmt(x * 10000, digits = 0, zero_fmt = zero_fmt), "BP")
  )
  if (escape) {
    stringr::str_replace_all(x, "\\%", "\\\\%")
  } else {
    x
  }
}


db_mo <- GCAMCPUB::activate_conn(db_mo)
nameseq = data.table(c('CNPC', 'Par', 'Life', 'Capital RMB',
                       'UV Individual', 'UV Group', 'Capital FX')) %>%
  setnames('Port_Name')
nameseq_UL <- data.table(c('UL Stable', 'UL Anyi', 'UL Increase', 'UL Strategy', 'UL Aggressive')) %>%
  setnames('Port_Name')
evaluation_baseline <- 0.045
CurrentDate <- to_date('2015-12-31')
DateFrom <- f_date_begin(CurrentDate, "year") - 1
DateTo <- f_date_end(CurrentDate, "year")
# ## former month
FormerDate <- f_date_begin(CurrentDate, "month") - 1
DateFrom_Former <- f_date_begin(FormerDate, "year") - 1
DateTo_Former <- f_date_end(FormerDate, "year")


##holding for cost based
sql <- "select Date, Port_Name, AV_Book_LC, AV_Mix_LC, UGL_LC, IAS, Class_L1, Class_L3,
Port_Type, Sec_Code, Sec_Name, YTM_Book, if_YTM_Ast, Maturity_Date_forYTM, if_Fund from 
CORE_Data_Holding where Date in('%s', '%s', '%s', '%s') and 
Port_Type in ('Cost Based', 'Cost Based FC')"
sql <- sprintf(sql, DateFrom, CurrentDate, DateFrom_Former, FormerDate)
holding <-
  DBI::dbGetQuery(db_mo, sql) %>%
  dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
  data.table::setDT() 

##CF external for cost based
sql <- "select Date, Port_Name,sum(CF_LC) as CF, Port_Type
from CORE_Data_CF_External where Date > '%s' and Date <= '%s'
and Port_Type in ('Cost based', 'Cost based FC')
group by Date, Port_Name, Port_Type
order by Date, Port_Name, Port_Type"
sql <- sprintf(sql, DateFrom_Former, CurrentDate)
CF <-
  DBI::dbGetQuery(db_mo, sql) %>%
  dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
  data.table::setDT() 
##PL
sql <- "select Date, Port_Name, PL_LC, Class_L1, Class_L3, PL_Type
from CORE_Data_PL where Port_Type in ('Cost Based', 'Cost Based FC') and  Date in ('%s', '%s')"
sql <- sprintf(sql, CurrentDate, FormerDate)
PL <-
  dbGetQuery(db_mo, sql) %>%
  dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
  data.table::setDT() %>%
  setkey("Port_Name")
PL <- PL[Port_Name != 'CNPC-UV', ]
##UGL
sql <- "select Date, Port_Name, sum(UGL_LC) as UGL
from CORE_Data_holding where Port_Type in ('Cost Based', 'Cost Based FC')
and Date = '%s'
group by Date, Port_Name"
sql <- sprintf(sql, CurrentDate)
UGL <-
  dbGetQuery(db_mo, sql) %>%
  dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
  data.table::setDT() 
UGL <- UGL[Port_Name != 'CNPC-UV',]
##target holding
if(CurrentDate >= to_date('2016-01-01')) {
  sql <- "select Date, Port_Name, Target from PortMgt_SAA where SAA_Class = 'Domestic listed equities' and
  Date = ( select max( date ) from PortMgt_SAA where Date>='%s' )"
  sql <- sprintf(sql, DateFrom)
  TargetHolding <-
    dbGetQuery(db_mo, sql) %>%
    dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
    data.table::setDT() %>%
    setkey(Port_Name)
} else {
  sql <- "select Date, Port_Name, Target from PortMgt_SAA where SAA_Class = 'listed equity' and
  Date = ( select max( date ) from PortMgt_SAA where Date>='%s' and Date < '2016-01-01' )"
  sql <- sprintf(sql, DateFrom)
  TargetHolding <-
    dbGetQuery(db_mo, sql) %>%
    dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
    data.table::setDT() %>%
    setkey(Port_Name)
}

##REquity Benchmark
sql <- "select Date, Bchmk_Name, Bchmk_Value from bchMK
where Bchmk_Code in ('000300', '000001') and Date in ('%s','%s')"
sql <- sprintf(sql, DateFrom, CurrentDate)
REquitybchmk <-
  dbGetQuery(db_mo, sql) %>%
  dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
  data.table::setDT() 
##Unit linked
#unitlinked holding
sql <- "select Date, UL_Name, UL_Code, UL_Class, Unit_Price
from unitPrice where Date in ('%s', '%s', '%s', '%s')
order by UL_Name, UL_Class, Date"
sql <- sprintf(sql, DateFrom, CurrentDate, FormerDate, DateFrom_Former)
UnitMarket <-
  dbGetQuery(db_mo, sql) %>%
  dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
  data.table::setDT() 
#unitlinked sum up AV_Mix_LC for each portfolio on each day
sql <- "select Date, Port_Name, sum(AV_Mix_LC) as AV_Mix
from CORE_data_holding where Date >= '%s' and Date <= '%s'
and Port_Type = 'Unit linked'
group by Date, Port_Name
order by Date, Port_Name"
sql <- sprintf(sql, DateFrom_Former, CurrentDate)
UnitAVMix <-
  dbGetQuery(db_mo, sql) %>%
  dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
  data.table::setDT() 
f_AVC_annual = function(InputDate){
  DateFrom <- f_date_begin(InputDate, "year") - 1
  AV_start <- holding[Port_Type %in% c('Cost based', 'Cost based FC') &
                        Date == DateFrom & Port_Name != "CNPC-UV",
                      list(AV_start = sum(AV_Book_LC)),
                      by = Port_Name] # sum up the AV_Book_LC group by Port_Name
  AV_start <- AV_start[nameseq, on = 'Port_Name'] # sort the 6 ports at the fixed sequence
  CF <- CF[Port_Name != 'CNPC-UV'& Date >= DateFrom & Date <= InputDate, ]
  TW_CF_annual <- CF[, CF_AVG := CF * as.numeric((DateTo - Date)) / 
                       as.numeric((DateTo - DateFrom))]
  TW_CF_annual <- TW_CF_annual[, list(CFavg_annual = sum(CF_AVG)), by = Port_Name] %>%
    setkey("Port_Name")
  AVC <- TW_CF_annual[AV_start]
  AVC <- AVC[, CFavg_annual := GCAMCPUB::na_fill(CFavg_annual, 0.0)]
  AVC <- AVC[, AVC_annual := AV_start + CFavg_annual]
  select(AVC, Port_Name, AVC_annual)
}
AVC_annual <- f_AVC_annual(CurrentDate)
AVC_annual_former <- f_AVC_annual(FormerDate)
# AVC_ytd = local({
#  AV_start <- holding[Port_Type %in% c('Cost based', 'Cost based FC') &
#                        Date == DateFrom & Port_Name != "CNPC-UV",
#                      list(AV_start = sum(AV_Book_LC)),
#                      by = Port_Name] # sum up the AV_Book_LC group by Port_Name
#  AV_start <- AV_start[nameseq, on = 'Port_Name'] # sort the 6 ports at the fixed sequence
#  CF <- CF[Port_Name != 'CNPC-UV', ]
#  TW_CF_ytd <- CF[, CF_AVG := CF * as.numeric((CurrentDate - Date)) / 
#                    as.numeric((CurrentDate - DateFrom))]
#  TW_CF_ytd <- TW_CF_ytd[, list(CFavg_ytd = sum(CF_AVG)), by = Port_Name] %>%
#    setkey("Port_Name")
#  AVC <- TW_CF_ytd[AV_start]
#  AVC <- AVC[, CFavg_ytd := GCAMCPUB::na_fill(CFavg_ytd, 0.0)]
#  AVC <- AVC[, AVC_ytd := AV_start + CFavg_ytd]
#  select(AVC, Port_Name, AVC_ytd)
# })
PL_sum <- PL[Date == CurrentDate, list(PL_LC = sum(PL_LC)), by = Port_Name]
PL_sum_former <- PL[Date == FormerDate, list(PL_LC = sum(PL_LC)), by = Port_Name]
AVC_PL <- PL_sum[AVC_annual, on = "Port_Name"]
AVC_PL_former <- PL_sum_former[AVC_annual_former, on = "Port_Name"]

####插入预测全年收益率的函数
####2 questions:
##1. the number of PL from Equity_investment should be considered firstly.
##2. if there is some value > 0 in the factual cost, should the costcoming be the base(0.002
##*AVC) or should it be - (0.002 * AVC + cost)?——choose the latter
yearreturn = function(table1, InputDate){
  yCash <- 0.035; yRepo <- 0.045; yGDPT <- 0.039; yZSJLCGQ <- 0.03
  DateFrom <- f_date_begin(InputDate, "year") - 1
  DateTo <- f_date_end(InputDate, "year")
  CurrentDate <- InputDate
  #the expected stable return
  table1 <- local({
    tmp <- select(holding, Date, Port_Name, Class_L1, Class_L3, Sec_Code, AV_Book_LC, YTM_Book, 
                  if_YTM_Ast, Maturity_Date_forYTM, UGL_LC, if_Fund)
    tmp <- tmp[Date == CurrentDate & Port_Name != "CNPC-UV", ]
    tmp <- tmp[, Maturity_Date_forYTM := to_date(tmp$Maturity_Date_forYTM)]
    tmp <- tmp[if_YTM_Ast == TRUE & Maturity_Date_forYTM > DateTo,
               YTM_PL := AV_Book_LC * ((1 + YTM_Book) ^ (as.numeric(DateTo - CurrentDate) / 365) - 1)]
    tmp <- tmp[if_YTM_Ast == TRUE & Maturity_Date_forYTM < DateTo,
               YTM_PL := AV_Book_LC * ((1 + YTM_Book) ^ (as.numeric(Maturity_Date_forYTM - 
                                                                      CurrentDate)/ 365) - 1)
               + AV_Book_LC * ((1 + YTM_Book) ^ (as.numeric(Maturity_Date_forYTM - 
                                                              CurrentDate)/ 365)) * 
                 ((1 + yCash) ^ (as.numeric(DateTo - Maturity_Date_forYTM) / 365) - 1)]
    tmp <- tmp[Class_L3 %in% c('Cash', 'MMF'), Cash :=
                 AV_Book_LC * ((1 + yCash) ^ (as.numeric(DateTo - CurrentDate) / 365) - 1)]
    tmp <- tmp[Class_L3 == "Repo", Repo := 
                 AV_Book_LC * ((1 + yRepo) ^ (as.numeric(DateTo - CurrentDate) / 365) -1)]
    #tmp <- tmp[Sec_Code == "GDPT.IB", Equity_investment := AV_Book_LC * yGDPT ]
    #tmp <- tmp[Sec_Code == "ZSJLCGQ.IB", Equity_investment := AV_Book_LC * yZSJLCGQ]
    tmp <- tmp[, Equity_investment := 0.0]
    tmp <- tmp[, YTM_PL := GCAMCPUB::na_fill(YTM_PL, 0.0)] 
    tmp <- tmp[, Cash := GCAMCPUB::na_fill(Cash, 0.0)]
    tmp <- tmp[, Repo := GCAMCPUB::na_fill(Repo, 0.0)]
    tmp <- tmp[, Equity_investment := GCAMCPUB::na_fill(Equity_investment, 0.0)]
    YTM_PL <- tmp[, list(YTM_PL = sum(YTM_PL)), by = Port_Name]
    Cash <- tmp[, list(Cash = sum(Cash)), by = Port_Name]
    Repo <- tmp[, list(Repo = sum(Repo)), by = Port_Name]
    Equity_investment <- tmp[, list(Equity_investment = sum(Equity_investment)), by = Port_Name]
    table1 <- YTM_PL[table1, on = 'Port_Name']
    table1 <- Cash[table1, on = 'Port_Name']
    table1 <- Repo[table1, on = 'Port_Name']
    table1 <- Equity_investment[table1, on = 'Port_Name']
    UGLTo <- holding[Date == CurrentDate & Port_Type %in% c('Cost based', 'Cost based FC') &
                       Port_Name != 'CNPC-UV' & (Class_L1 == 'Equity' | if_Fund == 1)
                     , ][, list(UGLTo = sum(UGL_LC)), by = Port_Name]
    table1 <- UGLTo[table1, on = 'Port_Name']
    table1
  })
  # the expected stable cost (20BP at least)
  cost_basepoint <- 0.002
  Cost <- local({
    tmp <- PL[Date == CurrentDate, ]
    suppressWarnings(
      tmp <- tmp[PL_Type %in% enc2utf8(c('其他业务支出','营业税金及附加','其他业务收入')),
                 list(Cost = sum(PL_LC)), by = Port_Name] )
    table1 <- tmp[table1, on = 'Port_Name']
    table1 <- table1[, Cost := GCAMCPUB::na_fill(Cost, 0.0)]
    if(substring(CurrentDate, 6, 10) != '12-31'){
      table1[Cost < 0 & abs(Cost) <= cost_basepoint * AVC_annual,
             CostComing := ((abs(Cost) - cost_basepoint * AVC_annual) - 
                              abs((abs(Cost) - cost_basepoint * AVC_annual))) / 2]
      table1[Cost < 0 & abs(Cost) > cost_basepoint * AVC_annual,
             CostComing := 0.0]
      table1[Cost >= 0, CostComing := - Cost -cost_basepoint * AVC_annual]
    } else {
      table1[, CostComing := 0.0]
    }
    table1
  })
  yearreturn <- select(Cost, Port_Name, AVC_annual, PL_LC, YTM_PL, Cash, Repo,
                       Equity_investment, CostComing, UGLTo)
  yearreturn <- yearreturn[, YR_Book := (PL_LC + YTM_PL + Cash + Repo + Equity_investment
                                         + CostComing) / AVC_annual]
  yearreturn <- yearreturn[, UGLplus := UGLTo / AVC_annual]
  yearreturn <- yearreturn[, YR_UGLplus := (PL_LC + YTM_PL + Cash + Repo + Equity_investment
                                            + CostComing + UGLTo) / AVC_annual]
  yearreturn
}
yearreturn_current <- yearreturn(AVC_PL, CurrentDate)
YR_Book_current <- select(yearreturn_current, Port_Name, AVC_annual, YR_Book) %>%
  setnames('YR_Book', 'YR_Book_current')
YR_Book_former <- select(yearreturn(AVC_PL_former, FormerDate), Port_Name, YR_Book) %>%
  setnames('YR_Book', 'YR_Book_former')
YR_diff <- YR_Book_current[YR_Book_former, on = "Port_Name"]
YR_diff <- YR_diff[, YR_diff := YR_Book_current - YR_Book_former]

## management fee

Contribution = data.table(
  c("CNPC", "Par", "Life", "Capital RMB", "Capital FX","UV Individual", "UV Group"),
  c(0.195, 0.3, 1, 1, 1, 1, 1)) %>%
  setnames(c("Port_Name", "Contribution")) %>%
  setkey("Port_Name")
proportionFun = function(x) {
  if ((x - 4.6 / 10000) <= 0.045) {
    0
  } else
    if((x - 4.6 / 10000) <= 0.0517) {
      ((x - 4.6 / 10000) - 0.045) * 0.15
    } else
      if ((x - 4.6 / 10000) <= 0.055){
        ((x - 4.6 / 10000) - 0.0517) * 0.2 + 0.0067 * 0.15
      } else
        if ((x - 4.6 / 10000) <= 0.06) {
          ((x - 4.6 / 10000) - 0.055) * 0.25 + 0.0033 * 0.2 + 0.0067 * 0.15
        } else {
          ((x - 4.6 / 10000) - 0.06) * 0.3 + 0.005 * 0.25 + 0.0033 * 0.2 + 0.0067 * 0.15
        }
}
YR_Book <- local({
  YR_Book <- select(yearreturn_current, Port_Name, AVC_annual, YR_Book)
  YR_Book <- YR_Book[, profit_proportion := sapply(YR_Book, proportionFun)]
  YR_Book <- Contribution[YR_Book, on = "Port_Name"]
  YR_Book <- YR_Book[, Fee_Book := AVC_annual * profit_proportion * Contribution]
  YR_Book <- select(YR_Book, Port_Name, AVC_annual, YR_Book, Fee_Book)
  YR_Book <- YR_Book[nameseq, on = 'Port_Name']
  YR_Book
})
YR_UGLplus <- local({
  YR_UGLplus <- select(yearreturn_current, Port_Name, AVC_annual, YR_UGLplus)
  YR_UGLplus <- YR_UGLplus[, profit_proportion_UGLplus := sapply(YR_UGLplus, proportionFun)]
  YR_UGLplus <- Contribution[YR_UGLplus, on = "Port_Name"]
  YR_UGLplus <- YR_UGLplus[, Fee_UGLplus := AVC_annual * profit_proportion_UGLplus * Contribution]
  YR_UGLplus <- select(YR_UGLplus, Port_Name, AVC_annual, YR_UGLplus, Fee_UGLplus)
  YR_UGLplus <- YR_UGLplus[nameseq, on = 'Port_Name']
  YR_UGLplus
})


TargetHolding <- local({
  PLequity <- PL[Class_L1 == 'Equity' & Date == CurrentDate, .(PL_equity = sum(PL_LC)),
                 by = Port_Name] 
  holding <- holding[Port_Type %in% c('Cost based', 'Cost based FC') &
                       Port_Name != "CNPC-UV" ]
  UGLTo <- holding[Date == CurrentDate & Class_L1 == 'Equity' & IAS == "AFS",
                   .(UGLTo = sum(UGL_LC)), by = Port_Name]
  TargetHolding <- AVC_annual[TargetHolding, on = 'Port_Name'] %>%
    select(Port_Name, AVC_annual, Target)
  TargetHolding <- TargetHolding[, Targetshare := Target * AVC_annual]
  TargetHolding <- UGLTo[TargetHolding, on = "Port_Name"]
  TargetHolding <- PLequity[TargetHolding, on = "Port_Name"]
  TargetHolding <- TargetHolding[, REquity := (PL_equity + UGLTo) / Targetshare]
  TargetHolding
})
REquity <- select(TargetHolding, Port_Name, REquity)
Cost_based_fee <- YR_Book[YR_UGLplus, on = 'Port_Name']
Cost_based_fee <- REquity[Cost_based_fee, on = 'Port_Name']
Cost_based_fee <- select(Cost_based_fee, Port_Name, AVC_annual, REquity, YR_Book, Fee_Book, 
                         YR_UGLplus, Fee_UGLplus)
Cost_based_fee <- Cost_based_fee[nameseq, on = 'Port_Name']

## so far, Cost_based_fee is the final data for each Port of the cost based, then we need to 
## add some total info and give an easy-reading version of the result
AVC_annual_sum <- sum(Cost_based_fee$AVC_annual)
REquity_sum <- (sum(TargetHolding$PL_equity) + sum(TargetHolding$UGLTo)) / sum(TargetHolding$Targetshare)

PL_Sum <- yearreturn(AVC_PL, CurrentDate)[, .(PL_sum = sum(PL_LC), YTM_PL_sum = sum(YTM_PL),
                                              Cash_sum = sum(Cash), Repo_sum = sum(Repo),
                                              Equity_invst_sum = sum(Equity_investment),
                                              CostComing_sum = sum(CostComing), UGLTo_sum = sum(UGLTo)
)]
PL_Sum <- PL_Sum[, YR_Book := (PL_sum + YTM_PL_sum + Cash_sum + Repo_sum +
                                 Equity_invst_sum + CostComing_sum) / AVC_annual_sum]
PL_Sum <- PL_Sum[, YR_UGLplus := (PL_sum + YTM_PL_sum + Cash_sum + Repo_sum +
                                    Equity_invst_sum + CostComing_sum + UGLTo_sum) / AVC_annual_sum]
cost_sumup <- data.table(
  matrix(c('总计', AVC_annual_sum, REquity_sum, PL_Sum$YR_Book,
           sum(YR_Book$Fee_Book),PL_Sum$YR_UGLplus, sum(YR_UGLplus$Fee_UGLplus)),
         ncol = 7, nrow = 1, byrow = TRUE)) %>%
  setnames(colnames(Cost_based_fee))
Cost_based_fee_show <- rbind(Cost_based_fee, cost_sumup)
##change the format of cost_based_fee_show use fmting function
Cost_based_fee_show <- Cost_based_fee_show %>%
  purrr::map_at(2:7, as.numeric) %>%
  purrr::map_at(c(2, 5, 7), fmting, unit = "mn") %>%
  purrr::map_at(c(3, 4, 6), fmting, unit = "pct") %>%
  as.data.frame() %>%
  setDT
colnames(Cost_based_fee_show) <- c(" ", "平均成本", "权益调整项收益率", "预测账面收益率", 
                                   "绩效管理费（账面）", "预测收益率（+UGL）", "绩效管理费（+UGL）")
#Equity Adjusted Index
AdjustedIndex <- local({
  AdjustedIndexFun = function(x, y){
    if (x > 0 & y > 0){
      10 * min (x, y, 0.05)
    } else {
      if (x < 0 & y < 0){
        10 * (0.0075 + max(x, y, -0.0575))
      } else
        0
    }
  }
  REbchmk <- REquitybchmk[Date == CurrentDate & Bchmk_Name == '沪深300', ]$Bchmk_Value /
    REquitybchmk[Date == DateFrom & Bchmk_Name == '沪深300', ]$Bchmk_Value - 1
  REbchmk <- min(0.35, max(REbchmk, -0.15))
  EREquity <- REquity_sum - REbchmk
  AdjustedIndexFun(REquity_sum, EREquity)
})
Limit <- local({
  tmp <- YR_Book[YR_UGLplus, on = c('Port_Name', 'AVC_annual')]
  tmp <- Contribution[tmp, on = 'Port_Name']
  tmp <- tmp[, Excess_Book := (YR_Book - evaluation_baseline - 4.6 / 10000) * AVC_annual * Contribution]
  tmp <- tmp[, Excess_UGLplus := (YR_UGLplus- evaluation_baseline - 4.6 / 10000) * AVC_annual *
               Contribution]
  limit_Book <- 0.3 * sum(tmp$Excess_Book)
  limit_UGLplus <- 0.3 * sum(tmp$Excess_UGLplus)
  tmp <- data.table(matrix(c(limit_Book, limit_UGLplus), 1, 2)) %>%
    setnames(c('Limit_Book', 'Limit_UGLplus'))
  tmp
})
Cost_based_fee_Book <-  sum(YR_Book$Fee_Book)
Cost_based_fee_UGLplus <- sum(YR_UGLplus$Fee_UGLplus)
Cost_based_fee_Book_adj <- min(Cost_based_fee_Book * (1 + AdjustedIndex),
                               Limit$Limit_Book)
Cost_based_fee_UGLplus_adj <- min(Cost_based_fee_UGLplus * (1 + AdjustedIndex),
                                  Limit$Limit_UGLplus)

#####Port Unit Linked
ULMatchTable = data.table(
  c("UL Stable", "UL Anyi", "UL Aggressive","UL Increase", "UL Strategy"),
  c("76300126", "76300158", "76300157", "76300127", "76300156"),
  c("中意稳健理财投资账户", "中意安逸稳健投资账户", "中意积极进取投资账户",
    "中意增长理财投资账户", "中意策略增长投资账户")) %>%
  setnames(c("Port_Name", "UL_Code", "UL_Name"))
UnitFeeRatio = function(x) {
  if (x > 0.5) {
    0
  } else {
    if (x >= 0.1 & x <= 0.5) {
      (-0.5) * x + 0.25
    } else {
      0.2
    }
  }
}
UnitMarket_curr <- UnitMarket[Date %in% c(CurrentDate, DateFrom),
                              Yield := Unit_Price / shift(Unit_Price, 1L) - 1, by = UL_Name]
UnitMarket_curr <- UnitMarket_curr[Date == CurrentDate, ] %>%
  setkey(UL_Class)
Market_Number_curr <- data.table(c("Equity", "Balanced", "Bond"),
                                 c(UnitMarket_curr[UL_Class == 'Equity', .N],
                                   UnitMarket_curr[UL_Class == 'Balanced', .N],
                                   UnitMarket_curr[UL_Class == 'Bond', .N])) %>%
  setnames(c("UL_Class", "Number")) %>%
  setkey(UL_Class)
UnitMarket_curr <- Market_Number_curr[UnitMarket_curr]
UnitMarket_curr <- UnitMarket_curr[, Rank := Number - rank(Yield) + 1, by = UL_Class]
Unitholding_curr <- UnitMarket_curr[UL_Code %in% ULMatchTable$UL_Code, ]
Unitholding_curr[, proportion := Rank / Number]
Unitholding_curr <- Unitholding_curr[, Ratio := sapply(proportion, UnitFeeRatio)]
#scale of asset AV_Mix_Lc
AV_Mix_avg_curr <- UnitAVMix[Date >= DateFrom, list(AV_Mix_avg = mean(AV_Mix)), by = Port_Name]
Unitholding_curr <- ULMatchTable[Unitholding_curr, on = "UL_Code"]
Unitholding_curr <- AV_Mix_avg_curr[Unitholding_curr, on = "Port_Name"] %>%
  select(Port_Name, UL_Code, Rank, Yield, Number,proportion, Ratio, AV_Mix_avg)
Unitholding_curr <- Unitholding_curr[, Fee := AV_Mix_avg * Ratio / 100]
Unitholding_curr <- Unitholding_curr[, Rank_paste := paste(Rank, "/", Number, sep = "")]
Unitholding_curr <- Unitholding_curr[nameseq_UL, on = "Port_Name"]
Unit_linked_fee <- select(Unitholding_curr, Port_Name, AV_Mix_avg, Rank_paste, proportion, Fee)
Unit_sumup <- data.table(
  matrix(c('总计', sum(AV_Mix_avg_curr$AV_Mix_avg), NA, NA, sum(Unit_linked_fee$Fee)),
         ncol = 5, nrow = 1)
) %>%
  setnames(colnames(Unit_linked_fee))
Unit_linked_fee_show <- rbind(Unit_linked_fee, Unit_sumup)
Unit_linked_fee_show <- Unit_linked_fee_show %>%
  purrr::map_at(c(2, 4, 5), as.numeric) %>%
  purrr::map_at(2, fmting, 'mn') %>%
  purrr::map_at(4, fmting, 'pct') %>%
  as.data.frame() %>%
  setDT
colnames(Unit_linked_fee_show) <- c(" ", "平均资产", "最新排名", "排名百分比", "预计绩效管理费")
YR_hs300 <- REquitybchmk[Date == CurrentDate & Bchmk_Name == '沪深300', ]$Bchmk_Value /
  REquitybchmk[Date == DateFrom & Bchmk_Name == '沪深300', ]$Bchmk_Value - 1

## show cost based portfolios' performance matrix
##组合管理跟踪简报——人民币成本组合
YR_budget <- data.table(matrix(c(nameseq[Port_Name != 'Capital FX', ]$Port_Name, 
                                 0.0517, 0.0496, 0.0559, 0.0524, 0.0495, 0.0539),6,2, byrow = F)) %>%
  setnames(c("Port_Name", "YR_budget"))
perfmce_cb <- select(TargetHolding, Port_Name, Targetshare, REquity)[YR_diff, on = "Port_Name"]
perfmce_cb <- select(yearreturn_current, Port_Name, UGLplus, 
                     YR_UGLplus)[perfmce_cb, on = "Port_Name"] %>%
  select(Port_Name, AVC_annual, YR_Book_current, YR_diff, UGLplus, YR_UGLplus, REquity, Targetshare)
perfmce_cb <- YR_budget[perfmce_cb, on = "Port_Name"]
perfmce_cb <- perfmce_cb[nameseq, on = "Port_Name"]
perfmce_cb[, YR_hs300 := YR_hs300]
perfmce_cb <- perfmce_cb[Port_Name != "Capital FX", ]
perfmce_cb <- select(perfmce_cb, Port_Name, AVC_annual, YR_Book_current, YR_diff,
                     UGLplus, YR_UGLplus, YR_budget, REquity, YR_hs300) 
##the final sum up col
Total <- data.table(local({
  Total_AVC_annual <- sum(AVC_annual[Port_Name != 'Capital FX', ]$AVC_annual)
  Total_YR_Book <- (sum(AVC_PL[Port_Name != 'Capital FX', ]$PL) /
                      sum(TargetHolding[Port_Name != 'Capital FX', ]$AVC_annual))
  Total_YR_Diff <- (Total_YR_Book - sum(AVC_PL_former[Port_Name != 'Capital FX', ]$PL_LC) /
                      sum(AVC_annual_former[Port_Name != 'Capital FX', ]$AVC_annual))
  Total_UGLplus <- sum(yearreturn_current[Port_Name != 'Capital FX', ]$UGLTo) /
    Total_AVC_annual
  Total_YR_UGLplus <- sum(perfmce_cb[Port_Name != 'Capital FX', ]$YR_UGLplus *
                            perfmce_cb[Port_Name != 'Capital FX', ]$AVC_annual) /
    sum(TargetHolding[Port_Name != 'Capital FX', ]$AVC_annual)
  Total_YR_Budget <- 0.0517
  Total_REquity <- (sum(TargetHolding$PL_equity) + sum(TargetHolding$UGLTo)) / sum(TargetHolding$Targetshare)
  Total_hs300 <- 0.0558
  t(c('Total', Total_AVC_annual, Total_YR_Book, Total_YR_Diff, Total_UGLplus, Total_YR_UGLplus,
      Total_YR_Budget, Total_REquity, Total_hs300))
}))
## combine with the sum-up and adjust the numbers format
colnames(Total) <- colnames(perfmce_cb)
perfmce_cb <- perfmce_cb %>%
  purrr::map_at(7, as.numeric) %>%
  purrr::map_at(2, fmting, unit = "thousand") %>%
  purrr::map_at(3:9, fmting, unit = "pct") %>%
  setDT
Total <- Total %>%
  purrr::map_at(2:9, as.numeric) %>%
  purrr::map_at(2, fmting, unit = "thousand") %>%
  purrr::map_at(3:9, fmting, unit = "pct") %>%
  setDT()
perfmce_cb <- rbind(perfmce_cb, Total)
perfmce_cb.t <- t(perfmce_cb)
colnames(perfmce_cb.t) <- perfmce_cb.t[1, ]
perfmce_cb.t <- perfmce_cb.t[-1, ]
perfmce_cb.t <- cbind(c('年化平均成本', '年化账面收益率', '收益率变化（上月底）', 
                        '权益及基金UGL', '账面收益率(+UGL)', '预算收益率', 
                        '权益调整项收益率', '沪深300收益率'), perfmce_cb.t) 
colnames(perfmce_cb.t)[1] <- ' '
rownames(perfmce_cb.t) <- NULL
shIndex <- REquitybchmk[Bchmk_Name == '上证综指' & Date == CurrentDate]$Bchmk_Value %>%
  f_fmt(, digits = 2)
# perfmce_cb.t <- perfmce_cb.t %>%
#   data.table %>%
#   purrr::map_at(2:8, as.numeric) %>%
#   purrr::map_at(2:8, fmting, unit = "pct") %>%
#   setDT


##show unit linked porfolios' performance 
##组合管理跟踪简报—投连组合
##current month data stored in UnitMarkte_curr
Unitholding_curr <- local({
  Market_median <- data.table(c("Equity", "Balanced", "Bond"),
                              c(UnitMarket_curr[UL_Class == 'Equity', .(median = median(Yield))],
                                UnitMarket_curr[UL_Class == 'Balanced', .(median = median(Yield))],
                                UnitMarket_curr[UL_Class == 'Bond', .(median = median(Yield))])) %>%
    setnames(c("UL_Class", "Median")) 
  tmp <- UnitMarket_curr[UL_Code %in% ULMatchTable$UL_Code, ]
  tmp <- ULMatchTable[tmp, on = "UL_Code"]
  tmp <- Market_median[tmp, on = "UL_Class"]
  tmp <- tmp[, Rank_paste_curr := paste(Rank, "/", Number, sep = "")]  %>%
    select(Port_Name, Yield, Median, Rank_paste_curr)
  tmp[, Median := as.numeric(Median)]
  tmp
})
Unitholding_former <- local({
  tmp <- UnitMarket[Date %in% c(FormerDate, DateFrom_Former),
                    Yield := Unit_Price / shift(Unit_Price, 1L) - 1, by = UL_Name]
  tmp <- tmp[Date == FormerDate, ] %>%
    setkey(UL_Class)
  Market_Number_former <- data.table(c("Equity", "Balanced", "Bond"),
                                     c(tmp[UL_Class == 'Equity', .N],
                                       tmp[UL_Class == 'Balanced', .N],
                                       tmp[UL_Class == 'Bond', .N])) %>%
    setnames(c("UL_Class", "Number")) %>%
    setkey(UL_Class)
  tmp <- Market_Number_former[tmp]
  tmp <- tmp[, Rank := Number - rank(Yield) + 1, by = UL_Class]
  tmp <- tmp[UL_Code %in% ULMatchTable$UL_Code, ]
  tmp <- ULMatchTable[tmp, on = "UL_Code"] 
  tmp <- tmp[, Rank_paste_former := paste(Rank, "/", Number, sep = "")] %>%
    select(Port_Name, Rank_paste_former)
  tmp
})
perfmce_ul <- Unitholding_former[Unitholding_curr, on = "Port_Name"]
perfmce_ul <- perfmce_ul[nameseq_UL, on = "Port_Name"] %>%
  select(Port_Name, Rank_paste_curr, Rank_paste_former, Yield, Median)
perfmce_ul[, Yield := f_fmt_pct(Yield, d = 2, "-")]
perfmce_ul[, Median := f_fmt_pct(Median, d = 2, "-")]
perfmce_ul.t <- t(perfmce_ul)[-1, ]
colnames(perfmce_ul.t) <- perfmce_ul$Port_Name
perfmce_ul.t <- cbind(c('当前排名', '上月排名', '收益率', '中位收益率'), perfmce_ul.t)
colnames(perfmce_ul.t)[1] <- " "
rownames(perfmce_ul.t) <- NULL

f_rnw2pdf("E:/RWD/RTest/Rcode/portMgtTrakingRpt.Rnw",
          "C:/Users/AMC161/Desktop/portMgtTrakingRpt.pdf", if_show = T)
f_rnw2pdf("E:/RWD/RTest/Rcode/mgtFeeForecast.Rnw",
          "C:/Users/AMC161/Desktop/mgtFeeForecast.pdf")

