# pkgs --------------------------------------------------------------------

#coreQuery(c("pos", "pl", "trans"), "CNPC", 
#as.Date(c("2016-08-01", "2016-08-05")), "Stock")
# github address:
# https://github.com/GCAMC/PortMgt/blob/master/report_to_Top_Mgt/report_to_Top_Mgt_v1.0.Rnw
# the equity investment computation suits for dates after 2015-12-28
lapply(list("GCAMCPUB", "data.table", "dplyr", "portanalytics", "xtable"),
       library, ch = TRUE)
#library(sca) # to show data in percent style
# 纠正格式的时候data.table要copy才能保证原来的不变
# reading data-------------------------------------------------------------

setLdcPath(ldcPathDefault)
portanalytics::updateLdc("201609", "All company")
portanalytics::readFromLdc(c("201412", "201609"), "All company")
pos <- dataCenter$pos[HS_Port_Code %in% portSelector("Cost based") &
                        !HS_Port_Code %in% portSelector("CNPC-UV"), ]
pos[, `:=`(Port_Name = coreInfo$read("HSPortInfo")[J(pos$HS_Port_Code), Port_Name],
           Port_Type = coreInfo$read("HSPortInfo")[J(pos$HS_Port_Code), Port_Type])]
pl <- dataCenter$pl[HS_Port_Code %in% portSelector("Cost based") &
                      !HS_Port_Code %in% portSelector("CNPC-UV"), ]
pl[, `:=`(Port_Name = coreInfo$read("HSPortInfo")[J(pl$HS_Port_Code), Port_Name],
          Port_Type = coreInfo$read("HSPortInfo")[J(pl$HS_Port_Code), Port_Type])]


#portSelector(paPorts(as.list(portNames$Port_Name), portNames$Port_Name))


# para setting ------------------------------------------------------------

dateCurrent <- as.Date('2016-09-21')
dateFrom <- f_date_begin(dateCurrent, "year") - 1
dateTo <- f_date_end(dateCurrent, "year")
#define function
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
source('E:/RWD/RTest/RCode/rtnForecast.R', encoding = 'UTF-8')

# table1:cost based -------------------------------------------------------

portOrder <- data.table(Item = c("CNPC", "Par", "Life", "Capital RMB",
                               "UV Individual", "UV Group", "Total"))
ports <- paPorts(list("CNPC", "Par", "Life", "Capital RMB",
                      "UV Individual", "UV Group",
                      c("CNPC", "Par", "Life", "Capital RMB",
                        "UV Individual", "UV Group")),
                 names = c("CNPC", "Par", "Life", "Capital RMB",
                           "UV Individual", "UV Group", "Total"))
costBased <- dietzRtn(ports = ports, dates = c(dateFrom, dateCurrent), ts = FALSE) 
if(month(dateCurrent) != 1){
  costBasedFormer <- dietzRtn(ports = ports, 
                              dates =c(dateFrom, f_date_begin(dateCurrent, "month") - 1),
                              ts = FALSE) %>%
    select(Item, PL_Former = PL)
  costBased <- costBasedFormer[costBased, on = "Item"]
  costBased[, Yield_Delta := (PL - PL_Former) / AVC]
} else{
  costBased[, Yield_Delta := Book_Rtn]
}
equityRtn <- equityAdjRtn(ports = ports,
                          dates = c(dateFrom, dateCurrent),
                          ts = FALSE,
                          avcType = c("Annual")) %>%
  select(Item, Equity_Rtn = Rtn, Bmk_HS300 = Bmk_Rtn_Dom)
budgetRtn <- data.table(Item = portOrder$Item, Budget_Rtn = c(
  0.0517, 0.0496, 0.0559, 0.0524, 0.0495, 0.0539, 0.0517))
costBased <- equityRtn[budgetRtn, on = "Item"][costBased, on = "Item"]
# annual return -----------------------------------------------------------

avcAnnual <- avc(ports, dates = c(dateFrom, dateTo), ts = FALSE) %>%
  select(Item, AVC_Annual = AVC)
# the coming pl prediction
plFcst <- local({
  posFcst <- pos[Date == dateCurrent, .(Date, HS_Port_Code, Class_L3_, Sec_Code,
                                        AV_Book_LC, YTM_Mix, Maturity_Date, 
                                        Port_Name, Port_Type)]
  pl <- pl[Date > dateFrom & Date <= dateCurrent, .(Date, HS_Port_Code, Class_L3_,
                                                    Sec_Code, PL_LC, IAS, PL_Type,
                                                    Port_Name, Port_Type)]
  pl <- pl[, .(PL_LC = sum(PL_LC)), by = .(HS_Port_Code, Class_L3_, Sec_Code, 
                                           PL_Type, Port_Name, Port_Type)]
  tmp <- pl[posFcst, on = c("HS_Port_Code", "Class_L3_", "Sec_Code", 
                            "Port_Name", "Port_Type")]
  tmp[is.na(PL_LC), PL_LC := 0.0]
  tmp[, Maturity_Date := to_date(tmp$Maturity_Date)]
  # plComing from YTM Ast
  tmp[!is.na(YTM_Mix) & !Class_L3_ %in% c("Repo", "Reverse Repo") &
        Maturity_Date > dateTo,
      YTM_PL := AV_Book_LC *
        ((1 + YTM_Mix) ^ (as.numeric(dateTo - dateCurrent) / 365) - 1)]
  tmp[!is.na(YTM_Mix) & !Class_L3_ %in% c("Repo", "Reverse Repo") &
        Maturity_Date < dateTo,
      YTM_PL := AV_Book_LC * ((1 + YTM_Mix) ^ 
                        (as.numeric(Maturity_Date - dateCurrent)/ 365)) *
        ((1 + yCash) ^ (as.numeric(dateTo - Maturity_Date) / 365) - 1)]
  # plComing from Cash/MMF/Repo/Reverse Repo/Pure bond fund
  tmp[Class_L3_ %in% c("Cash", "MMF"), Cash :=
        AV_Book_LC * ((1 + yCash) ^ (as.numeric(dateTo - dateCurrent) / 365) - 1)]
  tmp[Class_L3_ %in% c("Repo", "Reverse Repo"), Repo :=
        AV_Book_LC * ((1 + yRepo) ^ (as.numeric(dateTo - dateCurrent) / 365) -1)]
  tmp[Class_L3_ == "Bond fund", Bond_Fund :=
        AV_Book_LC *
        ((1 + bondFund) ^ (as.numeric(dateTo - dateCurrent) / 365) - 1)]
  # plComing from Equity Investment
  tmp[Sec_Code == "GDPT.IB",
      Equity_Investment := AV_Book_LC * yGDPT - PL_LC]
  tmp[Sec_Code == "ZSJLCGQ.IB",
      Equity_Investment := AV_Book_LC * yZSJLCGQ - PL_LC]
  tmp[, `:=`(YTM_PL = GCAMCPUB::na_fill(YTM_PL, 0.0),
             Cash = GCAMCPUB::na_fill(Cash, 0.0),
             Repo = GCAMCPUB::na_fill(Repo, 0.0),
             Bond_Fund = GCAMCPUB::na_fill(Bond_Fund, 0.0),
             Equity_Investment = GCAMCPUB::na_fill(Equity_Investment, 0.0))]
  # sum up total plComing
  if(ncol(tmp) == 6){###########need revised
    tmp <- tmp[, PL_Coming := YTM_PL + Cash + Repo + 
                 Bond_Fund + Equity_Investment]
  } else{
    tmp <- tmp[, PL_Coming := YTM_PL + Cash + Bond_Fund + Equity_Investment]
  }
 
  tmp[, .(PL_Coming = sum(PL_Coming)), by = "Port_Name"] %>%
    setnames("Port_Name", "Item")
  })
# the expected stable cost (20BP at least)
costFcst <- local({
  pl <- pl[Date > dateFrom & Date <= dateCurrent, .(Date, HS_Port_Code, Class_L3_,
                                                    Sec_Code, PL_LC, IAS, PL_Type,
                                                    Port_Name, Port_Type)]
  tmp <- pl[, .(PL_LC = sum(PL_LC)), by = .(HS_Port_Code, Class_L3_, Sec_Code, 
                                            PL_Type, Port_Name, Port_Type)]
  tmp <- tmp[PL_Type %in% c('其他业务支出','营业税金及附加','其他业务收入'),
             .(Cost = sum(PL_LC)), by = .(Port_Name)] %>%
    setnames("Port_Name", "Item")
  tmp <- tmp[avcAnnual, on = "Item"]
  tmp[, Cost := GCAMCPUB::na_fill(Cost, 0.0)]
  if(substring(dateCurrent, 6, 10) != '12-31'){
    tmp[Cost < 0,
        Cost_Coming := ((abs(Cost) - costBP * AVC_Annual) - 
                          abs((abs(Cost) - costBP * AVC_Annual))) / 2]
    tmp[Cost >= 0, Cost_Coming := - Cost - costBP * AVC_Annual]
  } else {
    tmp[, Cost_Coming := 0.0]
  }
  tmp
})
plrealized <- costBased[, .(Item, PL), on = "Item"]
rtnAnnual <- plFcst[costFcst, on = "Item"]
rtnAnnual <- plrealized[rtnAnnual, on = "Item"]
# deal with the total cost based port
rtnAnnual[Item == "Total", ]$PL_Coming <- 
  sum(rtnAnnual[Item != "Total"]$PL_Coming)
rtnAnnual[Item == "Total", ]$Cost <- 
  sum(rtnAnnual[Item != "Total"]$Cost)
rtnAnnual[Item == "Total", ]$Cost_Coming <- 
  sum(rtnAnnual[Item != "Total"]$Cost_Coming)
rtnAnnual <- rtnAnnual[, Book_Annual := (PL + PL_Coming + Cost_Coming) / AVC_Annual] %>%
  select(Item, Book_Annual)
costBased <- rtnAnnual[costBased, on = "Item"] %>%
  select(Item, AVC, YTD = Book_Rtn, Yield_Delta,
         Comp_Rtn, Comp_Rtn_Excl_Bond, Budget_Rtn, Book_Annual, Equity_Rtn, Bmk_HS300,
         Realizable_Book_Rtn_Floor, Realizable_Book_Rtn_Ceiling, Realizable_Book_Rtn)
costBased <- costBased[portOrder, on = "Item"]


shIndex <- dataCenter$bchmk[
  Date == dateCurrent & Bchmk_Code == "000001.SH", ]$Bchmk_Value %>%
  GCAMCPUB::f_fmt(digits = 2)
# table2: Capital FX  --------------------------------------------------------

pos <- dataCenter$pos[HS_Port_Code %in% portSelector("Capital FX"), ]

ytd <- dietzRtn(paPorts(list("Capital FX"), "Capital FX"), 
                dates = c(dateFrom, dateCurrent),
                ts = F) %>%
  select (Port_Name = Item, PL, Book_Rtn, AVC)

annual <- rtnForecast("Capital FX", dateCurrent, "Book") %>%
  select(Port_Name, PL_LC, PL_Coming, Cost_Coming, AVC, Rtn_Fcst)
tmp <- ytd[annual, on = "Port_Name"]
tmp[, `:=`(
  Book_Excl_Ex = (PL_LC - sum(pl[PL_Type == "汇兑损益", ]$PL_LC)) / AVC,
  Annual_Excl_Ex = (PL_LC + PL_Coming + Cost_Coming - 
                      sum(pl[PL_Type == "汇兑损益", ]$PL_LC)) / AVC)]
capitalFX <- data.table(
  " " = c("YTD", "年化"),
  Excl_Exchange = c(tmp$Book_Excl_Ex, tmp$Annual_Excl_Ex),
  Incl_Exchange = c(tmp$Book_Rtn, tmp$Rtn_Fcst)) %>%
  setnames(c(" ", "不包含汇兑损益", "包含汇兑损益"))

# table3: Unit linked --------------------------------------------------------

rank <- ulRank(c(dateFrom, dateCurrent), ts = FALSE, rtnAll = FALSE, probs = 0.5) %>%
  setnames("50%", "Median") %>%
  select(Item, Cum_Rtn, Median, Rank, N)
rank <- rank[, Rank := paste0(Rank, "/", N)] %>%
  select(-N)
if(month(dateCurrent) == 1){
  rankFormer <- ulRank(
    c(f_date_begin(f_date_begin(dateCurrent, "month") - 1, "year") - 1,
      f_date_end(f_date_begin(dateCurrent, "month") - 1, "year")),
    ts = FALSE, rtnAll = FALSE, probs = 0.5
  ) %>%
    select(Item, Rank, N)
  rankFormer <- rankFormer[, Rank := paste0(Rank, "/", N)] %>%
    select(-N, -Rank)
} else{
  rankFormer <- ulRank(
    c(dateFrom, f_date_begin(dateCurrent, "month") - 1),
    ts = FALSE, rtnAll = FALSE, probs = 0.5) %>%
    select(Item, Rank, N)
  rankFormer <- rankFormer[, Rank_Former := paste0(Rank, "/", N)] %>%
    select(-N, -Rank)
}
rank <- rankFormer[rank, on = "Item"] %>%
  select(Item, Rank, Rank_Former, Cum_Rtn, Median)


# table4: third party -----------------------------------------------------

ports <- paPorts(list("AMC Capital"), "AMC Capital")
amcCapital <- dietzRtn(ports, dates = c(dateFrom, dateTo), ts = F) %>%
  select(Item, Book_Rtn, Comp_Rtn)
amcAnnual <- data.table(Item = "AMC Capital",
                        Book_Rtn = rtnForecast("AMC Capital", dateCurrent, "Book")$Rtn_Fcst,
                        Comp_Rtn = rtnForecast("AMC Capital", dateCurrent, "Comp")$Rtn_Fcst)
amcCapital <- rbind(amcCapital, amcAnnual) %>%
  select(-Item)

# table 5: AMC capital ----------------------------------------------------



f_rnw2pdf("E:/RWD/RTest/Rcode/portMgtTrakingRpt.Rnw",
          "C:/Users/AMC161/Desktop/portMgtTrakingRpt.pdf", if_show = T)

f_rnw2pdf("E:/RWD/RTest/Rcode/portMgtTrakingRpt.Rnw",
          "C:/Users/AMC161/Desktop/portMgtTrakingRpt.pdf", if_show = T,
          eval_in_temp = TRUE)
f_rnw2pdf("E:/RWD/RTest/Rcode/mgtFeeReport.Rnw",
          "C:/Users/AMC161/Desktop/mgtFeeReport.pdf", if_show = T,
          eval_in_temp = TRUE)


