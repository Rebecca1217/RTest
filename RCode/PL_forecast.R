# The result of this file is predicted PL for the coming year
# by Class_L3 or by Port_Name
# You can use it in the annualised return forecasting and other forecasting cases
# CNPC-UV port is not included yet
# pkgs------------------------------------------------------------------------------------
lapply(list("GCAMCPUB", "data.table", "dplyr", "portanalytics"), 
       library, ch = TRUE)
# para setting----------------------------------------------------------------------------
inputDate <- to_date('2016-08-18')
yCash <- 0.020; yRepo <- 0.025; yGDPT <- 0.039; yZSJLCGQ <- 0.03; bondFund <- 0.045

#db_mo <- GCAMCPUB::activate_conn(db_mo)
dateFrom <- f_date_begin(inputDate, "year") - 1
dateTo <- f_date_end(inputDate, "year")
# read data-------------------------------------------------------------------------------

setLdcPath(ldcPathDefault)
portanalytics::updateLdc("201608", "All company")
#只预测接下来的话只需要读取当个月的数据
portanalytics::readFromLdc(c(paste0(year(inputDate), "01"), 
                             paste0(year(inputDate), substr(inputDate, 6, 7))), 
                           "All company")

pos <- dataCenter$pos %>%
  select(Date, HS_Port_Code, Class_L3, AV_Book_LC, Sec_Code, YTM_Mix, 
         Maturity_Date) 
pos[, `:=`(Port_Name = coreInfo$read("HSPortInfo")[J(pos$HS_Port_Code), Port_Name],
           Port_Type = coreInfo$read("HSPortInfo")[J(pos$HS_Port_Code), Port_Type])]
pos <- pos[Port_Name != "CNPC-UV" & Date == inputDate, ]
pl <- dataCenter$pl %>%
  select(Date, HS_Port_Code, Sec_Code, PL_LC, Class_L3, PL_Type)
pl[, `:=`(Port_Name = coreInfo$read("HSPortInfo")[J(pl$HS_Port_Code), Port_Name],
          Port_Type = coreInfo$read("HSPortInfo")[J(pl$HS_Port_Code), Port_Type])]
pl <- pl[Port_Name != "CNPC-UV", ]
pl <- pl[, .(PL_LC = sum(PL_LC)), by = .(Date, Sec_Code, Port_Name, Class_L3, Port_Type)]
pl <- pl[Date <= inputDate, .(PL_LC = sum(PL_LC)), by = .(Sec_Code, 
                                                           Port_Name, Class_L3, Port_Type)]
# the expected pl-------------------------------------------------------------------------

plTable <- local({
    tmp <- select(pos, Date, Port_Name, Class_L3, Port_Type,
                  Sec_Code, AV_Book_LC, YTM_Mix, Maturity_Date)
    tmp <- pl[tmp, on = c("Sec_Code", "Port_Name", "Port_Type")] %>%
      select(Date, Class_L3 = i.Class_L3, 
             Sec_Code, Port_Name, Port_Type, AV_Book_LC, PL_LC, 
             Maturity_Date, YTM_Mix)
    tmp[is.na(PL_LC), PL_LC := 0.0]
    tmp[, Maturity_Date := to_date(tmp$Maturity_Date)]
    tmp[!is.na(YTM_Mix) & !Class_L3 %in% c("Repo", "Reverse Repo") &
                 Maturity_Date > dateTo,
               YTM_PL := AV_Book_LC *
                 ((1 + YTM_Mix) ^ (as.numeric(dateTo - inputDate) / 365) - 1)]
    tmp[!is.na(YTM_Mix) & !Class_L3 %in% c("Repo", "Reverse Repo") &
          Maturity_Date < dateTo,
        YTM_PL := AV_Book_LC * ((1 + YTM_Mix) ^
                                  (as.numeric(Maturity_Date - inputDate)/ 365) - 1) +
                 AV_Book_LC * ((1 + YTM_Mix) ^ 
                                 (as.numeric(Maturity_Date - inputDate)/ 365)) *
          ((1 + yCash) ^ (as.numeric(dateTo - Maturity_Date) / 365) - 1)]
    tmp[Class_L3 %in% c('Cash', 'MMF'), Cash :=
          AV_Book_LC * ((1 + yCash) ^ (as.numeric(dateTo - inputDate) / 365) - 1)]
    tmp[Class_L3 == "Repo", Repo :=
          AV_Book_LC * ((1 + yRepo) ^ (as.numeric(dateTo - inputDate) / 365) -1)]
    tmp[Class_L3 == "Reverse Repo", Reverse_Repo :=
          AV_Book_LC * ((1 + yRepo) ^ (as.numeric(dateTo - inputDate) / 365) -1)]
    tmp[Class_L3 == "Pure bond fund", Bond_Fund :=
          AV_Book_LC *
          ((1 + bondFund) ^ (as.numeric(dateTo - inputDate) / 365) - 1)]
    tmp[Sec_Code == "GDPT.IB",
        Equity_Investment := AV_Book_LC * yGDPT - PL_LC]
    tmp[Sec_Code == "ZSJLCGQ.IB",
        Equity_Investment := AV_Book_LC * yZSJLCGQ - PL_LC]
    tmp[, `:=`(YTM_PL = GCAMCPUB::na_fill(YTM_PL, 0.0),
               Cash = GCAMCPUB::na_fill(Cash, 0.0),
               Repo = GCAMCPUB::na_fill(Repo, 0.0),
               Reverse_Repo = GCAMCPUB::na_fill(Reverse_Repo, 0.0),
               Bond_Fund = GCAMCPUB::na_fill(Bond_Fund, 0.0),
               Equity_Investment = GCAMCPUB::na_fill(Equity_Investment, 0.0))]
    tmp <- tmp[, PL_Coming := YTM_PL + Cash + Repo + Reverse_Repo + 
                 Bond_Fund + Equity_Investment]
    select(tmp, Class_L3, Port_Name, Port_Type, Sec_Code, PL_Coming)
})
# you can sum up either by Class_L3 or by Port_Name---------------------------------------
plByClass <- plTable[, .(PL_Coming = sum(PL_Coming)), by = .(Port_Type, Class_L3)]
plByPort <- plTable[, .(PL_Coming = sum(PL_Coming)), by = Port_Name]
plByClassRealized <- pl[, .(PL_LC = sum(PL_LC)), by = .(Port_Type, Class_L3)]
plByPortRealized <- pl[, .(PL_LC = sum(PL_LC)), by = .(Port_Type, Port_Name)]
write_open_xlsx(mget(c("plByClass", "plByPort", "plByClassRealized", "plByPortRealized")))

