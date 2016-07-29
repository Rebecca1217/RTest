lapply(list("GCAMCPUB", "data.table", "dplyr", "portanalytics"), library, ch = TRUE)

portanalytics::paDM$setLdcPath("E:/paData")
cbstr <- c("CNPC", "Par", "Life", "UV Individual", "UV Group", "CNPC-UV", "Capital RMB",
           "Flexible Allocation", "Daily Increase", "Strengthen Yield")
portanalytics::paDM$readFromLdc(c("201501", "201512"), cbstr)
dataCenter <- portanalytics::paDM$dataCenter

##data preparation
#matching tables
nameCodeMatch <- data.table(c(rep("CNPC", 4), rep("Par", 7), rep("Life", 7),
                              rep("Capital RMB", 7), rep("UV Individual", 4), rep("UV Group", 4),
                              rep("Flexible Allocation", 2), rep("Daily Increase", 2),
                              rep("Strengthen Yield", 2)),
                             c(portanalytics::portSelector(x = 'CNPC', portType = "HS_Port_Code"),
                               portanalytics::portSelector(x = 'Par', portType = "HS_Port_Code"),
                               portanalytics::portSelector(x = 'Life', portType = "HS_Port_Code"),
                               portanalytics::portSelector(x = 'Capital RMB', portType = "HS_Port_Code"),
                               portanalytics::portSelector(x = 'UV Individual', portType = "HS_Port_Code"),
                               portanalytics::portSelector(x = 'UV Group', portType = "HS_Port_Code"),
                               portanalytics::portSelector(x = 'Flexible Allocation', portType = "HS_Port_Code"),
                               portanalytics::portSelector(x = 'Daily Increase', portType = "HS_Port_Code"),
                               portanalytics::portSelector(x = 'Strengthen Yield', portType = "HS_Port_Code"))) %>%
  setnames(c("Port_Name", "HS_Port_Code"))
feeRatio <- data.table(c('990001.IB', '990002.IB', '990006.IB'),
                       c(0.012, 0.005, 0.0025)) %>%
  setnames(c("Sec_Code", "Fee_Ratio"))
nameMatch <- data.table(c('中意资产灵活配置', '中意资产-日日增利资产管理产品', 
                          '中意资产-增强收益1号资产管理产品'),
                        c('Flexible Allocation', 'Daily Increase', 'Strengthen Yield')) %>%
  setnames(c('Sec_Name', 'Sec_Name_En'))
# True AV_Mix_LC from Flexible Allocation, Daily Increade and Strengthen Yield
avMixTrue <- local({
  avMixTrue <- dataCenter$pos %>%
    select(Date, HS_Port_Code, AV_Mix_LC)
  avMixTrue <- nameCodeMatch[avMixTrue, on = "HS_Port_Code"]
  avMixTrue <- avMixTrue[Port_Name %in% c('Flexible Allocation', 'Daily Increase', 'Strengthen Yield'), ]
  avMixTrue <- avMixTrue[, .(AV_Mix_True = sum(AV_Mix_LC)), by = .(Date, Port_Name)] %>%
    arrange(Port_Name, Date) %>%
    setnames("Port_Name", "Sec_Name")
})
# Total share from Core_Date_Share of the 3 products
share <- dataCenter$share[Date >= '2015-01-01' & Date <= '2015-12-31', ] %>%
  setnames("Port_Name", "Sec_Name")
# pos from Core_Date_hoding and linking with AV_Mix_True and Share, compute the share weight of each
# port ,then compute the mgtFee
pos <- dataCenter$pos %>%
  select(Date, HS_Port_Code, Sec_Code, Sec_Name, Quantity, AV_Mix_LC)
# create a share table with complete date
completeShare <- local({
  date <- sort(unique(pos$Date))
  completeShare <- data.table(rep(date, 3), c(rep("Flexible Allocation", length(date)), 
                                              rep("Daily Increase", length(date)), rep("Strengthen Yield", length(date)))) %>%
    setnames(c("Date", "Sec_Name"))
  completeShare <- share[completeShare, on = c("Date", "Sec_Name")]
  tmp <- completeShare
  tmp[, Flag := !is.na(Share)] %>%
    arrange(Sec_Name, Date)
  tmp[, Label := cumsum(Flag), by = .(Sec_Name)]
  tmp <- tmp[, Cum_Share := Cum_Share[1], by =.(Sec_Name, Label)] %>%
    mutate(Cum_Share = GCAMCPUB::na_fill(Cum_Share, 0.0))
  select(tmp, Date, Sec_Name, Cum_Share)
})
##the process of linking and computing
pos <- pos[Sec_Code %in% c('990001.IB', '990002.IB', '990006.IB')]
pos <- nameMatch[pos, on = "Sec_Name"] %>%
  select(-Sec_Name) %>%
  setnames("Sec_Name_En", "Sec_Name")
pos <- nameCodeMatch[pos, on = "HS_Port_Code"]
pos <- feeRatio[pos, on = "Sec_Code"]
pos <- completeShare[pos, on = c("Date", "Sec_Name")]
pos <- avMixTrue[pos, on = c("Date", "Sec_Name")]
pos <- arrange(pos, Port_Name, Sec_Name, Date)
pos[, AV_Mix_Port := Quantity / Cum_Share * AV_Mix_True]
pos[, New_Date := shift(Date, type = "lead"), by = .(Port_Name, Sec_Code)]
pos[is.na(New_Date), New_Date := Date + 1]
pos <- pos %>%
  mutate(Gap= New_Date - Date, Mgt_Fee_Daily = AV_Mix_Port * Fee_Ratio / 365,
         Mgt_Fee_Weekend = AV_Mix_Port * Fee_Ratio / 365 * Gap)
tmp <- pos[, .(Mgt_Fee_DailySum = sum(Mgt_Fee_Daily), 
        Mgt_Fee_WeekendSum = sum(Mgt_Fee_Weekend)), by = .(Port_Name, Sec_Name)]




portanalytics::portSelector(x = 'JinGu', portType = "HS_Port_Code")
