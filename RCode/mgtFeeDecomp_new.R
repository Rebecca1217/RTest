lapply(list("GCAMCPUB", "data.table", "dplyr", "portanalytics"), library, ch = TRUE)

portanalytics::paDM$setLdcPath("E:/paData")
cbstr <- c("CNPC", "Par", "Life", "UV Individual", "UV Group", "CNPC-UV", "Capital RMB",
           "Flexible Allocation", "Daily Increase", "Strengthen Yield")
portanalytics::paDM$readFromLdc(c("201501", "201512"), cbstr)
dataCenter <- portanalytics::paDM$dataCenter
mgtFee <- read.csv("C:/Users/AMC161/Desktop/mgtFee.csv", header = T) %>%
  setDT()
mgtFee$Date <- as.Date(mgtFee$Date)
cumShare <- read.csv("C:/Users/AMC161/Desktop/cumShare.csv", header = T)%>%
  setDT() 
cumShare$Date <- as.Date(cumShare$Date)
quantityChange <- read.csv("C:/Users/AMC161/Desktop/quantityChange.csv", header = T) %>%
  setDT()
quantityChange$Date <- as.Date(quantityChange$Date)
# nameCodeMatch <- data.table(c(rep("CNPC", 4), rep("Par", 7), rep("Life", 7),
#                               rep("Capital RMB", 7), rep("UV Individual", 4), rep("UV Group", 4),
#                               rep("Flexible Allocation", 2), rep("Daily Increase", 2),
#                               rep("Strengthen Yield", 2)),
#                             c(portanalytics::portSelector(x = 'CNPC', portType = "HS_Port_Code"),
#                               portanalytics::portSelector(x = 'Par', portType = "HS_Port_Code"),
#                               portanalytics::portSelector(x = 'Life', portType = "HS_Port_Code"),
#                               portanalytics::portSelector(x = 'Capital RMB', portType = "HS_Port_Code"),
#                               portanalytics::portSelector(x = 'UV Individual', portType = "HS_Port_Code"),
#                               portanalytics::portSelector(x = 'UV Group', portType = "HS_Port_Code"),
#                               portanalytics::portSelector(x = 'Flexible Allocation', portType = "HS_Port_Code"),
#                               portanalytics::portSelector(x = 'Daily Increase', portType = "HS_Port_Code"),
#                               portanalytics::portSelector(x = 'Strengthen Yield', portType = "HS_Port_Code"))) %>%
#   setnames(c("Port_Name", "HS_Port_Code"))
# nameMatch <- data.table(c('中意资产灵活配置', '中意资产-日日增利资产管理产品', 
#                           '中意资产-增强收益1号资产管理产品'),
#                         c('Flexible Allocation', 'Daily Increase', 'Strengthen Yield')) %>%
#   setnames(c('Sec_Name', 'Sec_Name_En'))
#db_mo <- GCAMCPUB::activate_conn(db_mo)
##share -- quantity
# sql <- "select Date, Port_Name, Share, Note
# from CORE_Data_Share where Port_Name in ('Flexible Allocation', 'Daily Increase', 'Strengthen Yield')
# and Date <= '2015-12-31'
# order by Date, Port_Name"
# shareDetail <- 
#   DBI::dbGetQuery(db_mo, sql) %>%
#   dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
#   data.table::setDT() 
# share <- dataCenter$share[Date >= '2015-01-01' & Date <= '2015-12-31', ] %>%
#   setnames("Port_Name", "Sec_Name")


# create a share table with complete date
completeShare <- local({
  date <- sort(unique(dataCenter$pos$Date))
  date <- c(as.Date('2015-05-23'), as.Date('2015-05-24'), date) %>%
    sort()
  completeShare <- data.table(rep(date, 3), c(rep("Flexible Allocation", length(date)),
                                              rep("Daily Increase", length(date)), rep("Strengthen Yield", length(date)))) %>%
    setnames(c("Date", "Sec_Name"))
  completeShare <- cumShare[completeShare, on = c("Date", "Sec_Name")]
  tmp <- completeShare
  tmp[, Flag := !is.na(Cum_Share)] %>%
    arrange(Sec_Name, Date)
  tmp[, Label := cumsum(Flag), by = .(Sec_Name)]
  tmp <- tmp[, Cum_Share := Cum_Share[1], by =.(Sec_Name, Label)] %>%
    mutate(Cum_Share = GCAMCPUB::na_fill(Cum_Share, 0.0))
  select(tmp, Date, Sec_Name, Cum_Share)
})
quantityChange[, Quantity := cumsum(Quantity_Change), by = .(Port_Name, Sec_Name)]
quantity <- local({
  completeQuantity <- read.csv("C:/Users/AMC161/Desktop/completeQuantity.csv", header = T) %>%
    setDT()
  completeQuantity$Date <- as.Date(completeQuantity$Date)
  tmp <- quantityChange[completeQuantity, on = c("Date", "Port_Name", "Sec_Name")] %>%
    select(-Quantity_Change) %>%
    arrange(Sec_Name, Port_Name, Date)
  tmp[, Flag := !is.na(Quantity)] 
  tmp[, Label := cumsum(Flag), by = .(Sec_Name, Port_Name)]
  tmp <- tmp[, Quantity := Quantity[1], by =.(Sec_Name, Port_Name, Label)] %>%
    mutate(Quantity = GCAMCPUB::na_fill(Quantity, 0.0))
  select(tmp, Date, Port_Name, Sec_Name, Quantity)
})
quantity <- completeShare[quantity, on = c("Date", "Sec_Name")]
# quantity <- quantity[is.na(Cum_Share), Cum_Share := shift(Cum_Share, 1L)]
# quantity <- quantity[is.na(Cum_Share), Cum_Share := shift(Cum_Share, 1L)]
for (i in 1 : nrow(quantity)){
  if(is.na(quantity$Cum_Share[i])){
    if(is.na(quantity$Cum_Share[i - 1])){
    if(is.na(quantity$Cum_Share[i - 2])){
      quantity$Cum_Share[i] = quantity$Cum_Share[i - 3]
    }
      quantity$Cum_Share[i] = quantity$Cum_Share[i - 2]
    }
    quantity$Cum_Share[i] = quantity$Cum_Share[ i - 1]
  }
  i = i + 1
}
quantity[, Weight := Quantity / Cum_Share]
quantity <- quantity[Date %in% as.Date(c('2015-05-22', '2015-05-23')) & Port_Name %in%
           c('Life', 'Par'), Weight := 0.5 ]
quantity <- mgtFee[quantity, on = c("Sec_Name", "Date")]
quantity <- quantity[!is.na(Mgt_Fee), ]
mgtFeeSplit <- quantity[, Mgt_Fee_Split := Weight * Mgt_Fee]
mgtFeeDecomp <- mgtFeeSplit[, .(Mgt_Fee_Split = sum(Mgt_Fee_Split)), by = .(Sec_Name, Port_Name)]
## 后期需要调整的地方:5.25 管理费日期缺一个，一开始没记周末，后来都记了周末
## 5.22和5.23没有持仓，但是记了管理费，给Life和Par平分

# 
# pos <- ({
#   pos <- dataCenter$pos[Sec_Code %in% c('990001.IB', '990002.IB', '990006.IB'), ] %>%
#     select(Date, HS_Port_Code, Sec_Name, Sec_Code, Quantity)
#   tmp <- nameCodeMatch[pos, on = 'HS_Port_Code']
#   tmp <- nameMatch[tmp, on = 'Sec_Name'] %>%
#     select(-Sec_Name, -HS_Port_Code, -Sec_Code) %>%
#     setnames("Sec_Name_En", "Sec_Name")
#   tmp <- completeShare[tmp, on = c("Date", "Sec_Name")]
#   tmp[, Weight := Quantity / Cum_Share]
#   tmp
# })
# 
 write.csv(mgtFeeSplit, "C:/Users/AMC161/Desktop/mgtFeeSplit.csv")
