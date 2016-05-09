lapply(list("GCAMCPUB", "data.table", "dplyr", "portanalytics"), library, ch = TRUE)
#see more from dataDef.R
#basic data configuation: load all data from cost based and unit linked from 2014-12 to 2016-04
portanalytics::paDM$setLdcPath("E:/paData")
cbstr <- c("CNPC", "Par", "Life", "UV Individual", "UV Group", "CNPC-UV")
ulstr <- c("UL Anyi", "UL Aggressive", "UL Stable", "UL Increase", "UL Strategy", 
           "Mighty Rotation", "Flexible Allocation", "Daily Increase")
portanalytics::paDM$readFromLdc(c("201412", "201604"), c(cbstr, ulstr))
dataCenter <- portanalytics::paDM$dataCenter
#Brinson function
Brinson_decomp <- function(Input_Port, Date_From, Date_To){
  ##match table preparation
  bchmrkw <- data.table(c("UL Stable", "UL Anyi", "UL Increase",
                          "UL Strategy", "UL Aggressive"),
                        c(0.05, 0.05, 0.5, 0.5, 0.8),
                        c(0.9, 0.9, 0.45, 0.45, 0.15),
                        rep(0.05,5)) %>%
    setnames(c("Port_Name", "Equity_w", "FI_w", "Liquidity_w"))
  bchmk_match <-  data.table(c("Bond", "Equity", "Liquidity"),
                             c("0571.CS", "000300.SH", "R007.IB")) %>%
    setnames(c("Class_L1", "Bchmk_Code"))
  Name_Codematch <- data.table(c(rep("UL Stable", 4), rep("UL Anyi", 3), rep("UL Increase", 4),
                                 rep("UL Strategy", 3), rep("UL Aggressive", 3)), 
                               c(portanalytics::portSelector(x = 'UL Stable', portType = "HS_Port_Code"),
                                 portanalytics::portSelector(x = 'UL Anyi', portType = "HS_Port_Code"),
                                 portanalytics::portSelector(x = 'UL Increase', portType = "HS_Port_Code"),
                                 portanalytics::portSelector(x = 'UL Strategy', portType = "HS_Port_Code"),
                                 portanalytics::portSelector(x = 'UL Aggressive', portType = "HS_Port_Code"))) %>%
    setnames(c("Port_Name", "HS_Port_Code"))
  Class_match <- data.table(c(portanalytics::astClassSelector("Equity"), portanalytics::astClassSelector(
    "Fixed income"), portanalytics::astClassSelector("Liquidity")),
    c(rep("Equity", length(portanalytics::astClassSelector("Equity"))),
      rep("Fixed income", length(portanalytics::astClassSelector("Fixed income"))), 
      rep("Liquidity", length(portanalytics::astClassSelector("Liquidity"))))) %>%
    setnames(c("Class_L3_", "Class_L1"))
  Class_match[Class_L1 == 'Fixed income', Class_L1 := 'Bond']
  # data preparation from dataCenter & paModel$unitPrice
  bchmk <- dataCenter$bchmk[Bchmk_Code %in% c('0571.CS', '000300.SH', "R007.IB") &
                              Date >= Date_From & Date <= Date_To, ]
  bchmk <- bchmk_match[bchmk, on = "Bchmk_Code"]
  pos <- dataCenter$pos[Date >= Date_From & Date <= Date_To, ] %>%
    select(Date, HS_Port_Code, AV_Mix_LC, Class_L3_)
  pos <- Name_Codematch[pos, on = "HS_Port_Code"]
  pos <- pos[Port_Name == Input_Port, ]
  #how to use list to read uniked linked data once time? is it necessary?
  unitPrice <- portanalytics::paModel$unitPrice(Input_Port, 
                                                dates = as.Date(c(Date_From, Date_To)))
  #Portfolio return
  Return_port <- last(unitPrice$Cum_Rtn)
  #cf & pos aggregation by Class_L1 and combine pos & cf
  cf <- dataCenter$trans[Date >= Date_From & Date <= Date_To, ] %>%
    select(Date, HS_Port_Code, Class_L3_, Trans_Type, CF_LC)
  cf <- Name_Codematch[cf, on = "HS_Port_Code"]
  cf <- cf[Port_Name == Input_Port, ]
  # change the weedends' cf to the next monday
  cf <-
    local({
      tmp <- data.table::data.table(
        Date = sort(unique(cf$Date)), DateNew = sort(unique(cf$Date)), key = "Date")
      tmp <- tmp[cf, on = "Date", roll = -Inf]
      tmp[, Date := NULL]
      data.table::setnames(tmp, "DateNew", "Date")
    })
  cf <- mutate(cf, CFpos = ifelse(CF_LC > 0, CF_LC, 0),
               CFneg = ifelse(CF_LC < 0, -CF_LC, 0))
  cf_sum <- cf[, list(cf_pos = sum(CFpos), cf_neg = sum(CFneg)), by = .(Date, Class_L3_)]
  cf_sum <- Class_match[cf_sum, on = 'Class_L3_']
  cf_sum <- cf_sum[, list(cf_pos = sum(cf_pos), cf_neg = sum(cf_neg)), by = .(Date, Class_L1)]
  pos <- Class_match[pos, on = 'Class_L3_']
  pos <- pos[, list(AV_Mix_LC = sum(abs(AV_Mix_LC))), by = .(Date, Port_Name, Class_L1)]
  pos <- cf_sum[pos, on = c("Date", "Class_L1")] %>%
    mutate(cf_pos = ifelse(is.na(cf_pos), 0.0, cf_pos), 
           cf_neg = ifelse(is.na(cf_neg), 0.0, cf_neg)) %>%
    arrange(Date)
  ##w_bchmk
  w_bchmk <-
    local({
      tmp <- bchmrkw[Port_Name == Input_Port, ]
      data.table(c('Equity', 'Bond', 'Liquidity'),
                 c(tmp$Equity_w, tmp$FI_w, tmp$Liquidity_w)) %>%
        setnames(c('Class_L1', 'wb'))
    })
  bchmk <- w_bchmk[bchmk, on = 'Class_L1']
  
  #compute wp, DR_p, wb, DR_b, note the DR of liquidity is R /365
  pos[, wp := AV_Mix_LC / sum(AV_Mix_LC), by = Date]
  pos <- pos[, DR_p := (AV_Mix_LC + cf_pos) / (data.table::shift(AV_Mix_LC, 1L) + cf_neg) - 1, by = Class_L1] %>%
    mutate(DR_p = GCAMCPUB::na_fill(DR_p, 0.0))
  bchmk[, DR_b := Bchmk_Value / shift(Bchmk_Value, 1L) - 1, by = .(Class_L1)]
  bchmk[Class_L1 == 'Liquidity', DR_b := Bchmk_Value / 365 / 100]
  bchmk[Date == Date_From & Class_L1 == 'Liquidity', DR_b := 0.0]
  bchmk <- bchmk[ , DR_b := GCAMCPUB::na_fill(DR_b, 0.0)]
  pos <- bchmk[pos, on = c("Date", "Class_L1")]
  pos <- w_bchmk[pos, on = 'Class_L1']
  pos[Class_L1 == "Liquidity", DR_p := DR_b]
  pos <- mutate(pos, deta_DR = DR_p - DR_b, deta_w = wp - wb)
  bchmk <- bchmk[, list(DR = sum(DR_b * wb)), by = .(Date)]
  bchmk <- bchmk[, RTN := cumprod(DR + 1) - 1]
  Return_bchmk <- last(bchmk$RTN)
  pos <- pos[, list(deta_w_Rb = sum(deta_w * DR_b), wb_deta_DR = sum(wb * deta_DR)), by = Date]
  pos <- pos[, AllocationEffect := cumprod(deta_w_Rb + 1) - 1]
  pos <- pos[, SecuritySelection := cumprod(wb_deta_DR + 1) - 1]
  
  #total Benchmark return
  
  # show results
  Return_excess <- Return_port - Return_bchmk
  AllocationEffect <- last(pos$AllocationEffect)
  SecuritySelection <- last(pos$SecuritySelection)
  Others <- Return_excess - AllocationEffect - SecuritySelection
  tmp <- data.table(t(c(Input_Port, Return_port, Return_bchmk, Return_excess,
                        AllocationEffect, SecuritySelection, Others))) %>%
    setnames(c("Port_Name", "Return_port", "Return_bchmk",  "Return_excess",
               "AllocationEffect", "SecuritySelection", "Others"))
  result <- data.table(t(c(Input_Port, f_fmt_pct(as.numeric(select(tmp, -Port_Name)[1, ]))))) %>%
    setnames(names(tmp))
  result
}
#test brinson
Brinson_decomp("UL Stable", "2014-12-31", "2015-11-23")


#Dietz function
# pos_cb <- dataCenter$pos[Date >= Date_From & Date <= Date_To & HS_Port_Code %in% 
#                            portanalytics::portSelector(x = Input_Port, portType = "HS_Port_Code"), ] %>%
#   select(Date, Class_L3_FundinDetail2, AV_Mix_LC, AV_Book_LC, UGL_LC)
# pos_cb <- pos_cb[, .(AV_Mix_LC = sum(AV_Mix_LC), AV_Book_LC = sum(AV_Book_LC), UGL_LC = sum(UGL_LC)), 
#                                      by = c("Date", "Class_L3_FundinDetail2")]
# trans <- dataCenter$trans[Date >= Date_From & Date <= Date_To & HS_Port_Code %in%
#                             portanalytics::portSelector(x = Input_Port, portType = "HS_Port_Code")] %>%
#   select(Date, Class_L3_FundinDetail2, Trans_Type, CF_LC)
# trans <- trans[, .(CF_LC = sum(CF_LC)), by = c("Date", "Class_L3_FundinDetail2", "Trans_Type")]
# pos_cb <- trans[pos_cb, on = c("Date", "Class_L3_FundinDetail2")]












#test dietz