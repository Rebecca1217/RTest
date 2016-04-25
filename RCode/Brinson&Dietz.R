lapply(list("GCAMCPUB", "data.table", "dplyr", "portanalytics"), library, ch = TRUE)
#可以查看dataDef.R
portanalytics::paDM$setLdcPath("D:/paData")
cbstr <- c("CNPC", "Par", "Life", "UV Individual", "UV Group", "CNPC-UV")
ulstr <- c("UL Anyi", "UL Aggressive", "UL Stable", "UL Increase", "UL Strategy", 
           "Mighty Rotation", "Flexible Allocation", "Daily Increase")
portanalytics::paDM$readFromLdc(c("201412", "201512"), c(cbstr, ulstr))
dataCenter <- portanalytics::paDM$dataCenter
bchmk_match <-  data.table(c("Bond", "Equity", "Liquidity"),
                   c("0571.CS", "000300.SH", "R007.IB")) %>%
                     setnames(c("Bchmk_Class", "Bchmk_Code"))
#ls(envir = dataCenter)
Input_Port <- "UL Stable"
Date_From <- as.Date('2014-12-31')
Date_To <- as.Date('2015-12-31')
bchmk <- dataCenter$bchmk[Bchmk_Code %in% c('0571.CS', '000300.SH', "R007.IB") &
                            Date >= Date_From & Date <= Date_To, ]
pos <- dataCenter$pos[Date >= Date_From & Date <= Date_To, ] %>%
  select(Date, HS_Port_Code, Class_L3, AV_Book_LC, AV_Mix_LC)
Name_Codematch <- data.table(c(rep("UL Stable", 4), rep("UL Anyi", 3), rep("UL Increase", 4),
                               rep("UL Strategy", 3), rep("UL Aggressive", 3)), 
                             c(2011, 2012, 2013, 2015, 2031, 2032, 2035,
                               2021, 2022, 2023, 2025, 2041, 2042, 2045,
                               2051, 2052, 2055)) %>%
  setnames(c("Port_Name", "HS_Port_Code"))
pos <- Name_Codematch[pos, on = "HS_Port_Code"]
pos <- pos[Port_Name == Input_Port, ]
# how to use list to read uniked linked data once time?
unitPrice <- portanalytics::paModel$unitPrice(Input_Port, 
                                  dates = as.Date(c(Date_From, Date_To)))
unitPrice <- cbind(Input_Port, unitPrice) 
# delete unused variables
unitPrice[, Item := NULL]; unitPrice[, Bchmk_Value := NULL]; unitPrice[, Bchmk_Rtn := NULL]
bchmk <- bchmk_match[bchmk, on = "Bchmk_Code"]
cf <- dataCenter$trans[Date >= Date_From & Date <= Date_To, ] %>%
  select(Date, HS_Port_Code, Class_L3, Trans_Type, CF_LC)
cf <- Name_Codematch[cf, on = "HS_Port_Code"]
cf <- cf[Port_Name == Input_Port, ]
#Portfolio return
Return_port <- last(unitPrice$Cum_Rtn)
#Benchmark return
##bchmrk权重对应表
bchmrkw <- data.table(c("UL Stable", "UL Anyi", "UL Increase",
                        "UL Strategy", "UL Aggressive"),
                      c(0.05, 0.05, 0.5, 0.5, 0.8),
                      c(0.9, 0.9, 0.45, 0.45, 0.15),
                      rep(0.05,5)) %>%
  setnames(c("Port_Name", "Equity_w", "FI_w", "Liquidity_w"))
##w_bchmk
w_bchmk <-
  local({
    tmp <- bchmrkw[Port_Name == Input_Port, ]
    c(tmp$Equity_w, tmp$FI_w, tmp$Liquidity_w)
  })

bchmk[, DR := Bchmk_Value / shift(Bchmk_Value, 1L) - 1, by = .(Bchmk_Class)]
bchmk[Bchmk_Class == 'Liquidity', DR := Bchmk_Value / 365 / 100]
bchmk[Date == Date_From & Bchmk_Class == 'Liquidity', DR := 0.0]
bchmk <- bchmk[ , DR := GCAMCPUB::na_fill(DR, 0.0)]
####三类资产加权到一起，计算每天的收益率
bchmk[, list(DR = sum(DR * w_bchmk)), by = .(Date)]
bchmk <- bchmk[, RTN := cumprod(DR+1) - 1]
Return_bchmrk <- last(bchmk$RTN)

Return_excess <- Return_port - Return_bchmk
###Brinson decomposition
tmp <- cf[Port_Name == Input_Port & Date >= Date_From &
                      Date <= Date_To, ] %>%
  mutate(CFpos = ifelse(CF_LC > 0, CF_LC, 0),
         CFneg = ifelse(CF_LC < 0, -CF_LC, 0))
cf_sum <- tmp[, list(cf_pos = sum(CFpos), cf_neg = sum(CFneg)), by = .(Date, Class_L3)]

