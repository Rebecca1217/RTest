# lapply(list("GCAMCPUB", "data.table", "portanalytics"), library, ch = TRUE)
library(GCAMCPUB)
library(data.table)
library(portanalytics)
#see more from dataDef.R

readFromLdc(c("201512", "201706"), c("Unit linked", "Third party"),
            jyDateRange = NULL)

# paras -------------------------------------------------------------------

pn <- "UL Aggressive"
date_from <- as.Date("2015-12-31")
date_to <- as.Date("2016-12-31")

# define the function -----------------------------------------------------

brinson_decomp <- function(pn, date_from, date_to){
  ##match table preparation
  bchmrkw <- data.table(c("UL Stable", "UL Anyi", "UL Increase",
                          "UL Strategy", "UL Aggressive"),
                        c(0.05, 0.05, 0.4, 0.4, 0.8),
                        c(0.9, 0.9, 0.55, 0.55, 0.15),
                        rep(0.05,5)) %>%
    setnames(c("Port_Name", "Equity_w", "FI_w", "Liquidity_w"))
  bchmk_match <-  data.table(c("Bond", "Equity", "Liquidity"),
                             c("0571.CS", "000300.SH", "Cash")) %>%
    setnames(c("Class_L1", "Bchmk_Code"))
  Class_match <- data.table(c(portanalytics::astClassSelector("Equity"), portanalytics::astClassSelector(
    "Fixed income"), portanalytics::astClassSelector("Liquidity")),
    c(rep("Equity", length(portanalytics::astClassSelector("Equity"))),
      rep("Fixed income", length(portanalytics::astClassSelector("Fixed income"))), 
      rep("Liquidity", length(portanalytics::astClassSelector("Liquidity"))))) %>%
    setnames(c("Class_L3_", "Class_L1"))
  Class_match[Class_L1 == 'Fixed income', Class_L1 := 'Bond']
  # data preparation from dataCenter & paModel$unitPrice
  bchmk <- dataCenter$bchmk[Bchmk_Code %in% c('0571.CS', '000300.SH', "Cash") &
                              Date >= Date_From & Date <= Date_To, ]
  bchmk <- bchmk_match[bchmk, on = "Bchmk_Code"]
  pos <- dataCenter$pos[Date >= Date_From & Date <= Date_To, ]
  pos <- pos[, .(Date, HS_Port_Code, AV_Mix_LC, Class_L3_)]
  pos <- attachHSPortInfo(pos, "Port_Name")
  pos <- pos[Port_Name == Input_Port, ]
  #how to use list to read uniked linked data once time? is it necessary?
  unitPrice <- portanalytics::unitPrice(Input_Port, 
                                        dates = as.Date(c(Date_From, Date_To)))
  #Portfolio return
  Return_port <- last(unitPrice$Cum_Rtn)
  #cf & pos aggregation by Class_L1 and combine pos & cf
  cf <- dataCenter$trans[Date >= Date_From & Date <= Date_To, ]
  cf <- cf[, .(Date, HS_Port_Code, Class_L3_, Trans_Type, CF_LC)]
  cf <- attachHSPortInfo(cf, "Port_Name")
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
  cf[, `:=`(
    CF_Pos = ifelse(CF_LC > 0, CF_LC, 0),
    CF_Neg = ifelse(CF_LC < 0, -CF_LC, 0)
  )]
  cf_sum <- cf[, list(CF_Pos = sum(CF_Pos), CF_Neg = sum(CF_Neg)), by = .(Date, Class_L3_)]
  cf_sum <- Class_match[cf_sum, on = 'Class_L3_']
  cf_sum <- cf_sum[, list(CF_Pos = sum(CF_Pos), CF_Neg = sum(CF_Neg)), by = .(Date, Class_L1)]
  pos <- Class_match[pos, on = 'Class_L3_']
  pos <- pos[, list(AV_Mix_LC = sum(abs(AV_Mix_LC))), by = .(Date, Port_Name, Class_L1)]
  pos <- cf_sum[pos, on = c("Date", "Class_L1")]
  pos <- pos[, `:=`(
    CF_Pos = ifelse(is.na(CF_Pos), 0.0, CF_Pos), 
    CF_Neg = ifelse(is.na(CF_Neg), 0.0, CF_Neg) 
  )] %>%
    setorder(Date)
  
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
  setDT(pos)
  pos[, wp := AV_Mix_LC / sum(AV_Mix_LC), by = Date]
  pos <- pos[, DR_p := na_fill(
    (AV_Mix_LC + CF_Pos) / (data.table::shift(AV_Mix_LC, 1L) + CF_Neg) - 1, 0.0),
    by = Class_L1] 
  bchmk[, DR_b := Bchmk_Value / shift(Bchmk_Value, 1L) - 1, by = .(Class_L1)]
  bchmk[Class_L1 == 'Liquidity', DR_b := Bchmk_Value / 365 / 100]
  bchmk[Date == Date_From & Class_L1 == 'Liquidity', DR_b := 0.0]
  bchmk <- bchmk[ , DR_b := GCAMCPUB::na_fill(DR_b, 0.0)]
  pos <- bchmk[pos, on = c("Date", "Class_L1")]
  pos <- w_bchmk[pos, on = 'Class_L1']
  pos[Class_L1 == "Liquidity", DR_p := DR_b]
  pos[, `:=`(
    deta_DR = DR_p - DR_b,
    deta_w = wp - wb
  )]
  bchmk <- bchmk[, list(DR = sum(DR_b * wb)), by = .(Date)]
  bchmk <- bchmk[, RTN := cumprod(DR + 1) - 1]
  Return_bchmk <- last(bchmk$RTN)
  setDT(pos)
  pos <- pos[, list(deta_w_Rb = sum(deta_w * DR_b), 
                    wb_deta_DR = sum(wb * deta_DR),
                    other = sum(deta_w * deta_DR)), by = Date]
  pos[, AllocationEffect := cumprod(deta_w_Rb + 1) - 1]
  pos[, SecuritySelection := cumprod(wb_deta_DR + 1) - 1]
  pos[, Other := cumprod(other + 1) - 1]
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
  tmp[, `:=`(
    Return_Port = f_fmt_pct(as.numeric(Return_port)),
    Return_Bchmk = f_fmt_pct(as.numeric(Return_bchmk)),
    Return_Excess= f_fmt_pct(as.numeric(Return_excess)),
    Alloc = f_fmt_pct(as.numeric(AllocationEffect)),
    Select = f_fmt_pct(as.numeric(SecuritySelection)),
    Others = f_fmt_pct(as.numeric(Others))
  )]
  tmp <- tmp[, .(Port_Name, Return_Port, Return_Bchmk, Return_Excess, Alloc, Select, Others)]
}
#test brinson
Brinson_decomp("UL Aggressive", "2015-12-31", "2016-12-31")


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













