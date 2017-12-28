library(portanalytics)
library(data.table)
library(GCAMCPUB)
library(DBI)
# 2015年之前境内境外SAA是不分的
readFromLdc(c("201612", "201711"), 
            c("All cost based", "Third party"), 
            jyDateRange = NULL)
dateBased <- as.Date("2017-11-23")
dateFrom<- as.Date("2016-12-31")
classEQ <- c("Active fund", "Stock", "Index fund", "REITs", "IAMP-Stock", "Equity future")
pos <- dataCenter$pos[Date %in% c(dateBased, dateFrom)]
# find a way to replace the following info
attachHSPortInfo(pos, c("Port_Name", "Port_Type"))
myConn <- activate_conn("db_mo")
sql <- "select HS_Port_Code, if_FC_HS_Port from CORE_para_Port"
portInfo <- dbGetQuery(myConn, sql) %>%
  setDT()
sql <- "select Port_Name, SAA_Class, Target from PortMgt_SAA where Date > '%s' and
SAA_Class in ('Listed equities', 'Domestic listed equities', 'Overseas listed equities')
and Date <= '%s'"
sql <- sprintf(sql, dateFrom, dateBased)
saaTarget <- dbGetQuery(myConn, sql) %>%
  setDT
dbDisconnect(myConn)
# pos <- portInfo[pos, on = "HS_Port_Code"]
# calculate allocation & YTM --------------------------------------------------

bondFund <- c("Bond fund", "IAMP-FI-NAV", "SFP-FI-NAV")
pe <- c("Equity investment", "OTC unlisted equity forward", "BJ-SH Railway")
liquidity <- c("MMF", "Reverse Repo", "Cash", "IAMP-MMP-NAV", "BFP-MMP-NAV")
equity <- c("Active fund", "Stock", "Index fund", "REITs", "IAMP-Stock")
portName <- c("New_Class", "CNPC", "Par",  "Life",  "Capital_RMB",
              "UV_Individual", "UV_Group", "Capital_FX", "CNPC_UV")
repo <- "Repo"
pos[!is.na(YTM_Mix), New_Class := "YTM Asset"]
pos[Class_L3_ %in% bondFund, New_Class := "Bond Fund Class"]
pos[Class_L3_ %in% pe, New_Class := "PE"]
pos[Class_L3_ %in% equity, New_Class := "Equity"]
pos[Class_L3_ %in% repo, New_Class := "Repo"]
pos[Class_L3_ %in% liquidity, New_Class := "Liquidity"]

alloc <- pos[Port_Type %in% c("Cost based", "Cost based FC") & Date == dateBased,
             .(AV_Book_LC = sum(AV_Book_LC)), 
             by = .(Port_Name, New_Class)]
allocWeight <- local({
  tmp <- dcast(alloc, New_Class ~ Port_Name, fun = sum, value.var = 'AV_Book_LC')
  tmp[, `:=`(
    CNPC_W = CNPC / sum(CNPC),
    `CNPC-UV_W` = `CNPC-UV` / sum(`CNPC-UV`), 
    `Capital RMB_W` = `Capital RMB` / sum(`Capital RMB`),
    Life_W = Life / sum(Life),
    Par_W = Par / sum(Par),
    `UV Group_W` = `UV Group` / sum(`UV Group`),
    `UV Individual_W` = `UV Individual` / sum(`UV Individual`),
    `Capital FX_W` = `Capital FX` / sum(`Capital FX`)
  )
  ]
  tmp <- tmp[, .(New_Class, CNPC_W,  Par_W, Life_W, `Capital RMB_W`,
                 `UV Group_W`, `UV Individual_W`, `Capital FX_W`, `CNPC-UV_W`)] %>%
    setnames(portName)
  tmp
})

ytm <- local({
  tmp <- pos[!is.na(YTM_Mix) & Port_Type %in% c("Cost based", "Cost based FC")]
  tmp <- tmp[, 
             .(YTM = sum(AV_Book_LC * YTM_Mix) / sum(AV_Book_LC)),
             by = .(Port_Name)]
  tmp <- data.table(t(tmp)) %>%
    setnames(portName[-c(1,9)])
  tmp <- tmp[2]
  tmp[, `:=`(
    New_Class = "YTM Asset" ,
    CNPC_UV = 0.0
  )]
  tmp[, .(New_Class, CNPC, Par, Life, Capital_RMB, UV_Group, UV_Individual, `Capital_FX`, CNPC_UV)]
})



# Equity Update -----------------------------------------------------------

port_order <- data.table(Port_Name = c("CNPC", "Par", "Life", "Capital RMB",
                                       "UV Individual", "UV Group", "Total"))
# add if_FC to pos --------------------------------------------------------

equityAlloc <- local({
  tmp <- copy(pos)[Date == dateBased]
  # mark HK Anxin & HuaTai & hugangshen fund to oversea assets; lookthrough 2 times
  tmpAnxin <- lookThrough(lookThrough(tmp[Sec_Code == "990045.IB"]))
  tmpAnxin[, if_FC := 1]
  tmpAnxin <- portInfo[tmpAnxin, on = "HS_Port_Code"]
  tmpExclAnxin <- lookThrough(lookThrough(tmp[Sec_Code != "990045.IB"]))
  tmpExclAnxin <- portInfo[tmpExclAnxin, on = "HS_Port_Code"]
  tmpExclAnxin[if_FC_HS_Port == TRUE | Sec_Code %in% c("990025.IB", "004091.OF", "519139.OF"),
               if_FC := 1]
  tmpExclAnxin[, if_FC := GCAMCPUB::na_fill(if_FC, 0)]
  tmpPos <- rbindlist(list(tmpAnxin, tmpExclAnxin)) %>%
    attachHSPortInfo(c("Port_Name", "Port_Type"))
  tmpPos <- tmpPos[Port_Type == "Cost based"]
  allocEQ <- tmpPos[Class_L3 %in% classEQ & Port_Type %in% c("Cost based", "Cost based FC"), 
               .(AV_Mix_EQ = sum(AV_Mix_LC)), 
               by = .(Port_Name, if_FC)]
  allocEQ <- dcast(allocEQ, Port_Name ~ if_FC, fun = sum, value.var = "AV_Mix_EQ") %>%
    setnames(c("Port_Name", "AV_EQ_Dom", "AV_EQ_FC"))
  alloc <- tmpPos[, 
               .(AV_Mix = sum(AV_Mix_LC)),
               by = .(Port_Name)]
  allocEQ <- alloc[allocEQ, on = "Port_Name"]
  allocEQ[, `:=`(
    Domestic = AV_EQ_Dom / AV_Mix,
    Overseas = AV_EQ_FC / AV_Mix
  )]
  allocEQ[, Total := Domestic + Overseas]
  addRow <- data.table(
    Port_Name = "Total",
    Domestic = sum(allocEQ$AV_EQ_Dom) / sum(allocEQ$AV_Mix),
    Overseas = sum(allocEQ$AV_EQ_FC) / sum(allocEQ$AV_Mix),
    Total = (sum(allocEQ$AV_EQ_Dom) + sum(allocEQ$AV_EQ_FC)) / sum(allocEQ$AV_Mix)
  )
  allocEQ[, `:=`(
    AV_Mix = NULL,
    AV_EQ_Dom = NULL,
    AV_EQ_FC = NULL
  )]
  equityAlloc <- rbindlist(list(allocEQ, addRow))
  stopifnot(nrow(equityAlloc) == nrow(port_order))
  equityAlloc <- equityAlloc[port_order, on = "Port_Name"]
  equityAlloc
})

equityAdj <- local({
  uglChg <- local({
    posUGL <- copy(pos)[, .(Date, HS_Port_Code, Sec_Code, Sec_Name, UGL_LC, Class_L3)]
    posUGL <- portInfo[posUGL, on = "HS_Port_Code"]
    posUGL[if_FC_HS_Port == TRUE | 
             Sec_Code %in% c("990045.IB", "990025.IB", "004091.OF", "519139.OF"),
           if_FC := 1]
    posUGL[, if_FC := GCAMCPUB::na_fill(if_FC, 0)]
    attachHSPortInfo(posUGL, c("Port_Name", "Port_Type"))
    posUGL <- posUGL[Port_Type == "Cost based" & Class_L3 %in% classEQ]
    uglEnd <- posUGL[Date == dateBased, 
                     .(UGL_End = sum(UGL_LC)),
                     .(Port_Name, if_FC, Date)] %>%
      dcast(Port_Name + Date ~ if_FC, fun = sum, value.var = "UGL_End") %>%
      setnames(c("Port_Name", "Date_End", "Dom_End", "Overs_End"))
    uglFrom <- posUGL[Date == dateFrom, 
                      .(UGL_End = sum(UGL_LC)),
                      .(Port_Name, if_FC, Date)] %>%
      dcast(Port_Name + Date ~ if_FC, fun = sum, value.var = "UGL_End") %>%
      setnames(c("Port_Name", "Date_From", "Dom_From", "Overs_From"))
    uglChg <- uglFrom[uglEnd, on = "Port_Name"]
    uglChg[, `:=`(
      UGL_Chg_Dom = Dom_End - Dom_From,
      UGL_Chg_Overs = Overs_End - Overs_From
    )]
    uglChg[, .(Port_Name, UGL_Chg_Dom, UGL_Chg_Overs)]
  })
  pl <- local({
    pl <- dataCenter$pl[Date > dateFrom & Date <= dateBased]
    pl <- portInfo[pl, on = "HS_Port_Code"]
    pl[if_FC_HS_Port == TRUE | 
         Sec_Code %in% c("990045.IB", "990025.IB", "004091.OF", "519139.OF"),
       if_FC := 1]
    pl[, if_FC := GCAMCPUB::na_fill(if_FC, 0)]
    attachHSPortInfo(pl, c("Port_Name", "Port_Type"))
    pl <- pl[Port_Type  == "Cost based" & Class_L3 %in% classEQ]
    pl <- pl[, .(PL = sum(PL_LC)), .(Port_Name, if_FC)] %>%
      dcast(Port_Name ~ if_FC, fun = sum, value.var = "PL") %>%
      setnames(c("Port_Name", "PL_Dom", "PL_Overs"))
    pl
  })
  eqPos <- local({
    ports <- paPorts(as.list(c("CNPC", "Par", "Life", "Capital RMB", "UV Individual",
                               "UV Group", "All cost based")),
                     names = c("CNPC", "Par", "Life", "Capital RMB","UV Individual", 
                               "UV Group", "Total"))
    avc <- portanalytics::avc(ports, c(dateFrom, dateBased), ts = F) 
    avc <- avc[, .(Port_Name = Item, AVC)]
    target <- dcast(saaTarget, Port_Name ~ SAA_Class, value.var = "Target")
    eqPos <- avc[target, on = "Port_Name"]
    eqPos[, `:=`(
      Dom = `Domestic listed equities` * AVC,
      Overs = `Overseas listed equities` * AVC,
      Total = `Listed equities` * AVC
    )][, .(Port_Name, Dom, Overs, Total)]
  })
  eqAdj <- uglChg[pl, on = "Port_Name"][eqPos, on = "Port_Name"]
  eqAdj[, `:=`(
    EQ_Adj_Dom = (PL_Dom + UGL_Chg_Dom) / Dom,
    EQ_Adj_Overs = (PL_Overs + UGL_Chg_Overs) / Overs,
    EQ_Adj_Total = (PL_Dom + UGL_Chg_Dom + PL_Overs + UGL_Chg_Overs) / Total
  )]
  addRow <- eqAdj[, .(UGL_Chg_Dom = sum(UGL_Chg_Dom),
                      UGL_Chg_Overs = sum(UGL_Chg_Overs),
                      PL_Dom = sum(PL_Dom),
                      PL_Overs = sum(PL_Overs),
                      Dom = sum(Dom),
                      Overs = sum(Overs),
                      Total = sum(Total))]
  addRow[, `:=`(
    Port_Name = "Total",
    EQ_Adj_Dom = (PL_Dom + UGL_Chg_Dom) / Dom,
    EQ_Adj_Overs = (PL_Overs + UGL_Chg_Overs) / Overs,
    EQ_Adj_Total = (PL_Dom + UGL_Chg_Dom + PL_Overs + UGL_Chg_Overs) / Total
  )]
  eqAdj <- rbindlist(list(eqAdj, addRow), use.names = TRUE)
  eqAdj <- eqAdj[port_order, on = "Port_Name"]
  eqAdj
})
# equity Adjusted Rtn(not considering HK Anxin lookthrough) ---------------

ports <- paPorts(as.list(c("CNPC", "Par", "Life", "Capital RMB", "UV Individual",
                           "UV Group", "Capital FX", "All cost based")),
                 names = c("CNPC", "Par", "Life", "Capital RMB","UV Individual", 
                           "UV Group", "Capital FX", "Total"))
equityRtn <- equityAdjRtn(ports = ports, dates = c(dateFrom, dateBased), ts = FALSE, avcType = c("YTD"))


# ulRank ------------------------------------------------------------------

write_open_xlsx(ulRank(c(dateFrom, dateBased)))


# equityAdj  original Currency --------------------------------------------

# AVC 没有调整那个调起来太麻烦了，感觉算那么精确意义不大

equityAdj <- local({
  uglChg <- local({
    posUGL <- copy(pos)[, .(Date, HS_Port_Code, Sec_Code, Sec_Name, UGL_OC, Class_L3)]
    posUGL <- portInfo[posUGL, on = "HS_Port_Code"]
    posUGL[if_FC_HS_Port == TRUE | 
             Sec_Code %in% c("990045.IB", "990025.IB", "004091.OF", "519139.OF"),
           if_FC := 1]
    posUGL[, if_FC := GCAMCPUB::na_fill(if_FC, 0)]
    attachHSPortInfo(posUGL, c("Port_Name", "Port_Type"))
    posUGL <- posUGL[Port_Type == "Cost based" & Class_L3 %in% classEQ]
    uglEnd <- posUGL[Date == dateBased, 
                     .(UGL_End = sum(UGL_OC)),
                     .(Port_Name, if_FC, Date)] %>%
      dcast(Port_Name + Date ~ if_FC, fun = sum, value.var = "UGL_End") %>%
      setnames(c("Port_Name", "Date_End", "Dom_End", "Overs_End"))
    uglFrom <- posUGL[Date == dateFrom, 
                      .(UGL_End = sum(UGL_OC)),
                      .(Port_Name, if_FC, Date)] %>%
      dcast(Port_Name + Date ~ if_FC, fun = sum, value.var = "UGL_End") %>%
      setnames(c("Port_Name", "Date_From", "Dom_From", "Overs_From"))
    uglChg <- uglFrom[uglEnd, on = "Port_Name"]
    uglChg[, `:=`(
      UGL_Chg_Dom = Dom_End - Dom_From,
      UGL_Chg_Overs = Overs_End - Overs_From
    )]
    uglChg[, .(Port_Name, UGL_Chg_Dom, UGL_Chg_Overs)]
  })
  pl <- local({
    pl <- dataCenter$pl[Date > dateFrom & Date <= dateBased]
    pl <- portInfo[pl, on = "HS_Port_Code"]
    pl[if_FC_HS_Port == TRUE | 
         Sec_Code %in% c("990045.IB", "990025.IB", "004091.OF", "519139.OF"),
       if_FC := 1]
    pl[, if_FC := GCAMCPUB::na_fill(if_FC, 0)]
    attachHSPortInfo(pl, c("Port_Name", "Port_Type"))
    pl <- pl[Port_Type  == "Cost based" & Class_L3 %in% classEQ]
    pl <- pl[, .(PL = sum(PL_OC)), .(Port_Name, if_FC)] %>%
      dcast(Port_Name ~ if_FC, fun = sum, value.var = "PL") %>%
      setnames(c("Port_Name", "PL_Dom", "PL_Overs"))
    pl
  })
  eqPos <- local({
    ports <- paPorts(as.list(c("CNPC", "Par", "Life", "Capital RMB", "UV Individual",
                               "UV Group", "All cost based")),
                     names = c("CNPC", "Par", "Life", "Capital RMB","UV Individual", 
                               "UV Group", "Total"))
    avc <- portanalytics::avc(ports, c(dateFrom, dateBased), ts = F) 
    avc <- avc[, .(Port_Name = Item, AVC)]
    target <- dcast(saaTarget, Port_Name ~ SAA_Class, value.var = "Target")
    eqPos <- avc[target, on = "Port_Name"]
    eqPos[, `:=`(
      Dom = `Domestic listed equities` * AVC,
      Overs = `Overseas listed equities` * AVC,
      Total = `Listed equities` * AVC
    )][, .(Port_Name, Dom, Overs, Total)]
  })
  eqAdj <- uglChg[pl, on = "Port_Name"][eqPos, on = "Port_Name"]
  eqAdj[, `:=`(
    EQ_Adj_Dom = (PL_Dom + UGL_Chg_Dom) / Dom,
    EQ_Adj_Overs = (PL_Overs + UGL_Chg_Overs) / Overs,
    EQ_Adj_Total = (PL_Dom + UGL_Chg_Dom + PL_Overs + UGL_Chg_Overs) / Total
  )]
  addRow <- eqAdj[, .(UGL_Chg_Dom = sum(UGL_Chg_Dom),
                      UGL_Chg_Overs = sum(UGL_Chg_Overs),
                      PL_Dom = sum(PL_Dom),
                      PL_Overs = sum(PL_Overs),
                      Dom = sum(Dom),
                      Overs = sum(Overs),
                      Total = sum(Total))]
  addRow[, `:=`(
    Port_Name = "Total",
    EQ_Adj_Dom = (PL_Dom + UGL_Chg_Dom) / Dom,
    EQ_Adj_Overs = (PL_Overs + UGL_Chg_Overs) / Overs,
    EQ_Adj_Total = (PL_Dom + UGL_Chg_Dom + PL_Overs + UGL_Chg_Overs) / Total
  )]
  eqAdj <- rbindlist(list(eqAdj, addRow), use.names = TRUE)
})
