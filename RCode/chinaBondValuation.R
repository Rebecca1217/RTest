library(data.table)
library(GCAMCPUB)



port <- "Credit Bond No2"
date_tgt <- as.Date("2017-12-04")

pos <- local({
  
  db_mo <- activate_conn("db_mo")
  on.exit(DBI::dbDisconnect(db_mo))
  
  sql <- "Select Date, Sec_Code, Sec_Name, Class_L3, Quantity, AV_Mix_LC, 
  DirtyPrice_CNBD, NetPrice_CNBD
  from CORE_Data_Holding
  where Port_Name = '{{port}}'"
  sql <- infuser::infuse(sql, port = port)
  
  pos <- sql2dt(db_mo, sql)
  
  pos[!is.na(DirtyPrice_CNBD), Valuation_CB := DirtyPrice_CNBD * Quantity]
  pos[, `:=`(
    Date = to_date(Date),
    AV_Mix_Adj = ifelse(!is.na(Valuation_CB), Valuation_CB, AV_Mix_LC)
  )]
  
  pos
})


share <- local({
  
  db_mo <- activate_conn("db_mo")
  on.exit(DBI::dbDisconnect(db_mo))
  
  sql <- "Select Date, Port_Name, Share from CORE_Data_Share where Port_Name = '{{port}}'"
  sql <- infuser::infuse(sql, port = port)
  
  share <- sql2dt(db_mo, sql)
  share[, Date := to_date(Date)]
  share
})


div <- local({
  

  
  sql <- "Select Port_Name, F_Unit_Profit, F_Total_Share, F_Total_Profit, Reg_Date as Date
  from CORE_para_Div_Info where Port_Name = '{{port}}'"
  sql <- infuser::infuse(sql, port = port)
  
  div <- sql2dt(db_mo, sql)
  div[, Date := to_date(Date)]
  
  div
})


div_mal_position <- local({
  
  db_mo <- activate_conn("db_mo")
  on.exit(DBI::dbDisconnect(db_mo))
  
  sql <- "Select Date, F_Unit_Profit as Unit_Mal_Position from CORE_para_Div_Malposition 
  where Port_Name = '{{port}}'"
  sql <- infuser::infuse(sql, port = port)
  mal_position <- sql2dt(db_mo, sql)
  
  mal_position[, Date := to_date(Date)]
  
  mal_position
})

# Calculate unit price
AV <- local({
  
  AV <- pos[, .(AV = sum(AV_Mix_Adj)), by = .(Date)]
  AV <- share[, .(Date, Share)][AV, on = "Date"]
  setorder(AV, Date)
  AV[, Cum_Share := cumsum(GCAMCPUB::na_fill(Share, 0))]
  AV[, Unit_Price := AV / Cum_Share]
  setkey(AV, Date)
  
  AV
})


# Div adjust
if(duplicated(div[, .(Date)])) stop("Now it's not suitable for structured ports yet!")

AV <- local({
  
  setkey(div, Date)
  
  AV <- div[, .(Date, F_Unit_Profit)][AV]
  
  AV <- div_mal_position[AV, on = "Date"]
  AV[, F_Unit_Profit := cumsum(na_fill(F_Unit_Profit, 0))]
  AV[, Unit_Price_Adj := Unit_Price + na_fill(F_Unit_Profit, 0) - na_fill(Unit_Mal_Position, 0)]
  
  AV[, .(Date, AV, Unit_Price, Unit_Price_Adj)]
})

write_open_xlsx(AV)



