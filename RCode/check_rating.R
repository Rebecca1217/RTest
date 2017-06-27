# The code is used for regularly check the internal rating between CORE System and DW warehouse.
# Since the DW data coming from HengTai rating system, so the rating update will be more timely.
# amdw.v_dw_rating_i is a view, amdw.dw_rating_i is a real table

# pkgs --------------------------------------------------------------------

library(GCAMCPUB)
library(DBI)
library(data.table)

# paras -------------------------------------------------------------------

tgt_date <- as.Date("2017-05-31")
# fetch rating data from oracle
get_dw <- (function(){
  db_dw <- activate_conn("db_dw")
  on.exit(DBI::dbDisconnect(db_dw))
  # get the internal rating (incl. bond & issuer)
  sql <- "Select * from amdw.dw_rating_i where to_date(BDATE, 'YYYY-MM-DD') <= date'{{date}}' and 
  to_date(EDATE, 'YYYY-MM-DD') > date'{{date}}'"
  sql <- infuser::infuse(sql, date = tgt_date)
  internal <- dbGetQuery(db_dw, sql) %>%
    setDT()
  internal[, `:=`(
    EDATE = to_date(EDATE),
    BDATE = to_date(BDATE)
  )]
  internal_bond <- internal[R_TYPE == "BI", .(TCODE, TNAME, CR, ISSUER_NAME, ISSUER_CODE,
                                              BDATE, EDATE)] %>%
    setnames("TCODE", "Sec_Code")
  internal_issuer <- internal[R_TYPE == "EI", .(TCODE, TNAME, CR, BDATE, EDATE)] %>%
    setnames("TNAME", "Issuer")
  mget(c("internal_bond", "internal_issuer"))
})()

# fetch rating data from CORE
core_rating <- (function(){
  db_mo <- activate_conn(db_mo)
  on.exit(DBI::dbDisconnect(db_mo))
  sql <- "
  select Date, Sec_Code, Sec_Name, Class_L3, Rating_Internal, 
  Issuer, Issuer_Rating_Internal, HS_Port_Code from CORE_Data_Holding
  where date = '{{date}}'
  and Class_L1 = 'fixed income' and Class_L3  not in ('bond fund', 'IAMP-FI-NAV', 'IAMP-FI') "
  sql <- infuser::infuse(sql, date = tgt_date)
  core_rating <- dbGetQuery(db_mo, sql) %>%
    setDT()
  core_rating[, Date := to_date(Date)]
  core_rating
})()

fc_port <- (function(){
  db_mo <- activate_conn(db_mo)
  on.exit(DBI::dbDisconnect(db_mo))
  sql <- "
  select distinct HS_Port_Code from CORE_para_Port where if_FC_HS_Port = 1"
  fc_port <- dbGetQuery(db_mo, sql)$HS_Port_Code
  fc_port
})()

# comparison of bond rating
check_bond <- get_dw$internal_bond[core_rating, on = "Sec_Code"][, .(
  Sec_Code, TNAME, Sec_Name, Class_L3, CR, Rating_Internal, BDATE, EDATE
)]
dw_null <- check_bond[is.na(TNAME) & is.na(CR)]
inconsis <- check_bond[!is.na(CR) & CR != Rating_Internal & 
                         ! (CR == "risk free" & Rating_Internal == "Risk_free")]
write_open_xlsx(unique(inconsis))

# comparison of issuer rating

# pick the null issuer and look it up in dw
issuer_null <- core_rating[is.na(Issuer) & ! Class_L3 %in% c("ABS", "CD") &
                             ! HS_Port_Code %in% fc_port]
print(issuer_null)
check_issuer <- get_dw$internal_issuer[core_rating, on = "Issuer"][, .(
  Issuer, Sec_Code, Sec_Name, CR, Issuer_Rating_Internal, BDATE, EDATE
)]
check_issuer <- check_issuer[!is.na(Issuer)]




# match the internal bond with issuer
issuer_code <- unique(internal_issuer[, .(ISSUER_CODE, BDATE, EDATE, CR)])
issuer_name <- unique(internal_issuer[, .(TNAME, BDATE, EDATE, CR)])
internal
