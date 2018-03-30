
# Note --------------------------------------------------------------------

# This is only used after 2016.12.31.
# If you need the info before that, go find it in MORM-JinTai_MgtFee.R, which
# contains some adjustments.

## If there is problem ,first check the avc ,if there is money from QuanWeiTuo
# to FeiBiao but you didn't identify them.

# load --------------------------------------------------------------------

library(data.table)
library(GCAMCPUB)
library(portanalytics)
date_from <- f_date_begin(Sys.Date(), "year") - 1
last_pos_date <- (function() {
  db_mo <- GCAMCPUB::activate_conn(db_mo)
  on.exit(DBI::dbDisconnect(db_mo))
  sql <- "select max(Date) as Max_Date from CORE_Data_Holding"
  max_date <- DBI::dbGetQuery(db_mo, sql)$Max_Date %>% to_date
  max_date
})()
date_tgt <- last_pos_date
date_to <- f_date_end(date_tgt)
pn <- "JinTai"
ports <- paPorts(pn)
dates <- c(date_from, date_tgt)
readFromLdc(dates, ports = ports)

# param -------------------------------------------------------------------

# basis fee has beened included in PL (both quanweituo & feibiao)
mgt_fee_yijiti <- 0
email_pwd <- "gcamc03!"

if(month(date_tgt) %in% c(4, 7, 10, 12)) {
  GCAMCPUB::f_msg("Make sure the mgt_fee_yijiti is complete.")
}

secu_feibiao <- 
  readr::read_csv("E:/RWD/RTest/external_data/jintai_feibiao.csv") %>%
  dplyr::filter(!is.na(Flag_FeiBiao)) %>%
  setDT(., key = "Sec_Code")

stopifnot(identical(
  anyDuplicated(secu_feibiao),
  0L
))

# decide non standard assets ----------------------------------------------

secu_pool <- local({
  tmp <- coreQuery(c("pos", "trans"), 
                   ports = ports, 
                   dates = dates, 
                   assets = paAssets("All non cash-like assets"))
  res <- tmp$pos[!Class_L3  %in% c("Stock", "MMF"), 
                 .(First_Date = min(Date)), 
                 keyby = .(Class_L3, Sec_Code, Sec_Name)]
  res2 <- tmp$trans[Trans_Type == "买入" & !Class_L3 %in% c("Stock", "MMF"), 
                    .(Buy_Dates = paste0(Date, collapse = ",")), 
                    keyby = .(Sec_Code)]
  res2[res, on = "Sec_Code"][, .(Sec_Code, Sec_Name, Class_L3, First_Date, Buy_Dates)]
})

# check if any new security needed label
local({
  res <- secu_pool[!Sec_Code %in% secu_feibiao$Sec_Code]
  if (nrow(res) > 0) {
    readr::write_csv(res, "E:/RWD/Port-Reports/external_data/jintai_feibiao.csv", append = TRUE)
    stop("there're new securities needs to mannually identify, ",
         "check `external_data/jintai_feibiao.csv`")
  }
  res <- copy(secu_pool)
  res[,  Buy_Dates_0 := secu_feibiao[J(res$Sec_Code), Buy_Dates]]
  res <- res[Buy_Dates != Buy_Dates_0]
  if (nrow(res) > 0) {
    GCAMCPUB::f_msg("The following securities Buy_Dates chgs, be sure the flag is valid.")
    print(res)
  }
})

secu_pool[, Flag_FeiBiao := secu_feibiao[J(secu_pool$Sec_Code), Flag_FeiBiao]]
secu_feibiao <- secu_pool[Flag_FeiBiao == TRUE, Sec_Code]


#### average capital ####

GCAMCPUB::f_msg("------------------------------------\n",
                "here needs manual check. Need to confirm the source of every Feibiao product.
                If it comes from QuanWeiTuo, remember to add it to cfExt!")

# av0

av_begin <- dataCenter$pos[!Sec_Code %in% secu_feibiao & Date == date_from,
                          .(AV_Mix_LC = sum(AV_Mix_LC))]$AV_Mix_LC

# cf (ext & int)
cf_total <- (function(){
  # external cf
  cf_ext <- coreQuery("cfExt", ports = ports, dates = c(date_from, date_to))$cfExt
  print(cf_ext)
  cf_ext <- cf_ext[Notes %in% c("收到委托投资款", "银行存款提取")]
  # exclude money which is solely used for purcashing a trust
  cf_ext <- cf_ext[!Date %in% c(as.Date("2016-08-01"), 
                                as.Date("2017-03-31"),
                                as.Date("2017-04-25"),
                                as.Date("2018-03-16"),
                                as.Date("2018-03-20"))] 
  print(cf_ext)
  # cf from coupon and maturity and sell feibiao
  cf_int1 <- coreQuery("trans", ports = ports, dates = c(date_from, date_to))$trans[
    Sec_Code %in% secu_feibiao & !Trans_Type %in% c("买入")
    ]
  print(cf_int1[, .(Date, Sec_Name, Trans_Type, CF_LC)])
  
  # cf from quanweituo going to feibiao
  cf_int2 <- coreQuery("trans", ports = ports, dates = c(date_from, date_to))$trans[
    Sec_Code %in% secu_feibiao & Trans_Type %in% c("买入")
    ]
  print(cf_int2[, .(Date, Sec_Name, Trans_Type, CF_LC)])
  GCAMCPUB::f_msg("Check if all feibiao capital comes from CF External.")
  check_cf <- local({
    cf_ext <- coreQuery("cfExt", ports = ports, dates = c(date_from, date_to))$cfExt
    cf_ext_feibiao <- cf_ext[!Notes %in% c("收到委托投资款", "银行存款提取")]
    cf_ext_feibiao <- rbindlist(list(cf_ext_feibiao, 
                                     cf_ext[Date %in% c(as.Date("2017-03-31"),
                                                        as.Date("2017-04-25"),
                                                        as.Date("2018-03-16"),
                                                        as.Date("2018-03-20"))]))
    all(-(cf_int2$CF_LC) %in% cf_ext_feibiao$CF_LC)
    # @2018.03.22, money buying feibiao coming from both external and quanweituo sell IAMP-FI-NAV
  })
  if(!check_cf) {
    cf_ext <- coreQuery("cfExt", ports = ports, dates = c(date_from, date_to))$cfExt
    cf_ext_feibiao <- cf_ext[!Notes %in% c("收到委托投资款", "银行存款提取")]
    cf_ext_feibiao <- rbindlist(list(cf_ext_feibiao, 
                                     cf_ext[Date %in% c(as.Date("2017-03-31"),
                                                        as.Date("2017-04-25"),
                                                        as.Date("2018-03-16"),
                                                        as.Date("2018-03-20"))]))
    cf_int2 <- cf_int2[!-CF_LC %in% cf_ext_feibiao$CF_LC, 
                       .(Date, Sec_Name, Trans_Type, CF_LC)]
    # cf_int2[Date == "2018-03-22" & Sec_Name == "光大信托保诚3号信托计划",
    #         CF_LC := CF_LC + 6e07]
  } else {
    cf_int2 <- cf_int2[Sec_Name == "empty table"]
  }
  print(cf_int2[, .(Date, Sec_Name, Trans_Type, CF_LC)])
  # sum up
  
  cf_total <- rbindlist(list(
    cf_ext[, .(Date, CF = CF_LC, Notes)],
    cf_int1[, .(Date, CF = CF_LC, Notes = paste(Sec_Name, Trans_Type, sep = "-"))],
    cf_int2[, .(Date, CF = CF_LC, Notes = paste(Sec_Name, Trans_Type, sep = "-"))]
  ))
  
  cf_total[, `:=`(
    Weight = weight <- as.integer(date_tgt - Date) / as.integer(date_tgt - date_from)
  )]
  cf_total
})()

avc <- (av_begin + cf_total[, sum(CF * Weight)]) %>% round(., 2)

#### pl ####

av_end <- 
  coreQuery("pos", ports = ports, dates = date_tgt)$pos[
    !Sec_Code %in% secu_feibiao, sum(AV_Mix_LC)]

pl_from_cf <- (av_end - cf_total[, sum(CF)] - av_begin) %>% round(., 2)

comp_pl <- local({
  pl <- coreQuery("pl", ports = ports, dates = c(date_from, date_tgt))$pl[
    !Sec_Code %in% secu_feibiao, sum(PL_LC)]
  # use date_from +1 doesn't contain 2017.01.03 data.Why?
  
  ugl_end <- coreQuery("pos", ports = ports, dates = date_tgt)$pos[
    !Sec_Code %in% secu_feibiao & IAS == 'AFS', sum(UGL_LC)]
  
  ugl_begin <- coreQuery("pos", ports = ports, dates = date_from)$pos[
    !Sec_Code %in% secu_feibiao & IAS == 'AFS', sum(UGL_LC)]
  
  pl + ugl_end - ugl_begin
})

stopifnot(all.equal(pl_from_cf, comp_pl))

#### basis fee ####

basis_fee <- (avc * 0.30 / 100 * as.integer(date_tgt - date_from) / 365) %>% round(., 2)

#### inv rtn ###

# +yijiti(feibiao & quanweituo) - yingjiti(quanweituo from date_begin)
inv_rtn <- ((pl_from_cf - basis_fee + mgt_fee_yijiti) / avc) %>% round(., 4)
ann_rtn <- (inv_rtn * 365 / as.integer(date_tgt - date_from)) %>% round(., 4)
perf_fee <- 
  (avc * max(0, 0.15 * (ann_rtn - 0.06)) * as.integer(date_to - date_from) / 365) %>%
  round(., 2)



# TWRR & Dietz asset Rtn --------------------------------------------------

stock_rtn_tw <- assetRtn(ports, dates, "Stock", "Time weighted",
                         bchmk = "000001.SH")[Date_To == date_tgt]
stock_rtn_dietz <- assetRtn(ports, dates, "Stock", "Dietz",
                         bchmk = "000300.SH")[Date_To == date_tgt]
bond_rtn <- assetRtn(ports, dates, "Corporate bond", "Dietz", 
                     bchmk = "CBA00201.CS")[Date_To == date_tgt]

# output ------------------------------------------------------------------

htmlBody <- "
  <body style='color:black;font-size:10pt;font-family:微软雅黑;'>
  <p>Dear:</p>
  <p>您好！截至{{date}}，锦泰财险全委托账户净值{{AV_Mix_LC}}元，
  自年初至{{date}}YTD综合收益率为{{rtn}}，
  年化综合收益率为{{ann_rtn}}，业绩考核基准为6%。
  </p>
  其中，股票资产平均资金占用规模{{stock_avc}}元，账面收益{{stock_pl}}元，
  时间加权收益率{{stock_tw}}，同期上证综指{{bchmk_sh}}，沪深300指数{{bchmk_300}}。
  </p>
  债券资产平均资金占用规模{{bond_avc}}元，账面收益{{bond_pl}}元，
  综合收益率{{bond_rtn}}，同期中债综合财富（总值）指数{{bchmk_057}}。
  </p>
  <p>注：产品净值及收益率数据均为初步核算数据，仅供参考。</p>
  <p>(本邮件系程序自动发送，请勿直接回复。)</p>
   </br>
  <p class='small'> <b>中意资产管理有限责任公司</b></p>
  <p class='small' style='font-family:Segoe UI Semibold;'>
  <b>Tel:</b> (010) 56801188 - 8833</p>
  <p class='small' style='font-family:Segoe UI Semibold;'>
  <b>Email:</b> <a href='mailto:Customer.Service@gc-amc.com'>
  Customer.Service@gc-amc.com</a></p>
  </body>"
htmlBody <- infuser::infuse(htmlBody, 
                            date = format(date_tgt, "%Y年%m月%d日"),
                            AV_Mix_LC = f_fmt(av_end, 2),
                            date = format(date_tgt, "%Y年%m月%d日"),
                            rtn = f_fmt_pct(inv_rtn, 2),
                            ann_rtn = f_fmt_pct(ann_rtn),
                            stock_avc = f_fmt(stock_rtn_dietz$AVC, 2),
                            stock_pl = f_fmt(stock_rtn_dietz$Book_PL, 2),
                            stock_tw = f_fmt_pct(stock_rtn_tw$Cum_Rtn, 2),
                            bchmk_sh = f_fmt_pct(stock_rtn_tw$Bchmk_Rtn, 2),
                            bchmk_300 = f_fmt_pct(stock_rtn_dietz$Bchmk_Rtn, 2),
                            bond_avc = f_fmt(bond_rtn$AVC, 2),
                            bond_pl = f_fmt(bond_rtn$Book_PL, 2),
                            bond_rtn = f_fmt_pct(bond_rtn$Cum_Rtn, 2),
                            bchmk_057 = f_fmt_pct(bond_rtn$Bchmk_Rtn, 2))
f_send_email(
  from = "Ruiling.Feng@gc-amc.com",
  to = c("Qi.Shi@gc-amc.com", "Andy.Wang@gc-amc.com"),
  bcc = c("Ruiling.Feng@gc-amc.com"),
  id = "AMC161",
  pwd = email_pwd,
  subject = paste("【自动邮件】锦泰财险-全委托账户-周度报送",
                  format(date_tgt, "%Y.%m.%d")),
  body = htmlBody
)

