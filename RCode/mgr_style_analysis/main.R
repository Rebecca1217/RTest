
library(dplyr)
library(readxl)
library(GCAMCPUB)
library(data.table)

# saved
# from_to <- c(20100101, 20180323)
#
# style_tbl <- local({
#
#   res <- GCAMCQT::factor_tbl(style_vars, from_to = from_to, freq = "weekly", value_col = "VALUE")
#   setnames(res, c("STOCKINNERCODE", "DATE", style_vars))
#
#   res
# })
#
# secumain <- read_gildata("secumain")$secumain
# secumain <- secumain[SECUMARKET %in% c(83, 90) & SECUCATEGORY == 1] %>% setkey(SECUCODE)
#
# hs300_dt <- index_comp_wt("CSI300", from_to, freq = "daily")
# zz500_dt <- index_comp_wt("CSI500", from_to, freq = "daily")
#
# setnames(hs300_dt, c("STOCKINNERCODE", "DATE", "NORMWEIGHT"))
# setnames(zz500_dt, c("STOCKINNERCODE", "DATE", "NORMWEIGHT"))
# hs300_dt[, INNERCODE := "HS300"]
# zz500_dt[, INNERCODE := "ZZ500"]

##
secumain <- readDtRds("./RCode/mgr_style_analysis/secumain.rds") %>% setkey(SECUCODE)
style_tbl <- readDtRds("./RCode/mgr_style_analysis/style_tbl.rds")
hs300_dt <- readDtRds("./RCode/mgr_style_analysis/hs300_dt.rds")
zz500_dt <- readDtRds("./RCode/mgr_style_analysis/zz500_dt.rds")

# once for one port ts data
stock_style_expose <- function(dt) {

  stopifnot(all(c("STOCKINNERCODE", "DATE", "NORMWEIGHT", "INNERCODE") %in% names(dt)),
            length(unique(dt$INNERCODE)) == 1)

  style_dt <- copy(dt)
  style_dt <- style_tbl[style_dt, on = c("STOCKINNERCODE", "DATE"), roll = Inf]

  style_dt[, c(style_expose, "WEIGHT_SQUARE") := purrr::map(.SD, function(x) {
    sum(x * NORMWEIGHT, na.rm = T)
  }), .SDcols = c(style_vars, "NORMWEIGHT"), by = DATE]

  expose_dt <- unique(style_dt[, c("DATE", style_expose), with = FALSE])
  expose_dt <- cbind(data.table(INNERCODE = rep(unique(style_dt$INNERCODE), nrow(expose_dt))),
                     expose_dt)

}

style_vars <- c("bbg_size", "bbg_liquidity", "bbg_value", "bbg_growth", "bbg_momentum", "barra_beta")

style_expose <- c("size_expose", "liquidity_expose", "value_expose", "growth_expose", "momentum_expose", "beta_expose")

# 读入持仓
dt <- read_excel("./RCode/mgr_style_analysis/CNPC.xlsx") %>% as.data.table(.)
dt <- dt[, .(Date, Sec_Code, Port_Name, AV_Mix_LC)] %>% setnames(c("DATE", "STOCKINNERCODE", "INNERCODE", "NORMWEIGHT"))
dt <- dt[!str_right(STOCKINNERCODE, 2) == "HK"]

dt[, DATE := to_date(DATE)]
dt[, STOCKINNERCODE := secumain[J(substr(dt$STOCKINNERCODE, 1, 6)), .(INNERCODE)]]
dt[, NORMWEIGHT := NORMWEIGHT / sum(NORMWEIGHT), by = .(DATE)]

# 风格判断
port_style <- stock_style_expose(dt)
hs300_style <- stock_style_expose(hs300_dt[DATE %in% port_style$DATE])
zz500_style <- stock_style_expose(zz500_dt[DATE %in% port_style$DATE])

style <- rbind(port_style, hs300_style, zz500_style)

