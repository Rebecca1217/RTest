# old Brinsonlibrary(GCAMCPUB)
library(data.table)
#library(RODBC)
library(R6)
library(dplyr)

Brinson_Pfm <- R6Class(
  "Brinson_Pfm",
  public = list(
    data = NULL,
    initialize = function (path = NULL){
      if (!is.null(path)) {
        self$load(path)
        return(invisible())
      }
      db_mo <- GCAMCPUB::activate_conn(db_mo)
      ##holding
      sql <- "select Date, Port_Name, Class_L1, Class_L3, sum(AV_Mix_LC) as AV
      from CORE_Data_Holding where Port_Type = 'Unit Linked' and
      Date >= '2010-12-31'
      group by Date, Port_Name, Class_L1, Class_L3"
      
      self$data$holding <-
        DBI::dbGetQuery(db_mo, sql) %>%
        dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
        data.table::setDT() %>%
        data.table::setnames(c("Port_Name", "Class_L3", "Class_L1"),
                             c("PortName", "ClassL3", "ClassL1"))
      self$data$dates <- sort(unique(self$data$holding$Date))
      ## cf
      sql <- "select Date, Port_Name, Class_L1, Class_L3,
      (case Trans_Type when '买入' then 'Buy'
      when '卖出' then 'Sell' else Trans_Type end) as Trans_Type,
      sum(CF_LC) as CF
      from CORE_Data_Trans
      where  Date >= '2010-12-31'
      and if_Cash = 0 and Port_Type = 'Unit Linked'
      group by Date, Port_Name, Class_L1, Class_L3,
      (case Trans_Type when '买入' then 'Buy'
      when '卖出' then 'Sell' else Trans_Type end)"
      self$data$cf <-
        DBI::dbGetQuery(db_mo, sql) %>%
        dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
        data.table::setDT() %>%
        data.table::setnames(c("Port_Name", "Class_L1", "Class_L3", "Trans_Type"),
                             c("PortName", "ClassL1", "ClassL3", "TransType"))
      ##benchmark
      bchmr_character <- paste('活期存款', '0571.CS','000300', sep = "','")
      sql <- paste("select Date, Bchmk_Class, Bchmk_Value
                   from bchMK where Bchmk_Code in ('", bchmr_character, "')
                   and Date >= '2010-12-31'", sep = "")
      self$data$bchmk <-
        DBI::dbGetQuery(db_mo, sql) %>%
        dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
        data.table::setDT() %>%
        data.table::setnames(c("Bchmk_Class", "Bchmk_Value"), c("BchmkClass", "BchmkValue"))
      ##unitprice
      sql <- "select Date, UL_Name, Unit_Price from unitPrice where Date>= '2010-12-31'
      and UL_Name in ('中意稳健理财投资账户', '中意安逸稳健投资账户', '中意积极进取投资账户',
      '中意增长理财投资账户', '中意策略增长投资账户')"
      ##UnitPrice
      self$data$UnitPrice <-
        DBI::dbGetQuery(db_mo, sql) %>%
        dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
        data.table::setDT() %>%
        data.table::setnames(c("Unit_Price", "UL_Name"), c("UnitPrice", "ULName"))
      
      ###### preparing data ####
      ##更改现金流发生的时间，把周末发生现金流的时间改为下周一
      self$data$cf <-
        local({
          tmp <- data.table::data.table(
            Date = self$data$dates, DateNew = self$data$dates, key = "Date")
          tmp <- tmp[self$data$cf, on = "Date", roll = -Inf]
          tmp[, Date := NULL]
          data.table::setnames(tmp, "DateNew", "Date")
          tmp[, list(CF = sum(CF)), by = .(Date, PortName, ClassL1, ClassL3, TransType)][]
        })
    },
    save = function( path) {
      if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
      saveRDS(list(holding = self$data$holding,
                   cf = self$data$cf,
                   dates = self$data$dates,
                   bchmk = self$data$bchmk,
                   UnitPrice = self$data$UnitPrice), path)
      invisible()
    },
    load = function (path) {
      if (!file.exists(path)) return(list(error = "load file doesn't exists"))
      r <- readRDS(path)
      self$data$dates <- r$dates
      self$data$holding <- r$holding
      self$data$cf <- r$cf
      self$data$bchmk <- r$bchmk
      self$data$UnitPrice <- r$UnitPrice
      #### return ####
      invisible()
    },
    ReturnCompute = function (PortNameTest, DateFrom, DateTo) {
      self$data$ULMatchTable <-
        data.table(c("UL Stable", "UL Anyi", "UL Aggressive",
                     "UL Increase", "UL Strategy"),
                   c("76300126", "76300158", "76300157",
                     "76300127", "76300156"),
                   c("中意稳健理财投资账户", "中意安逸稳健投资账户",
                     "中意积极进取投资账户", "中意增长理财投资账户",
                     "中意策略增长投资账户")) %>%
        setnames(c("PortName", "ULCode", "ULName"))
      ##计算组合收益率
      PortReturn <-
        local({
          ULnamematch <- self$data$ULMatchTable[PortName == PortNameTest, ]$ULName
          UnitPricePeriod <-
            self$data$UnitPrice[Date >= DateFrom & Date <= DateTo &
                                  ULName == ULnamematch, ]
          UnitPriceEnd <- UnitPricePeriod[Date == DateTo, ]$UnitPrice
          UnitPriceFrom <- UnitPricePeriod[Date == DateFrom, ]$UnitPrice
          #return
          UnitPriceEnd / UnitPriceFrom -1
        })
      ##计算benchmark收益率
      ##bchmrk权重对应表
      self$data$bchmrkw <- data.table(c("UL Stable", "UL Anyi", "UL Increase",
                                        "UL Strategy", "UL Aggressive"),
                                      c(0.05, 0.05, 0.5, 0.5, 0.8),
                                      c(0.9, 0.9, 0.45, 0.45, 0.15),
                                      rep(0.05,5)) %>%
        setnames(c("PortName", "Equity", "FI", "Liquidity"))
      bchmk <- Brinson_data$data$bchmk
      ##wb
      wb <-
        local({
          tmp <- self$data$bchmrkw[PortName == PortNameTest, ]
          c(tmp$Equity, tmp$FI, tmp$Liquidity)
        })
      ##Rb
      Rb <-
        local({
          tmp <- bchmk[Date  >= DateFrom & Date <= DateTo, ]
          tmp <- tmp[, DR := BchmkValue / shift(BchmkValue, 1L) - 1, by = .(BchmkClass)]
          tmp[BchmkClass == 'Liquidity', DR := BchmkValue / 365 / 100]
          tmp[Date == DateFrom & BchmkClass == 'Liquidity', DR := 0.0]
          tmp <- tmp[ , DR := GCAMCPUB::na_fill(DR, 0.0)]
          tmp <- tmp[, list(DR = sum(DR * wb)), by = .(Date)]
          tmp <- tmp[, RTN := cumprod(DR+1) - 1]
          tmp$RTN
        })
      QueryDate <- sort(unique(self$data$holding[Date >= DateFrom & Date <= DateTo, ]$Date))
      BchmrkReturn <- Rb[length(QueryDate)]
      ###收益拆分计算
      mix_sum <- local({
        tmp <- self$data$holding[PortName == PortNameTest &
                                   Date >= DateFrom & Date <= DateTo, ]
        holding_sum <- tmp[, list(AV = sum(abs(AV))), by = .(Date, PortName, ClassL1)]
        tmp <- self$data$cf[PortName == PortNameTest & Date >= DateFrom &
                              Date <= DateTo, ] %>%
          mutate(CFpos = ifelse(CF > 0, CF, 0),
                 CFneg = ifelse(CF < 0, -CF, 0))
        cf_sum <- tmp[, list(CFpos = sum(CFpos), CFneg = sum(CFneg)),
                      by = .(Date, PortName, ClassL1)]
        tmp <- cf_sum[holding_sum, on = c("Date", "PortName", "ClassL1")]
        tmp <- mutate(tmp, CFneg = GCAMCPUB::na_fill(CFneg, 0.0),
                      CFpos = GCAMCPUB::na_fill(CFpos, 0.0)) %>%
          arrange(Date) 
        tmp <- tmp[, wp := AV / sum(AV), by = Date]
        tmp <- tmp[, DR_p := (AV + CFpos) / (data.table::shift(AV, 1L) + CFneg) - 1,
                   by = .(ClassL1)] %>%
          mutate(DR_p = GCAMCPUB::na_fill(DR_p, 0.0))
        tmp_bch <- bchmk[Date  >= DateFrom & Date <= DateTo, ]
        tmp_bch <- tmp_bch[, DR := BchmkValue / shift(BchmkValue, 1L) - 1, by = .(BchmkClass)]
        tmp_bch[BchmkClass == 'Liquidity', DR := BchmkValue / 365 / 100]
        tmp_bch[Date == DateFrom & BchmkClass == 'Liquidity', DR := 0.0]
        tmp_bch <- tmp_bch[, DR_b := GCAMCPUB::na_fill(DR, 0.0)]
        tmp_bch <- tmp_bch[, wb := wb] %>%
          setnames("BchmkClass", "ClassL1")
        mix <- tmp_bch[tmp, on = c("Date", "ClassL1")]
        mix <- mix[ClassL1 == 'Liquidity', DR_p := DR_b] %>%
          mutate(deta_DR = DR_p - DR_b,
                 deta_w = wp -wb)
        mix_sum <- mix[, list(deta_w = sum(deta_w * DR_b), 
                              deta_DR = sum(wb * deta_DR)), by = Date]
        mix_sum <- mix_sum[, AllocationEffect := cumprod(deta_w + 1) - 1]
        mix_sum <- mix_sum[, SecuritySelection := cumprod(deta_DR + 1) - 1]
        #return
        mix_sum[]
      })
      AllocationEffect <- mix_sum$AllocationEffect[length(QueryDate)]
      SecuritySelection <- mix_sum$SecuritySelection[length(QueryDate)]
      ExcessReturn <- PortReturn - BchmrkReturn
      Others <- ExcessReturn - AllocationEffect - SecuritySelection
      self$data$result <- data.table(t(c(PortNameTest, PortReturn, BchmrkReturn, ExcessReturn,
                                         AllocationEffect, SecuritySelection, Others))) %>%
        setnames(c("PortName", "PortReturn", "BchmrkReturn", "ExcessReturn",
                   "AllocationEffect", "SecuritySelection", "Others"))
      #return
      self$data$result
    }
)
)
#########test######################

Brinson_data <- Brinson_Pfm$new()
Brinson_data$save("D:/test.rds")
Brinson_data <- Brinson_Pfm$new("D:/test.rds")
#DateTo 需交易日
ReturnResult <- Brinson_data$ReturnCompute(DateFrom = as.Date('2014-12-31'),
                                           DateTo = as.Date('2015-11-23'),
                                           PortNameTest = 'UL Stable')





