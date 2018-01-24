library(portanalytics)
library(data.table)
ptfs <- c("CNPC", "Par", "UL Increase", "UV Group", "UV Individual", "UL Stable",
          "Life", "Capital RMB", "UL Anyi", "UL Strategy", "UL Aggressive",
          "CNPC-UV", "Capital FX")
readFromLdc(c("201412", "201712"), ports = ptfs, jyDateRange = NULL)

dateFrom <- as.Date("2015-12-31")
dateTo <- as.Date("2016-12-31")

tradingDates <- GCAMCPUB::get_trading_day(dateTo, dateFrom, type = "a-shares")

wt <- 
  astAlloc(
    ports = ptfs %>% purrr::map(~list(., "Domestic")) %>% paPorts(ptfs), 
    dates = c(dateFrom, dateTo),
    assets = "Equity",
    groupBy = "Class_L1",
    ts = TRUE
  )
print(wt[, .(AVG = mean(Weight) * 100L, 
             MAX = max(Weight) * 100L, 
             MIN = min(Weight) * 100L), keyby = Item])


drt <- 
  assetRtn(
    ports = ptfs %>% purrr::map(~list(., "Domestic")) %>% paPorts(ptfs), 
    dates = c(dateFrom, dateTo),
    assets = "Equity",
    method = "Time weighted",
    bchmk = "000300.SH"
  )

param <- data.table(
  PTF = c("CNPC", "Par", "UL Increase", "UV Group", "UV Individual", "UL Stable",
          "Life", "Capital RMB", "UL Anyi", "UL Strategy", "UL Aggressive"),
  LOW =  c(0,  0,  20, 0,  0,  0,  0,  0,  0,  20, 45),
  TGT =  c(4,  6,  50, 7,  7,  10, 5,  5,  10, 50, 80),
  HIGH = c(10, 15, 80, 15, 15, 20, 15, 15, 20, 80, 100),
  key = "PTF"
)
cashRtn <- 3 / 100
mgtFee <- 1.5 / 100

# param <- data.table(
#   PTF = c("Par", "UL Increase", "UV Group", "UV Individual", "UL Stable"),
#   LOW = c(0, 20, 0, 0, 0),
#   TGT = c(7, 40, 5, 4, 10),
#   HIGH = c(14, 60, 10, 8, 20),
#   key = "PTF"
# )
allocL <- 20
allocT <- 60
allocH <- 95


res <- drt[Date_To %in% tradingDates, .(PTF = Item, DATE = Date_To,
                                        DRT = Daily_Rtn, BDRT = Bchmk_DR)]
res[, EQ_WT := wt[res, Weight * 100L, on = c("Item" = "PTF", "Date_To" = "DATE")]]
res[, EQ_WT := GCAMCPUB::na_fill(EQ_WT, 0)]
res <- param[res, on = "PTF"]
res[, SIM_WT := {
  wt <- EQ_WT
  wt[wt < LOW] <- LOW[wt < LOW]
  wt[wt > HIGH] <- HIGH[wt > HIGH]
  
  lowFlag <- (wt < LOW)
  wt[lowFlag] <- (allocT - allocL) / 
    (TGT[lowFlag] - LOW[lowFlag]) * 
    (wt[lowFlag] - LOW[lowFlag]) + allocL
  highFlag <- (wt >= LOW)
  wt[highFlag] <- allocH - (allocH - allocT) / 
    (HIGH[highFlag] - TGT[highFlag]) * 
    (HIGH[highFlag] - wt[highFlag])
  
  wt
}]
res[, DCOUNT := as.integer(DATE - shift(DATE)), keyby = PTF]
res[, SIM_BDRT := BDRT * allocT / 100 + cashRtn / 365 * DCOUNT * (1 - allocT / 100) ]
res[, SIM_DRT := DRT * SIM_WT / 100 + cashRtn / 365 * DCOUNT * (1 - SIM_WT / 100) -
      mgtFee / 365 * DCOUNT]
res[, SIM_CRT := cumprod(1 + GCAMCPUB::na_fill(SIM_DRT, 0)) - 1, keyby = PTF]
res[, EXCESS_DRT := SIM_DRT - SIM_BDRT]
res[, SIM_BCRT := cumprod(1 + GCAMCPUB::na_fill(SIM_BDRT, 0)) - 1, keyby = PTF]
res[, EXCESS := SIM_CRT - SIM_BCRT]
res[, DD := {
  (SIM_CRT + 1) / (cummax(SIM_CRT) + 1) - 1
}, keyby = PTF]

# breakdown
res[, `:=`(
  SEL = sel <- cumprod(1 + GCAMCPUB::na_fill(DRT - BDRT, 0)) - 1,
  ALLOC =  alloc <- cumprod(GCAMCPUB::na_fill((SIM_WT - 50) / 100 * BDRT, 0) + 1) - 1,
  OTHERS = EXCESS - sel - alloc
), keyby = PTF]

stats <- res[, .(CUMR = last(SIM_CRT),
                 CUMBR = last(SIM_BCRT),
                 EXCESS = last(EXCESS),
                 SEL = last(SEL),
                 ALLOC = last(ALLOC),
                 OTHERS = last(OTHERS),
                 SD = sd(DRT, na.rm = TRUE) * 252 ^ 0.5,
                 MEAN = mean(DRT, na.rm = TRUE) * 252,
                 SD_EXCESS = sd(EXCESS_DRT, na.rm = TRUE) * 252 ^ 0.5,
                 MEAN_EXCESS = mean(DRT, na.rm = TRUE) * 252,
                 MDD = min(DD)),
             keyby = PTF]
stats[, SHARPE := MEAN /SD]
stats[, IR := MEAN_EXCESS / SD_EXCESS]
stats[, DDRATIO := - MEAN / MDD]

print(stats)

resCR <- dcast(res, DATE ~ PTF, value.var = "SIM_CRT")
GCAMCPUB::write_open_xlsx(resCR)



# total  ------------------------------------------------------------------


dateFrom <- as.Date("2016-12-31")
dateTo <- as.Date("2017-12-31")

tradingDates <- GCAMCPUB::get_trading_day(dateTo, dateFrom, type = "a-shares")

wt <- 
  astAlloc(
    ports = "All cost based" %>% purrr::map(~list(., "Domestic")) %>% paPorts("All cost based"), 
    dates = c(dateFrom, dateTo),
    assets = "Equity",
    groupBy = "Class_L1",
    ts = TRUE
  )
print(wt[, .(AVG = mean(Weight) * 100L, 
             MAX = max(Weight) * 100L, 
             MIN = min(Weight) * 100L), keyby = Item])


drt <- 
  assetRtn(
    ports = "All cost based" %>% purrr::map(~list(., "Domestic")) %>% paPorts("All cost based"), 
    dates = c(dateFrom, dateTo),
    assets = "Equity",
    method = "Time weighted",
    bchmk = "000300.SH"
  )

param <- data.table(
  PTF = c("All cost based"),
  LOW =  c(0),
  TGT =  c(4),
  HIGH = c(10),
  key = "PTF"
)
cashRtn <- 3 / 100
mgtFee <- 1.5 / 100

# param <- data.table(
#   PTF = c("Par", "UL Increase", "UV Group", "UV Individual", "UL Stable"),
#   LOW = c(0, 20, 0, 0, 0),
#   TGT = c(7, 40, 5, 4, 10),
#   HIGH = c(14, 60, 10, 8, 20)
#   key = "PTF"
# )
allocL <- 20
allocT <- 60
allocH <- 95


res <- drt[Date_To %in% tradingDates, .(PTF = Item, DATE = Date_To,
                                        DRT = Daily_Rtn, BDRT = Bchmk_DR)]
res[, EQ_WT := wt[res, Weight * 100L, on = c("Item" = "PTF", "Date_To" = "DATE")]]
res[, EQ_WT := GCAMCPUB::na_fill(EQ_WT, 0)]
res <- param[res, on = "PTF"]
res[, SIM_WT := {
  wt <- EQ_WT
  wt[wt < LOW] <- LOW[wt < LOW]
  wt[wt > HIGH] <- HIGH[wt > HIGH]
  
  lowFlag <- (wt < LOW)
  wt[lowFlag] <- (allocT - allocL) / 
    (TGT[lowFlag] - LOW[lowFlag]) * 
    (wt[lowFlag] - LOW[lowFlag]) + allocL
  highFlag <- (wt >= LOW)
  wt[highFlag] <- allocH - (allocH - allocT) / 
    (HIGH[highFlag] - TGT[highFlag]) * 
    (HIGH[highFlag] - wt[highFlag])
  
  wt
}]
res[, DCOUNT := as.integer(DATE - shift(DATE)), keyby = PTF]
res[, SIM_BDRT := BDRT * allocT / 100 + cashRtn / 365 * DCOUNT * (1 - allocT / 100) ]
res[, SIM_DRT := DRT * SIM_WT / 100 + cashRtn / 365 * DCOUNT * (1 - SIM_WT / 100) -
      mgtFee / 365 * DCOUNT]
res[, SIM_CRT := cumprod(1 + GCAMCPUB::na_fill(SIM_DRT, 0)) - 1, keyby = PTF]
res[, EXCESS_DRT := SIM_DRT - SIM_BDRT]
res[, SIM_BCRT := cumprod(1 + GCAMCPUB::na_fill(SIM_BDRT, 0)) - 1, keyby = PTF]
res[, EXCESS := SIM_CRT - SIM_BCRT]
res[, DD := {
  (SIM_CRT + 1) / (cummax(SIM_CRT) + 1) - 1
}, keyby = PTF]

# breakdown
res[, `:=`(
  SEL = sel <- cumprod(1 + GCAMCPUB::na_fill(DRT - BDRT, 0)) - 1,
  ALLOC =  alloc <- cumprod(GCAMCPUB::na_fill((SIM_WT - 50) / 100 * BDRT, 0) + 1) - 1,
  OTHERS = EXCESS - sel - alloc
), keyby = PTF]

stats <- res[, .(CUMR = last(SIM_CRT),
                 CUMBR = last(SIM_BCRT),
                 EXCESS = last(EXCESS),
                 SEL = last(SEL),
                 ALLOC = last(ALLOC),
                 OTHERS = last(OTHERS),
                 SD = sd(DRT, na.rm = TRUE) * 252 ^ 0.5,
                 MEAN = mean(DRT, na.rm = TRUE) * 252,
                 SD_EXCESS = sd(EXCESS_DRT, na.rm = TRUE) * 252 ^ 0.5,
                 MEAN_EXCESS = mean(DRT, na.rm = TRUE) * 252,
                 MDD = min(DD)),
             keyby = PTF]
stats[, SHARPE := MEAN /SD]
stats[, IR := MEAN_EXCESS / SD_EXCESS]
stats[, DDRATIO := - MEAN / MDD]
print(stats)
