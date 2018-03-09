
pos <- dataCenter$pos[Class_L3 %in% class_eq, 
                      .(Date, Class_L3, Sec_Code, Sec_Name, Quantity, AV_Mix_LC)]
trans <- dataCenter$trans[Class_L3 %in% class_eq,
                          .(Date, Class_L3, Sec_Code, Sec_Name, Quantity, Trans_Type, CF_LC)]

attach_label <- function(data, tan_sec_code){
  data[Sec_Code %in% tan_sec_code & abs(Quantity) < 150000, Label := "Tan"]
  data[is.na(Label), Label := "Hu"]
  data
}

pos_old <- attach_label(pos, tan_sec_code)
trans <- attach_label(trans, tan_sec_code)



class_fi <- c("Pure bond fund", "Corporate bond", 'IAMP-FI-NAV', 'Policy financial bond',
              "A of fund", "CD")
avg_av <- local({
  pos <- dataCenter$pos[Date > date_from & Date <= date_to]
  pos[Class_L3 %in% class_fi, Class_L1 := "FI"]
  pos[is.na(Class_L1) & Class_L3 %in% class_eq, Class_L1 := "EQ"]
  pos <- pos[, .(AV_Mix_LC = sum(AV_Mix_LC)),
             by = .(Date, Class_L1)]
  pos
})
avg_av_total <- avg_av[, .(AV_Mix_LC = sum(AV_Mix_LC)), by = .(Date)]
avg_av_total <- mean(avg_av_total$AV_Mix_LC)

avg_av_fi <- avg_av[Class_L1 == "FI", .(AV_Mix_LC = mean(AV_Mix_LC))]$AV_Mix_LC
avg_av_eq <- avg_av[Class_L1 == "EQ", .(AV_Mix_LC = mean(AV_Mix_LC))]$AV_Mix_LC



pl_fi <- dataCenter$pl[Date > date_from & Date <= date_to &
                         Class_L3 %in% class_fi, .(PL_LC = sum(PL_LC))]$PL_LC

avg_pos_tan <- mean(pos[Label == "Tan", .(AV_Mix_LC = sum(AV_Mix_LC)), by = .(Date)]$AV_Mix_LC)
avg_pos_hu <- mean(pos[Label == "Hu", .(AV_Mix_LC = sum(AV_Mix_LC)), by = .(Date)]$AV_Mix_LC)





# new code ----------------------------------------------------------------

library(portanalytics)
library(GCAMCPUB)
library(readxl)
library(data.table)
readFromLdc(c(201612, 201707), "UL Anyi", jyDateRange = NULL)
tan_stock <- read_xlsx("C:/Users/AMC161/Desktop/anyi stock_tan.xlsx") %>% setDT()
tan_sec_code <- tan_stock$Sec_Code
tan_stock[, Date := to_date(Date)]

date_from <- as.Date("2016-12-31")
date_to <- as.Date("2017-07-05")

class_eq <- c("Stock", "Index fund", "IAMP-Stock", "B of bond fund")

# divide pos into 2 parts based on tan_stock

tan_holding_stocks <- function(date) {
  
  tan_stock[Date <= date, .(Quantity = sum(Quantity)), keyby = Sec_Code][Quantity > 0]
}
# tan_holding_stocks(lubridate::ymd(20170101))


pos <- dataCenter$pos[Class_L3 %in% class_eq & Date >= date_from & Date <= date_to, 
                      .(Date, Class_L3, Sec_Code, Sec_Name, Quantity, AV_Mix_LC)]

pos_tan <- vector("list", uniqueN(pos$Date))

dates <- sort(unique(pos$Date))
dates <- dates[dates >= lubridate::ymd(20161231)]
for (i in seq_along(pos_tan)) {
  
  date <- dates[i]
  exp_stock <- tan_holding_stocks(date)
  r <- pos[Date == date &  Sec_Code %in% exp_stock[["Sec_Code"]]]
  r[, EXP_QTY := exp_stock[J(r$Sec_Code), Quantity]]
  pos_tan[[i]] <- r
}
pos_tan <- rbindlist(pos_tan) %>% setkey(Date, Sec_Code, Quantity, AV_Mix_LC)
setkey(pos, Date, Sec_Code, Quantity, AV_Mix_LC)
pos[J(pos_tan), Label := "Tan"]
pos[Class_L3 %in% class_eq & is.na(Label), Label := "Hu"]
pos_hu <- pos[Label == "Hu"]


#divide pl into 2 parts based on tan_stock

tan_stock_tmp <- copy(tan_stock)
tan_taock_tmp <- tan_stock_tmp[Trans_Type == enc2utf8("卖出"), Date := Date + 1]
tan_pl_stocks <- function(date) {
  
  tan_stock_tmp[Date <= date, .(Quantity = sum(Quantity)), keyby = Sec_Code][Quantity > 0]
}

pl <- dataCenter$pl[Class_L3 %in% class_eq,
                    .(Date, Class_L3, Sec_Code, Sec_Name, PL_LC, PL_Type)]

pl_tan <- vector("list", uniqueN(pl$Date))

for (i in seq_along(pl_tan)) {
  
  date <- dates[i]
  exp_stock <- tan_pl_stocks(date)
  pl_tan[[i]] <- pl[Date == date & Sec_Code %in% exp_stock[["Sec_Code"]]]
}
pl_tan <- rbindlist(pl_tan) %>% setkey(Date, Sec_Code, PL_LC, PL_Type)
pl_total <- dataCenter$pl[Class_L3 %in% class_eq & Date >= date_from & PL_LC != 0, 
                          .(Date, Sec_Code, Sec_Name, Class_L3, PL_Type, PL_LC)] %>%
  setkey(Date, Sec_Code, PL_LC, PL_Type)

pl_total[pl_tan, Label := "Tan"]
pl_total[is.na(Label), Label := "Hu"]
pl_hu <- pl_total[Label == "Hu"]


# divide trans into 2 parts
trans_tan <- tan_stock[, .(Date, Trans_Type, Sec_Code, Sec_Name, Quantity, CF_LC)] %>%
  setkey(Date, Sec_Code, Quantity)
trans_total <- dataCenter$trans[Class_L3 %in% class_eq & Date > date_from & Date <= date_to,
                                .(Date, Trans_Type, Sec_Code, Sec_Name, Quantity, CF_LC)]
# trans_total[, `:=`(
#   Trans_Type = enc2utf8(Trans_Type),
#   Sec_Code = enc2utf8(Sec_Code)
# )]
setkey(trans_total, Date, Sec_Code, Quantity)
# .......
trans_total[trans_tan, Label := "Tan"]
trans_total[is.na(Label) & Trans_Type != "会计冲调", Label := "Hu"]
trans_total[is.na(Label) & Sec_Code %in% pos_tan$Sec_Code, Label := "Tan"]
print(trans_total[Sec_Code == "600219.SH"])
trans_total[Sec_Code == "600219.SH" & Date > as.Date("2017-01-09"), Label := "Hu"]
print(trans_total[Sec_Code == "600219.SH"])
trans_total[is.na(Label), Label := "Hu"]
trans_hu <- trans_total[Label == "Hu"]


# time weighted rtn -------------------------------------------------------


# define tw_rtn function
tw_rtn <- function(pos, trans, date_from, date_to){
  pos <- pos[Date >= date_from & Date <= date_to, 
             .(AV_Mix_LC = sum(AV_Mix_LC)),
             by = .(Date)] %>% setkey(Date)
  trans <-  trans[Date > date_from & Date < date_to,
                  .(CF_Neg = sum((CF_LC < 0) * -CF_LC), 
                    CF_Pos = sum((CF_LC > 0) * CF_LC)), 
                  by = .(Date)] %>% setkey(Date)
  pos <- trans[pos]
  pos[, `:=`(
    CF_Neg = na_fill(CF_Neg, 0),
    CF_Pos = na_fill(CF_Pos, 0)
  )]
  pos[, DR:= (AV_Mix_LC + CF_Pos) / (shift(AV_Mix_LC, n = 1L, "lag") + CF_Neg) - 1]
  pos[, DR:= na_fill(DR, 0)]
  pos[, Cum_Rtn := cumprod(DR + 1) - 1]
  pos
}



anyi_rtn <- tw_rtn(pos, trans, date_from, date_to)
anyi_rtn_tan <- tw_rtn(pos_tan, trans_tan, date_from, date_to)
anyi_rtn_hu <- tw_rtn(pos_hu, trans_hu, date_from, date_to)

