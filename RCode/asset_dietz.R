
# Because in FDA the dietz becomes AFS/HFT only, so this file will be used frequently.


# pkgs --------------------------------------------------------------------

library(GCAMCPUB)
library(portanalytics)
library(data.table)
# paras -------------------------------------------------------------------
# Paras for jing ying ji xiao fen xi
bond <- c('Corporate bond', 'Policy financial bond', 'Government bond', 'CBN',
           'Convertible bond',  'Huijin bond', 'Subordinated bond',
           'ST financial bill', 'Other financial bond', 'CD')
# Note : CD is not standard bond and if_deposit = 0, but we deem it as corporate bond
stock <- "Stock"
fund <- c("Active fund", "Index fund", "REITs", "A of fund",
          "B of bond fund", "Pure bond fund")
deposit <- c('Negotiated deposit', 'Structured deposit', 'Term deposit')
debt_plan <- "Debt plan"
trust <- "Trust"
equity_inv <- c("Equity investment", "Pipeline", "PE fund", 'BJ-SH Railway')


# classification
class <- list(bond, stock, fund, deposit, debt_plan, trust, equity_inv)
class_name <- c("bond", "stock", "fund", "deposit", "debt_plan", "trust", "equity_inv")
# Paras for GCL analysis
# bond_plus_deposit <- c('Corporate bond', 'Policy financial bond', 'Government bond', 'CBN',
#                        'Convertible bond',  'Huijin bond', 'Subordinated bond',
#                        'ST financial bill', 'Other financial bond',
#                        'Negotiated deposit', 'Structured deposit', 'Term deposit', 'CD')
# # Note : CD is not standard bond and if_deposit = 0, but we deem it as corporate bond
# bond_fund <- c("A of fund", "B of bond fund", "Pure bond fund")
# non_standard <- c("Debt plan", "IAMP-FI-NAV", "Trust")
# # Note: There is no 'IAMP-FI' in GCL's holding yet.
# equity <- c("Stock", "Active fund", "Index fund", "REITs", "IAMP-Stock",
#             "Equity future")
# pe <- c("Equity investment", "Pipeline", "PE fund", 'BJ-SH Railway')
# 
# # classification
# class <- list(bond_plus_deposit, bond_fund, non_standard, equity, pe)
# class_name <- c("bond_plus_deposit", "bond_fund", "non_standard", "equity", "pe")

# The above classification doesn't contain 'OTC unlisted equity forward' and 'Debt'
# 'OTC unlisted equity forward' is WDYY.IB, and there is no longer 'Debt' in our holding.

# fetch data --------------------------------------------------------------

port <- paPorts(list("All cost based"), "All cost based")
dateFrom <- as.Date("2016-12-31")
dateTo <- as.Date("2017-11-30")
dates <- c(dateFrom, dateTo)
readFromLdc(dates, "All cost based", jyDateRange = NULL)

# dietz rtn compute---------------------------------------------------------

res <- class %>%
  lapply(assetRtn, ports = port, dates = dates, method = "Dietz") %>%
  lapply(last) %>%
  rbindlist()
res[, `:=`(
  Classification = class_name,
  Book_Rtn = Book_PL / AVC
)]

res <- res[, .(Item, Date_From, Date_To, Classification, Book_Rtn, Comp_Rtn = Cum_Rtn)]


write_open_xlsx(res)



