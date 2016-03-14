# for the update of monthly performance report
# Date:2016.03
# the procedure is like this: firstly, calculate common used parameters like AVC, PL, delta UGL...
# and create different functions to solve different tables one by one using the above paras. 
# the raw data fetched for SQL contained the port "CNPC-UV"


library(GCAMCPUB)
library(data.table)
library(dplyr)
library(R6)
Report_to_Top_Mgt <- R6Class(
  "Report_to_Top_Mgt",
  public = list(
    data = NULL,
    initialize = function(path = NULL, DateFrom, DateTo, CurrentDate){
      if (!is.null(path)) {
        self$load(path)
        return(invisible())
      }
      CurrentDate <- GCAMCPUB::to_date(CurrentDate)
      db_mo <- GCAMCPUB::activate_conn(db_mo)
      ##holding for cost based
      sql <- "select Date, Port_Name, AV_Book_LC, AV_Mix_LC from 
      CORE_Data_Holding where Date = '%s' and 
      Port_Type in ('Cost Based', 'Cost Based FC')"
      sql <- sprintf(sql, DateFrom)
      self$data$holding <-
        DBI::dbGetQuery(db_mo, sql) %>%
        dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
        data.table::setDT() 
      ##CF external for cost based
      sql <- "select Date, Port_Name,sum(CF_LC) as CF, Port_Type
      from CORE_Data_CF_External where Date > '%s' and Date <= '%s'
      and Port_Type in ('Cost based', 'Cost based FC')
      group by Date, Port_Name, Port_Type
      order by Date, Port_Name, Port_Type"
      sql <- sprintf(sql, DateFrom, CurrentDate)
      self$data$CF <-
        DBI::dbGetQuery(db_mo, sql) %>%
        dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
        data.table::setDT() 
      
      self$data$AVC_annual = local({
        AV_start <- self$data$holding[Port_Name != 'CNPC-UV', list(AV_start = sum(AV_Book_LC)),
                                      by = Port_Name] # sum up the AV_Book_LC group by Port_Name
        AV_start <- AV_start[private$nameseq, on = 'Port_Name'] # sort the 6 ports at the fixed sequence
        CF <- self$data$CF[Port_Name != 'CNPC-UV']
        TW_CF_annual <- CF[, CF_AVG := CF * as.numeric((DateTo - Date)) / 
                             as.numeric((DateTo - DateFrom))]
        TW_CF_annual <- TW_CF_annual[, list(CFavg_annual = sum(CF_AVG)), by = Port_Name] %>%
          setkey("Port_Name")
        AVC <- TW_CF_annual[AV_start]
        AVC <- AVC[, CFavg_annual := GCAMCPUB::na_fill(CFavg_annual, 0.0)]
        AVC <- AVC[, AVC_annual := AV_start + CFavg_annual]
        select(AVC, Port_Name, AVC_annual)
      })
      self$data$AVC_ytd = local({
        AV_start <- self$data$holding[Port_Name != 'CNPC-UV', list(AV_start = sum(AV_Book_LC)),
                                      by = Port_Name] # sum up the AV_Book_LC group by Port_Name
        AV_start <- AV_start[private$nameseq, on = 'Port_Name'] # sort the 6 ports at the fixed sequence
        CF <- self$data$CF[Port_Name != 'CNPC-UV']
        TW_CF_ytd <- CF[, CF_AVG := CF * as.numeric((CurrentDate - Date)) / 
                             as.numeric((CurrentDate - DateFrom))]
        TW_CF_ytd <- TW_CF_ytd[, list(CFavg_ytd = sum(CF_AVG)), by = Port_Name] %>%
          setkey("Port_Name")
        AVC <- TW_CF_ytd[AV_start]
        AVC <- AVC[, CFavg_ytd := GCAMCPUB::na_fill(CFavg_ytd, 0.0)]
        AVC <- AVC[, AVC_ytd := AV_start + CFavg_ytd]
        select(AVC, Port_Name, AVC_ytd)
      })
      
    },
    save = function (path) {
      if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
      saveRDS(list(holding = self$data$holding,
                   CF = self$data$CF), path)
      invisible()
    },
    load = function (path) {
      if (!file.exists(path)) return(list(error = "load file doesn't exists"))
      r <- readRDS(path)
      self$data$holding <- r$holding
      self$data$CF <- r$CF
      #### return ####
      invisible()
    },
    AVC_ytd = local({
      
    }),
    port_RMB = function(){
     
    }
  ),
  private = list(
    nameseq = data.table(c('CNPC', 'Par', 'Life', 'Capital RMB',
                               'UV Individual', 'UV Group')) %>%
      setnames('Port_Name')
  )
)
report_data <- Report_to_Top_Mgt$new(
  path = NULL, DateFrom = as.Date('2015-12-31'), 
  DateTo = as.Date('2016-12-31'),
  CurrentDate = as.Date('2016-02-29'))
report_data$save("D:/R/report_test.rds")
report_data <- Report_to_Top_Mgt$new(
  path = "D:/R/report_test.rds", 
  DateFrom = as.Date('2015-12-31'), 
  DateTo = as.Date('2016-12-31'),
  CurrentDate = as.Date('2016-02-29'))