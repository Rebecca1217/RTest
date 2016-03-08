# for the update of monthly performance report
# Date:2016.03

library(GCAMCPUB)
library(data.table)
library(dplyr)
library(R6)
mon_performance_report <- R6Class(
  "mon_performance_report",
  public = list(
    data = NULL,
    initialize = function(path = NULL, DateFrom, DateTo, CurrentDate){
      if (!is.null(path)) {
        self$load(path)
        return(invisible())
      }
      CurrentDate <- GCAMCPUB::to_date(CurrentDate)
      db_mo <- GCAMCPUB::activate_conn(db_mo)
      ##holding
      sql <- "select Date, Port_Name, AV_Book_LC, AV_Mix_LC from 
      CORE_Data_Holding where Date = '%s' and 
      Port_Type in ('Cost Based', 'Cost Based FC')"
      sql <- sprintf(sql, DateFrom)
      self$data$holding <-
        DBI::dbGetQuery(db_mo, sql) %>%
        dplyr::mutate(Date = GCAMCPUB::to_date(Date)) %>%
        data.table::setDT() 
    },
    save = function (path) {
      if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
      saveRDS(list(holding = self$data$holding), path)
      invisible()
    },
    load = function (path) {
      if (!file.exists(path)) return(list(error = "load file doesn't exists"))
      r <- readRDS(path)
      self$data$holding <- r$holding
      #### return ####
      invisible()
    },
    table_update = function(){
      AV_start <- self$data$holding[, list(AV_start = sum(AV_Book_LC)),
                                    by = Port_Name]
      AV_start[pirvate$nameseq, on = 'Port_Name']
    }
  )#,
  #private = list(
  #  nameseq <- data.table(c('CNPC', 'Par', 'Life', 'Capital RMB',
  #                             'UV Individual', 'UV Group')) %>%
  #    setnames('Port_Name')
  #)
)
report_data <- mon_performance_report$new(
  path = NULL, DateFrom = as.Date('2015-12-31'), 
  DateTo = as.Date('2016-12-31'),
  CurrentDate = as.Date('2016-02-29'))