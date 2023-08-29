# this script is used to calculate the bed day for each individual in each day for 30HN hospital data
## written and maintained by Tung Trinh
# Feb 22nd 2023
# version 1.0 
# For more information, please contact to tungts@oucru.org 
###################################################################################################
monthly_bedday <- function(bed = "input bed data"){
  ##### required packages and built-in functions ######
  require(data.table)
  require(lubridate)
  require(dplyr)
  ##### calculate the bed day #####
  # fix the date of ngay_ra and ngay vao 
  bed <- bed %>% mutate(ngay_vao = as.Date(ngay_vao, format = "%Y-%m-%d"),
                        ngay_ra = as.Date(ngay_ra, format = "%Y-%m-%d"))
  # filter the record which doesn't have either ngay_vao or ngay ra 
  bed <- bed %>% filter(!is.na(ngay_vao) & !is.na(ngay_ra))
  # coerce dates from character to IDate
  cols <- c("ngay_vao", "ngay_ra")
  DT <- as.data.table(bed)[, (cols) := lapply(.SD, as.IDate), .SDcols = cols]
  
  # create sequence of months which cover all periods
  mon_seq <- DT[, as.IDate(seq(floor_date(min(ngay_vao), unit = "months"), 
                               ceiling_date(max(ngay_ra), unit = "months"),
                               by = "month"))]
  # create helper data.table with first and last day of months
  mDT <- data.table(ngay_vao = head(mon_seq, -1L), ngay_ra = tail(mon_seq, -1L) - 1L)
  setkeyv(DT, cols)
  # find overlapping pieces for each month
  bed_day <- foverlaps(mDT, DT, nomatch = 0L)[
    # compute count of days in each month
    , {tmp <- pmax(ngay_vao, i.ngay_vao)
    .(crfid = crfid,ten_khoa = ten_khoa, month_yr = format(tmp, "%Y-%m"), 
      bed_day = as.integer(difftime(pmin(ngay_ra, i.ngay_ra), tmp, units = "days")) + 1L)
    }]
  ##### output the result #####
  return(bed_day)
}