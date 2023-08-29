# this script is used to read multiple file with different kind of extension 
# written and maintained by Tung Trinh
# Jan 3rd 2023
# modified by Tung Trinh
# Aug 20th 2023
# version 2.0
# for more information, please contact to tungts@oucru.org
##########################################################################################################
multiple_read <- function(input){
  ### required packages  and built-in function###
  require(readxl)
  require(data.table)
  require(tools)
  source("Scripts/smooth_excel.R")
  ### load data with different extension ###
  # create an empty list 
  list_dat <- list()
  # create an indicator 
  i = 1 
  # create a for loop
  for(file in input){
    # get the file extension
    ext <- file_ext(file)
    # print out the extension
    print(ext)
    # create an if else condition  
    # rds 
    if(ext == "Rds"){
      # read the file 
      list_dat[[i]] <- readRDS(file) %>% dplyr::mutate_all(as.character)
    }
    # csv 
    if(ext == "csv"){
      # read the file 
      list_dat[[i]] <- read.csv(file = file, header = T, sep = ",") %>% dplyr::mutate_all(as.character)
    }
    # excel 
    if(ext %in% c("xlsx","xls","xlsm")){
      # read the file  
      #tmp <- read_excel(path = file)
      # search for the column name row 
      #ind_start <- tmp %>% pull(1) %>% grep("stt","STT",.)
      #colnames(tmp) <- tmp[ind_start,]
      #list_dat[[i]] <- tmp %>% slice(-(1:ind_start))
      list_dat[[i]] <- smooth_excel(path = file) %>% dplyr::mutate_all(as.character)
    }
    # accumulate the indicator 
    i = i + 1
  }
  # merge the data set 
  dat <- rbindlist(list_dat, fill = TRUE, use.names = TRUE)
  # return data
  return(dat)
}
