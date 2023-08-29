# this script is used to read the excel file and discard empty row column and convert it into usable 
# struture data frame.
# the input simply is path of excel file, similar to read_excel function in readxl package
# written and maintained by Tung Trinh
# Aug 20th 2023
# version 1.0
# for more information, please contact to tungts@oucru.org
####################################################################################################
smooth_excel <- function(input_path,sheet = NULL){
  ##### required path and built-in functions #####
  require(readxl)
  require(dplyr)
  require(stringr)
  require(stringi)
  ##### read the excel #####
  excel <- read_excel(input_path, sheet = sheet)
  ##### processing the file #####
  ### first check the column name
  # get the column name 
  col <- colnames(excel)
  # print out the column name 
  print(paste("The exist variable name:",col))
  # check wether the column name is right or wrong 
  total_undectected_variable <- grepl("[...]",col) %>% sum()
  if(total_undectected_variable >= (ncol(excel)-2)){
    need_smooth <- T
  }else{
    need_smooth <- F
  }
  ### clean the excel 
  if(need_smooth == T){
    # calculate the number of NA value per row 
    excel$no_na <- rowSums(is.na(excel))
    # find the first row have most detailed information 
    ind_row <- which(excel$no_na == min(excel$no_na))[1]
    target_variable_name <- excel[ind_row,] %>% unlist()
    # replace space with underscore 
    target_variable_name <- target_variable_name %>% stri_trans_general(., id = "Latin-ASCII") %>% 
      str_replace_all(.," ","_") %>% tolower()
    # print out the target variable name 
    print(paste("New variable names:",paste(target_variable_name, collapse = ", ")))
    # create another data to write out and discard no_na variable 
    out <- excel %>% select(-no_na)
    # set the column name 
    colnames(out) <- target_variable_name
    # discard the row until after column name
    out <- out[-c(1:ind_row),]
  }else{
    print("No need to clean the excel file")
  }
  ### discard empty columns 
  # check the total number of NA in column 
  ind_col <- which(colSums(is.na(out)) == nrow(out),arr.ind = T) %>% as.vector()
  # discard
  out <- out[,-ind_col]
  ##### output the excel #####
  return(out)
  
}