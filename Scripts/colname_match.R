# this script is used to create a name matching function for the columns of the data set
# the input is the string of variable names of the dataset 
# written and maintained by Tung Trinh
# Dec 20th 2022
# version 2.0
# Updated in Jan 3rd 2023
# for more information, please contact to tungts@oucru.org
#########################################################################################
colname_match <- function(input = "vector of column name",dataset = "kind of dataset"){
  ##### required packages and built-in function #####
  require(stringi)
  require(stringr)
  require(dplyr)
  require(data.table)
  # require(readxl)
  "%ni%" <- Negate("%in%")
  ##### read the reference to get the target column name #####
  # patient
  patient <- readRDS("Data/patient_target_variable_name_2023-03-27.rds")
  # drug
  drug <- readRDS("Data/drug_target_variable_name_2023-03-27.rds")
  # bed 
  bed <- readRDS("Data/bed_target_variable_name_2023-03-27.rds")
  ##### create the destinated colnames name for each data set #####
  # patient data 
  patient_colname <- c("rowid", "stt", "new_crfid", "new_patid","ma_the","new_hin", "ngay_sinh", "gioi_tinh","dia_chi","ma_dkbd","gt_the_tu","gt_the_den","mien_cung_ct",
                       "ten_benh","ten_benhkhac","ma_benh","ma_benhkhac","ma_lydo_vvien","ma_noi_chuyen","ma_tai_nan",
                       "ngay_vao","ngay_ra", "so_ngay_dtri","ket_qua_dtri","tinh_trang_rv", "t_tongchi", "t_xn","t_cdha", "t_thuoc","t_vtyt", "t_dvkt_tyle", "t_thuoc_tyle", "t_vtyt_tyle",
                       "t_kham", "t_giuong", "t_vchuyen", "t_bntt","t_bncct", "t_bhtt", "t_ngoaids","t_nguonkhac", "nam_qt", "thang_qt",
                       "ngay_tt","ma_khuvuc","ma_loai_kcb","ma_cskcb","ma_khoa","ten_khoa","ma_pttt_qt","can_nang","state_xuat","state_gui")
  # drug data 
  drug_colnames <- c("stt","new_crfid","new_hin","new_patid", "dia_chi","ngay_sinh","benh", "ma_benh","ma_thuoc","ma_nhom","ten_thuoc","ten_hoat_chat",
                     "ham_luong","duong_dung","lieu_dung","so_dang_ky","tt_thau","pham_vi","tyle_tt",
                     "so_luong","don_gia", "don_vi_tinh","thanh_tien","muc_huong","t_nguonkhac","t_bntt","t_bhtt","t_bncct","t_ngoaids", "ngay_yl",
                     "ten_khoa","ma_khoa","ten_benhphong","ma_bac_si", "nhap_vien")
  # bed data 
  bed_colnames <- c("stt", "new_crfid","new_patid","ten_khoa","ten_phong","ngay_vao","ngay_ra")
  ##### process the column names #####
  ### choose the target dataset  
  # patient
  if(dataset == "patient"){
    # assign the colnames 
    target_col <- patient_colname
  }
  # bed
  if(dataset == "bed"){
    # assign the colnames
    target_col <- bed_colnames
  }
  # drug 
  if(dataset == "drug"){
    # assign the colnames
    target_col <- drug_colnames
  }
  # stop running if dataset not in drug,bed and patient
  if(dataset %ni% c("patient","drug","bed")){
    stop("please choose between patient, drug or bed")
  }
  ### choose the target reference
  # patient
  if(dataset == "patient"){
    # assign the colnames 
    target_ref <- patient
  }
  # bed
  if(dataset == "bed"){
    # assign the colnames
    target_ref <- bed
  }
  # drug 
  if(dataset == "drug"){
    # assign the colnames
    target_ref <- drug
  }
  ### search the column names that match with the target 
  # turn into data frame 
  to_search_dt <- data.frame(colnames = input,
                             converted = stri_trans_general(input, id = "Latin-ASCII") %>% 
                               tolower() %>% str_replace(.," ","_"))
  # match with reference 
  to_fix <- to_search_dt %>% left_join(.,target_ref, by = c("converted" = "fixed"))
  # get the number of NA within the fixed column names 
  no.na <- is.na(to_fix$target_variable_name) %>% sum()
  # condition to fix the column name if no.na > 0
  if(no.na > 0){
    # fix the column name
    to_fix$target_variable_name[which(is.na(to_fix$target_variable_name))] <- paste0("c",1:no.na)
  }
  ### return the fixed column name 
  return(to_fix$target_variable_name)
}
