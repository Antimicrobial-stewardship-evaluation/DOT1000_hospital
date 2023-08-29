# this function is used to clean data of 30HN hospital project 
# can use to clean three differenet data: patient, drug and bed
# March 15th 2023
# written and maintained by Tung Trinh
# version 1.0 
# for more information, please contact to tungts@oucru.org
###################################################################
clean_data <- function(input = "input data", type = "type of data"){
  ##### required packages and built-in functions #####
  require(dplyr)
  require(tictoc)
  source("Scripts/colname_match.R")
  "%ni%" <- Negate("%in%")
  ##### condition to check the correct input #####
  if(type %in% c("patient","drug","bed")){
    print(paste("You have selected:",type,"option"))
  }else{
    stop("Please choose the correct type: patient, drug or bed")
  }
  ##### start cleaning the data #####
  ### create the destinated colnames name for each data set 
  # patient data 
  patient_colname <- c("rowid", "stt", "crfid", "patid","hin", "ngay_sinh", "gioi_tinh","dia_chi","ma_dkbd","gt_the_tu","gt_the_den","mien_cung_ct",
                       "ten_benh","ten_benhkhac","ma_benh","ma_benhkhac","ma_lydo_vvien","ma_noi_chuyen","ma_tai_nan",
                       "ngay_vao","ngay_ra", "so_ngay_dtri","ket_qua_dtri","tinh_trang_rv", "t_tongchi", "t_xn","t_cdha", "t_thuoc","t_vtyt", "t_dvkt_tyle", "t_thuoc_tyle", "t_vtyt_tyle",
                       "t_kham", "t_giuong", "t_vchuyen", "t_bntt","t_bncct", "t_bhtt", "t_ngoaids","t_nguonkhac", "nam_qt", "thang_qt",
                       "ngay_tt","ma_khuvuc","ma_loai_kcb","ma_cskcb","ma_khoa","ten_khoa","ma_pttt_qt","can_nang","state_xuat","state_gui")
  # drug data 
  drug_colname <- c("stt","crfid","hin","patid", "dia_chi","ngay_sinh","benh", "ma_benh","ma_thuoc","ma_nhom","ten_thuoc","ten_hoat_chat",
                    "ham_luong","duong_dung","lieu_dung","so_dang_ky","tt_thau","pham_vi","tyle_tt",
                    "so_luong","don_gia", "don_vi_tinh","thanh_tien","muc_huong","t_nguonkhac","t_bntt","t_bhtt","t_bncct","t_ngoaids", "ngay_yl",
                    "ten_khoa","ma_khoa","ten_benhphong","ma_bac_si", "nhap_vien")
  # bed data 
  bed_colname <- c("stt", "crfid","patid","ten_khoa","ma_khoa","ten_phong","ngay_vao","ngay_ra")
  ### choose the target dataset  
  # patient
  if(type  == "patient"){
    # assign the colnames 
    target_col <- patient_colname
  }
  # bed
  if(type == "bed"){
    # assign the colnames
    target_col <- bed_colname
  }
  # drug 
  if(type == "drug"){
    # assign the colnames
    target_col <- drug_colname
  }
  # stop running if dataset not in drug,bed and patient
  if(type %ni% c("patient","drug","bed")){
    stop("please choose between patient, drug or bed")
  }
  ### cleaning the data 
  # get info of the data 
  print(dim(input))
  ### cleaning the data 
  # start counting the time 
  tic("start cleaning the data")
  # get the column names of the data 
  col_dat <- colnames(input)
  # get the designated column 
  fixed_col <- colname_match(input = col_dat, dataset = type)
  # rename the column 
  colnames(input) <- fixed_col
  # find the missing columns of the dataset 
  miss_col <- setdiff(target_col,fixed_col)
  # condition to add missing columns
  if(length(miss_col) > 0){
    # create a new dataset
    new_dat <- matrix(nrow = nrow(input),ncol = length(miss_col)) %>% as.data.frame()
    # assign column name 
    colnames(new_dat) <- miss_col
    # combine with current dataset
    output <- cbind(input,new_dat)
  }
  # logical output
  if(sum(duplicated(colnames(output))) > 0){
    # deduplicate the column
    output <- output[,-duplicated(colnames(output))]
    print(-duplicated(colnames(output)))
  }
  # select variable to output
  output <- output %>% select(contains(target_col)) %>% as.data.frame()
  # fix the date of ngay_ra and ngay vao of bed and admin dataset 
  if(type %in% c("patient","bed")){
    output <- output %>% mutate(ngay_vao = as.Date(ngay_vao, format = "%Y-%m-%d"),
                        ngay_ra = as.Date(ngay_ra, format = "%Y-%m-%d"))
  }
  # use ma_khoa as ten_khoa in case of missing ten_khoa for bed
  if(type %in% c("bed","drug")){
    output <- output %>% mutate(ten_khoa = if_else(is.na(ten_khoa),ma_khoa,ten_khoa))
  }
  # stop the time counting
  toc()
  ##### return the output #####
  return(output)
}