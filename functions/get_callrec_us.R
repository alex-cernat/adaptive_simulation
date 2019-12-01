# Function to get call records in UKHLS


get_callrec_us <- function(folder) {
  
  file <- list.files(folder, pattern = "callrec")
  
  cont_data <- read_dta(str_c(folder, file))
  
  prefix <- str_which(letters, str_remove(file, "_callrec.+")) 
  
  if (sum(str_detect(names(cont_data), "mm")) > 0) {
  
    count_data <- cont_data %>%
      rename_all(funs(str_remove_all(., "._|call"))) %>%  
      mutate(contact_time = make_datetime(hour = strtdathh, 
                                          min = strtdatmm),
             contact_day = make_datetime(day = strtdatd,
                                         month = strtdatm,
                                         year = strtdaty),
             status = relabel(status)) %>% 
      rename(callno = no) %>% 
      dplyr::select(hidp, issueno, callno, length, status,
             contact_time, contact_day) %>% 
      rename_all(funs(str_c(., "_",prefix)))
      
  } else {
  
  count_data <- cont_data %>%
    rename_all(funs(str_remove_all(., "._"))) %>%  
    mutate(contact_time = make_datetime(hour = startmin %/% 100, 
                                        min = startmin %% 100),
           contact_day = make_datetime(day = startday,
                                       month = startmth,
                                       year = startyr),
           status = relabel(status)) %>% 
    dplyr::select(hidp, issueno, callno, length, status,
           contact_time, contact_day) %>% 
    rename_all(funs(str_c(., "_",prefix)))
  
  }
  
  
  colnames(count_data)[1] <- str_c(str_remove(file, "callrec.+"), 
                                   "hidp")
  
  count_data
}

