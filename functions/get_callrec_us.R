# Function to get call records in UKHLS


get_callrec_us <- function(folder) {
  
  # name of file
  file <- list.files(folder, pattern = "callrec")
  
  # get the call record data
  cont_data <- read_dta(str_c(folder, file))
  
  # get the prefix
  prefix <- str_which(letters, str_remove(file, "_callrec.+")) 
  
  
  # check the coding  fot time
  if (sum(str_detect(names(cont_data), "mm")) > 0) {

    # clean data and select variables of interest  
    count_data <- cont_data %>%
      rename_all(funs(str_remove_all(., "._|call"))) %>%  
      mutate_at(vars(matches("strt")),
                ~ ifelse(. %in% c(-8, -9), NA, .)) %>% 
      mutate(contact_time = make_datetime(hour = strtdathh, 
                                          min = strtdatmm),
             contact_day = make_datetime(day = strtdatd,
                                         month = strtdatm,
                                         year = strtdaty),
             miss_call_day = ifelse(is.na(contact_day), 1, 0),
             status = relabel(status),
             week_day = wday(contact_day, label = T, week_start = 1),
             contact_hour = hour(contact_time),
             contact_daytime = case_when(
               contact_hour < 12 ~ "morning",
               contact_hour >= 12 & contact_hour <= 17 ~ "afternoon",
               contact_hour > 17  ~ "evening")
             ) %>% 
      rename(callno = no) %>% 
      dplyr::select(hidp, issueno, callno, length, status,
             contact_time, contact_day, week_day, contact_daytime) %>% 
      rename_all(funs(str_c(., "_", prefix)))
      
  } else {
  
  count_data <- cont_data %>%
    rename_all(funs(str_remove_all(., "._"))) %>%  
    mutate_at(vars(matches("start")),
              ~ ifelse(. %in% c(-8, -9), NA, .)) %>% 
    mutate(contact_time = make_datetime(hour = startmin %/% 60, 
                                        min = startmin %% 60),
           contact_day = make_datetime(day = startday,
                                       month = startmth,
                                       year = startyr),
           miss_call_day = ifelse(is.na(contact_day), 1, 0),
           status = relabel(status),
           week_day = wday(contact_day, label = T, week_start = 1),
           contact_hour = hour(contact_time),
           contact_daytime = case_when(
             contact_hour < 12 ~ "morning",
             contact_hour >= 12 & contact_hour <= 17 ~ "afternoon",
             contact_hour > 17  ~ "evening"
           )
    ) %>% 
    dplyr::select(hidp, issueno, callno, length, status, miss_call_day,
           contact_time, contact_day, week_day, contact_daytime) %>% 
    rename_all(funs(str_c(., "_", prefix)))
  
  }
  
  # give time dependent household number
  colnames(count_data)[1] <- str_c(letters[prefix], "_hidp")
  
  count_data
}
