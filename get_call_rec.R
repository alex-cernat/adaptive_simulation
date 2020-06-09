
### syntax to get contact history we want from wave
### we just run once and then saved, bellow we import the data

# the variables we want:


# number of contacts after the end of period 1 - nrcalls + nrcalls2
# the day of the week of the last call - last_weekday
# the elapsed time between the first and last call - days_int + days_int2 
# the outcome of the last call - status_p1
# the time of the last call - last_daytime
# the proportion of non-contact calls in the initial period - noreply_ph1
# number of calls after phase 1 - nrcalls_ph23


# an interview was done with another person in the household - anyinter_ph1
# -> need to make a different version based on outcome variable

# the type of the last call (whether face-to-face or telephone) - don't have?




folders <- list.files("./data/stata/", pattern = "_w[0-6]",
                      full.names = T)

load("./data/usw.RData")


# get timing info
call_rec_data <- NULL
call_rec_data <- map(str_c(folders, "/"), get_callrec_us)

index <- 2
map(call_rec_data[2:6], function(x) {

  # get info from individual data file
  step1 <- usw %>%
    dplyr::select(str_c(letters[index], "_hidp"),
                  str_c(c(
                    "int_phase", "out", "phase_j1end", "month"
                  ), "_", index)) %>%
    rename(hidp = str_c(letters[index], "_hidp")) %>% # delete prefix
    group_by(hidp) %>%
    filter(row_number() == 1) %>%
    ungroup() 
 
 # re-apply wave prefix
 names(step1)[1] <- str_c(letters[index], "_hidp")

 # put toghether contact info and period
 step2 <- right_join(x, step1)

 
 # make call record data 
 
  phase1 <- step2 %>%
   rename_all(funs(str_remove_all(., "_[0-9]|^[a-z]_"))) %>%
   group_by(month) %>%
   # here we give the average
   mutate(ph1 = phase_j1end,
          ph1 = ifelse(is.na(ph1),
                       mean(ph1, na.rm = T), ph1),
          ph1 = as_date(ph1)) %>%
   # keep only contacts before the deadline
   filter(contact_day < mean(phase_j1end, na.rm = T)) %>%
   dplyr::select(-phase_j1end) %>% # eliminate variable
   filter(!is.na(status)) %>% # delete if they have missing status
   filter(contact_day > 0) %>%
   group_by(hidp) %>%
   mutate(
     status_p1 = ifelse(max(contact_day, na.rm = T) == contact_day,
                        status, NA),
     status_p1 = max(status_p1, na.rm = T),
     status_p1 = ifelse(status_p1 == -Inf, NA, status_p1),
     days_int = min(contact_day, na.rm = T) %--%
       max(contact_day, na.rm = T),
     days_int = dseconds(days_int) / ddays(1),
     days_int2 = days_int^2,
     nrcalls = max(row_number()),
     nrcalls2 = nrcalls^2,
     last_daytime = ifelse(max(contact_day, na.rm = T) == contact_day,
                                   contact_daytime, NA),
     last_daytime = max(last_daytime, na.rm = T),
     last_weekday = ifelse(max(contact_day, na.rm = T) == contact_day,
                          week_day, NA),
     last_weekday = max(last_weekday, na.rm = T),
     noreply = ifelse(status == "no reply", 1, 0),
     noreply_ph1 = sum(noreply)/max(row_number()),
     noreply2_ph1 = noreply_ph1^2,
     interview = ifelse(status == "any interviewing done", 1, 0),
     anyinter_ph1 = sum(interview),
     anyinter_ph1 = ifelse(anyinter_ph1 > 0, 1, 0)) %>%
   ungroup() %>% 
    mutate(last_weekday = factor(last_weekday,
                                 labels = c("Mon", "Tue", "Wed", "Thu", 
                                            "Fri", "Sat", "Sun")),
           anyinter_ph1 = factor(anyinter_ph1, 
                                 labels = c("No", "Yes")))
  
  # deal with unequal categories
  
  status_lab <- c(
    "no reply",
    "contact made",
    "appointment made",
    "any interviewing done",
    "any other status"
  )
  
  nr_cat <- table(phase1$status_p1) %>% length()
  
  if (nr_cat == 5) {
    phase1 <- phase1 %>% 
  mutate(status_p1 = factor(status_p1,
                              labels = status_lab))
  } else {
    status_lab <- c("missing", status_lab)
    phase1 <- phase1 %>% 
      mutate(status_p1 = factor(status_p1,
                                labels = status_lab))
  }
  

  # select variables and label
  phase1 <- phase1 %>% 
    select(matches("hidp"), ph1,
           status_p1, days_int, days_int2, nrcalls, nrcalls2,
           last_daytime, last_weekday, noreply_ph1, noreply2_ph1,
           anyinter_ph1) %>%
    group_by(hidp) %>%
    filter(row_number() == 1) %>%
    ungroup() 
  
  
  # get number of calls after phase1
  
  ph23 <- step2 %>%
    rename_all(funs(str_remove_all(., "_[0-9]|^[a-z]_"))) %>%
    group_by(month) %>%
    # here we give the average
    mutate(ph1 = phase_j1end,
           ph1 = ifelse(is.na(ph1),
                        mean(ph1, na.rm = T), ph1),
           ph1 = as_date(ph1)) %>%
    # keep only contacts before the deadline
    filter(contact_day > mean(phase_j1end, na.rm = T)) %>%
    group_by(hidp) %>% 
    mutate(nrcalls_ph23 = max(row_number())) %>% 
    group_by(hidp) %>%
    filter(row_number() == 1) %>%
    ungroup() %>% 
    select(hidp, nrcalls_ph23) 
    
  
  # merge and code as 0 if not in phase2-3 data
  data2 <- full_join(phase1, ph23) %>% 
    mutate(nrcalls_ph23 = ifelse(is.na(nrcalls_ph23), 
                                   0, nrcalls_ph23)) 
  
  
  # make phase presence indicator
  
  hh_both <- semi_join(phase1, ph23, by = "hidp")$hidp
  hh_ph1 <- anti_join(phase1, ph23, by = "hidp")$hidp
  hh_ph2 <- anti_join(ph23, phase1, by = "hidp")$hidp
  
  data2 <- data2 %>% 
    mutate(phase_miss = case_when(hidp %in% hh_ph1 ~ "Phase 1",
                                  hidp %in% hh_ph2 ~ "Phase 2",
                                  hidp %in% hh_both ~ "Both phases")) 
  
  

  
  # rename variables
  data3 <- data2 %>%
    rename_all(funs(str_c(., "_", index) %>%
                      str_replace(., "hidp_[0-9]",
                                  str_c(letters[index],
                                        "_hidp"))))
  
 write_rds(data3, str_c("./data/call_rec_", index, ".RDS"))

 rm(data, data2, data3)
 gc()

 index <<- index + 1
}
)
