
### syntax to get contact history we want from wave
### we just run once and then saved, bellow we import the data



folders <- list.files("./data/stata/", pattern = "_w[0-6]")

call_rec_data <- NULL
call_rec_data <- map(
  str_c("./data/stata/", folders, "/"),
    get_callrec_us)

# get month sample info

index <- 2
call_rec_data2 <- map(call_rec_data[2:6], function(x) {
month_data <- usw %>%
  select(str_c(letters[index], "_hidp"),
         str_c("month_", index))

data <- month_data %>%
    left_join(x)

index <<- index + 1

data

}
)

#
#
#
# # get call records indicators

rm(call_rec_data)
gc()

#x <- call_rec_data2[[2]]


index <- 5
x <- call_rec_data2[[5]]

map(call_rec_data2[3:5], function(x) {

 step1 <- usw %>%
  dplyr::select(str_c(letters[index], "_hidp"),
         str_c(c("int_phase_", "out_", "phase_j1end_"),
               index)) %>%
   group_by(f_hidp) %>%
   filter(row_number() == 1)


 step2 <- semi_join(x, step1) %>%
   select(matches("hidp|month|status|contact"))
 step3 <- left_join(step2, step1)

 data <- step3 %>%
   rename_all(funs(str_remove_all(., "_[0-9]|^[a-z]_"))) %>%
  group_by(month) %>%
  mutate(ph1 = phase_j1end,
         ph1 = ifelse(is.na(ph1),
                      mean(ph1, na.rm = T), ph1),
         ph1 = as_date(ph1)) %>%
  filter(contact_day < mean(phase_j1end, na.rm = T)) %>%
  dplyr::select(-phase_j1end) %>%
  filter(!is.na(status)) %>%
  filter(contact_day > 0) %>%
    group_by(month, hidp) %>%
  mutate(
    status_p1 = ifelse(
    max(contact_day, na.rm = T) == contact_day,
    status, NA),
    status_p1 = max(status_p1, na.rm = T),
    status_p1 = ifelse(status_p1 == -Inf, NA, status_p1),
    days_int = min(contact_day, na.rm = T) %--%
      max(contact_day, na.rm = T),
    days_int = dseconds(days_int) / ddays(1)) %>%
   ungroup() %>%
    dplyr::select(matches("hidp"), ph1,
                status_p1, days_int) %>%
   group_by(hidp) %>%
   filter(row_number() == 1) %>%
   ungroup() %>%
   rename_all(funs(str_c(., "_", index) %>%
                    str_replace(., "hidp_[0-9]",
                                str_c(letters[index],
                                      "_hidp"))))

 save(data, file = str_c("./data/call_rec_", index, ".RData"))

 rm(data)
 gc()

 index <<- index + 1
}
)