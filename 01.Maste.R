###
#
# Adaptive survye design in longitudinal studies
#       with Nicole Watson and Mark O'Shea
#
###

####
# 
# 1. Import data
# 
# 2. Select cases of interest
# 
# 3. Make interview outcome variables
# 
# 4. Make variables of interest
# 
# 5. Make fielwwork graph
# 
# 6. Make fieldwork variables
# 
###



# 00 Admin ----------------------------------------------------------------

# clean memory
rm(list = ls())
gc()

# use local packages on work machine
if (Sys.getenv("USERNAME") == "msassac6") {.libPaths(c(
  paste0(
    "C:/Users/",
    Sys.getenv("USERNAME"),
    "/Dropbox (The University of Manchester)/R/package"
  ),
  .libPaths()
))}


# load packages
pkg <- c("tidyverse", "haven", "lubridate")

sapply(pkg, library, character.only = T)


# load functions
functions <- list.files("./functions/", pattern = ".R")
map(str_c("./functions/", functions), source)


# 01 Make phase variable ----------------------------------------------------


# get wave 1 data and cross wave information
us1r_full <- read_dta("./data/stata/us_w1/a_indresp.dta")
us1h_full <- read_dta("./data/stata/us_w1/a_hhresp.dta")
usxw_full <- read_dta("./data/stata/us_wx/xwavedat.dta")
usxwid_full <- read_dta("./data/stata/us_wx/xwaveid.dta")

phase_data <- list(NULL)

vars <- c("intdatd_dv", "intdatm_dv", "intdaty_dv",
          "month", "ivfio", "hidp")


phase1 <- c(0, 6, 6, 6, 12, 10)
phase2 <- c(0, 6, 6, 6, 4, 6)
phase3 <- c(0, 4, 4, 9, 3, 4)

for (i in 2:6) {
  
  # get date data from all waves
  temp_data <- read_dta(str_c("./data/stata/us_w", i, "/",
                              letters[i], "_indall.dta"))

  temp_data <- temp_data %>%
    dplyr::select(pidp, str_c(letters[i], "_", vars)) %>% 
    rename_all(funs(str_remove(., str_c(letters[i], '_')) %>%
                      str_remove(., '_dv'))) %>% 
    mutate(intdate = dmy("1-1-1988"),
           intdate = update(intdate,
                            day = intdatd, 
                            month = intdatm, 
                            year = intdaty),
           month = relabel(month)) %>% 
    dplyr::select(-intdatd, -intdatm, -intdaty)
  
  # make phase variables
  
  temp_data <- temp_data %>%
    filter(ivfio == 1) %>% 
    group_by(month) %>% 
    arrange(intdate, hidp) %>%
    mutate(case = 1,
           cum_case = cumsum(case)/sum(case),
           phase_j1end = min(intdate, na.rm = T) + weeks(phase1[i]),
           phase_j2end = phase_j1end + weeks(phase2[i]),
           phase_j3end = phase_j2end + weeks(phase3[i]),
           last_int = max(intdate)) %>%
    ungroup() %>% 
    arrange(month, intdate, cum_case) %>% 
    dplyr::select(-ivfio, -hidp, -case) %>% 
    rename_all(funs(str_replace(., ".+", str_c(., "_", i)) %>% 
                      str_replace_all(., 'pidp_.', 'pidp')))
  
  phase_data[[i]] <- temp_data
    
}

phase_data <- reduce(phase_data[2:6], full_join)

# take into account that year 2 of wave 4 had a different data collection
# period

phase_data[as.numeric(phase_data$month_4) %in% 13:24,
           c("phase_j1end_4", "phase_j2end_4", "phase_j3end_4")] <-
phase_data %>% 
  filter(as.numeric(month_4) %in% 13:24) %>% 
  group_by(month_4) %>% 
  mutate(phase_j1end_4 = min(intdate_4, na.rm = T) + weeks(phase1[5]),
         phase_j2end_4 = phase_j1end_4 + weeks(phase2[5]),
         phase_j3end_4 = phase_j2end_4 + weeks(phase3[5])) %>% 
  ungroup() %>% 
  dplyr::select(phase_j1end_4, phase_j2end_4, phase_j3end_4)

# make individual phase variable

phase_data <- phase_data %>% 
  mutate(int_phase_2 = case_when(intdate_2 < phase_j1end_2 ~ 1,
                                 intdate_2 >= phase_j1end_2 & 
                                   intdate_2 < phase_j2end_2 ~ 2,
                                 intdate_2 >= phase_j2end_2 ~ 3),
         int_phase_3 = case_when(intdate_3 < phase_j1end_3 ~ 1,
                                 intdate_3 >= phase_j1end_3 & 
                                   intdate_3 < phase_j2end_3 ~ 2,
                                 intdate_3 >= phase_j2end_3 ~ 3),
         int_phase_4 = case_when(intdate_4 < phase_j1end_4 ~ 1,
                                 intdate_4 >= phase_j1end_4 & 
                                   intdate_4 < phase_j2end_4 ~ 2,
                                 intdate_4 >= phase_j2end_4 ~ 3),
         int_phase_5 = case_when(intdate_5 < phase_j1end_5 ~ 1,
                                 intdate_5 >= phase_j1end_5 & 
                                   intdate_5 < phase_j2end_5 ~ 2,
                                 intdate_5 >= phase_j2end_5 ~ 3),
         int_phase_6 = case_when(intdate_6 < phase_j1end_6 ~ 1,
                                 intdate_6 >= phase_j1end_6 & 
                                   intdate_6 < phase_j2end_6 ~ 2,
                                 intdate_6 >= phase_j2end_6 ~ 3))





# 02. Select cases of interest --------------------------------------------

# select baseline respondents from the general sample of GB

us1_select <- us1r_full %>% 
  filter(a_hhorig == 1) %>% # GB general sample
  filter(a_ivfio == 1) %>% # full interview
  dplyr::select(pidp)




# 03. Make interview outcome variables ------------------------------------

out_list <- list(NULL)

for (i in 2:7) {
data <- read_dta(str_c("./data/stata/us_w", i, "/", letters[i], 
                       "_indsamp.dta"))

vars <- c("ivfio", "issued2w")

data <- data %>% 
  rename_all(funs(str_remove(., str_c(letters[i], "_")))) %>%
  filter(issued2w == 1) %>% # issues to the wave
  filter(memorig %in% 1:2) %>% # check this (here we choose NI as well)
  mutate(out = ifelse(ivfio == 1, 1, 0)) %>% 
  dplyr::select(pidp, out, ivfio) %>% 
  right_join(us1_select, by = "pidp") 

out_list[[i]] <- data
names(out_list)[i] <- str_c("us", i)
}



# exclude dead people

for (i in 2:7){
  dead_data <- usxw_full %>%
    filter(usxw_full$dcsedw_dv %in% 1:i) %>% 
    dplyr::select(pidp)
  
  out_list[[i]] <- out_list[[i]] %>%
    anti_join(dead_data, by = "pidp") 
}

# larger proporitons of participation because the mean does not
# include all NA's, I change that later in the code
sapply(out_list, summary)


# 04. Make variables of interest ------------------------------------------



usxw <- usxw_full %>% 
  dplyr::select(pidp, ukborn, plbornc,)


varsi <- c("hidp", "dvage", "jbhrs", "jbstat",
           "gor_dv", "sex", "mastat_dv",
           "hhtype_dv", "nchild_dv", "hiqual_dv",
           "urban_dv", "hhresp_dv", "sf1", "health",
           "xpmove", "ivfio", "fibenothr_dv",
           "sf3a", "fenow")

varsh <- c("hidp", "hhsize", "hsownd", "numadult",
           "fihhmnsben_dv", "fihhmnnet1_dv")

us1h <- us1h_full %>% 
  dplyr::select(str_c("a_", varsh))

us1 <- us1r_full %>% 
  dplyr::select(pidp, str_c("a_", varsi)) %>% 
  left_join(us1h, by = "a_hidp") %>% 
  rename_all(funs(str_remove_all(., 'a_') %>%
                    str_remove_all(., '_dv|ff_'))) %>% 
  left_join(usxw, by = "pidp")


######################## !!!
# remove proxy?

us1 <- us1 %>% filter(ivfio == 1)


# recode variables

us1 <- us1 %>% 
  mutate(agecat = cut(dvage, c(15, 19, 24, 34,
                               44, 54, 64, 74,
                               105)),
         employed = case_when(jbstat == 3 ~ 1,
                              jbstat %in% c(1, 2) ~ 2,
                              jbstat > 3 ~ 0),
         employed_fct = factor(employed,
                               labels = c("Not in labour force",
                                          "Unemployed",
                                          "Employed")),
         jbhrs = ifelse(employed %in% c(0, 1), 0, jbhrs),
         jobhrs = cut(jbhrs, c(-1, 0, 14, 29, 39, 49, 100)),
         owner = as.factor(case_when(hsownd %in% 1:3 ~ "Yes",
                           hsownd > 3 ~ "No")),
         relationship = case_when(mastat %in% 2:3 ~ 1,
                                  mastat == 10 ~ 2,
                                  mastat %in% 4:9 ~ 3,
                                  mastat == 1 ~ 4),
         relationship_fct = factor(relationship,
                                   labels = c("Married",
                                              "De facto",
                                              "Separated",
                                              "Single")),
         likelymove = as.factor(case_when(xpmove == 1 ~ "Yes",
                                xpmove == 2 ~ "No")),
         countryofbirth = case_when(ukborn %in% 1:4 ~ 1,
                                    ukborn == 5 ~ 3),
         countryofbirth = ifelse(plbornc %in% c(5, 13:16, 26),
                                 2, countryofbirth),
         countryofbirth_fct = factor(countryofbirth,
                                      labels = 
                                        c("UK",
                                          "English speak",
                                          "Other")),
         female = as.factor(case_when(sex == 2 ~ "Yes",
                            sex == 1 ~ "No")),
         childreninhh = cut(nchild, c(-1, 0, 1, 10)),
         adultsinhh = cut(numadult, c(-1, 0, 1, 2, 11)),
         long_cond = case_when(health == 1 ~ 1,
                               health == 2 ~ 0),
         work_limit = case_when(sf3a %in% c(1, 2) ~ 1,
                                sf3a > 2 ~ 0),
         benefit_prop = fihhmnsben/fihhmnnet1,
         benefit = as.factor(ifelse(benefit_prop > .6, "Yes", "No")),
         benefit = ifelse(is.na(benefit), "No", benefit),
         ineducation = as.factor(ifelse(fenow == 3, "Yes", "No")),
         health_sum = as.factor(long_cond + work_limit),
         education_fct = relabel(hiqual)
         ) %>% 
  dplyr::select(pidp, agecat:education_fct, hiqual)



# Do outcome variables ----------------------------------------------------



### add outcome variables
### another way to calcualte the outcomes

usw <- us1

usxwid <- usxwid_full %>% 
  dplyr::select(matches("ivf|pidp|month|hidp|pno"))

usw <- left_join(usw, usxwid, by = "pidp") 

# function to code missing: 1 = int, 3 = not issued, 0 = rest
us_out_code <- function(x){
  case_when(x == 1 ~ 1,
            x == -9 ~ 3,
            !(x %in% c(1, -9, 54, 55, 80, 84, 99)) ~ 0)
}


# function to rename output variables
left_to_nr <- function(x) {
  temp <- str_replace(x,
                      "^([a-z])_out$",
                      "out_\\1")
  temp_nr <- which(letters == str_extract(temp, "[a-z]$"))
  str_replace(temp, "out_[a-z]",
              str_c("out_", temp_nr))
}


# make new outcome variables
usw <- usw %>% 
  mutate_at(vars(matches("\\_ivfio$")),
            funs(out = us_out_code(.))) %>% 
  rename_all(funs(str_replace_all(., "ivfio_out",
                             "out"))) %>% 
  rename_at(vars(matches("[a-z]_out$")),
            funs(left_to_nr(.)))

# bring together with usw data    
usw <- left_join(usw, phase_data, by = "pidp")


## make missing patterns and save as excel

participation_patterns <- usw %>% 
  count(out_2, out_3, out_4, out_5, out_6) %>% 
  mutate(prop = round(n/sum(n), 3)) %>% 
  arrange(-n) 

write_excel_csv(participation_patterns, path = "./results/us_resp.csv")




## make variable that says how many times they participated before


# recode the ineligebles to 0 for this
small_data <- usw %>% 
  dplyr::select(starts_with("out")) %>% 
  mutate_all(funs(ifelse(. == 3, 0, .)))


out_res <- list(NULL)


for (i in 3:8) {
  res <- cbind(rowSums(small_data[, 1:(i - 2)], na.rm = T),
                    rowSums(is.na(small_data[, 1:(i - 2)])))
  
  colnames(res) <- c(str_c("part_", i),
                     str_c("noissue_", i))
  
  res <- tbl_df(res)
  
  out_res[[i]] <- res
}


usw <- tbl_df(cbind(usw, reduce(out_res[3:8], cbind)))
      

summary(usw)

ggplot(usw, aes(part_8 + 1)) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:7) +
  theme_bw() +
  labs(x = "Number of waves participated",
       y = "Count")

ggsave("./results/participation.png")

ggplot(usw, aes(noissue_8)) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:7) +
  theme_bw() +
  labs(x = "Number of times ineligible",
       y = "Count")

ggsave("./results/ineligible.png")


## save data

save(usw, file = "./data/usw.RData")


rm(out_list, out_res, data, 
   participation_patterns, res,
   temp_data, small_data, 
   us1, us1_select, us1h_full,
   us1r_full, us1h, usxw_full,
   usxwid_full)
gc()

# 05. Import contact history ----------------------------------------------

load("./data/usw.RData")

### function to get contact history we want from wave

# 
# folders <- list.files("./data/stata/", pattern = "_w[0-6]")
# 
# call_rec_data <- NULL
# call_rec_data <- map(
#   str_c("./data/stata/", folders, "/"),
#     get_callrec_us)
# 
# # get month sample info
# 
# index <- 2
# call_rec_data2 <- map(call_rec_data[2:6], function(x) {
# month_data <- usw %>%
#   select(str_c(letters[index], "_hidp"),
#          str_c("month_", index))
# 
# data <- month_data %>%
#     left_join(x)
# 
# index <<- index + 1
# 
# data
# 
# }
# )

# 
# 
# 
# # get call records indicators
# 
# rm(call_rec_data)
# gc()
# 
# #x <- call_rec_data2[[2]]
# 
# 
# index <- 5
# x <- call_rec_data2[[5]]
# 
# map(call_rec_data2[3:5], function(x) {
#   
#  step1 <- usw %>% 
#   dplyr::select(str_c(letters[index], "_hidp"),
#          str_c(c("int_phase_", "out_", "phase_j1end_"), 
#                index)) %>% 
#    group_by(f_hidp) %>% 
#    filter(row_number() == 1)
#   
#  
#  step2 <- semi_join(x, step1) %>% 
#    select(matches("hidp|month|status|contact"))
#  step3 <- left_join(step2, step1)
#  
#  data <- step3 %>% 
#    rename_all(funs(str_remove_all(., "_[0-9]|^[a-z]_"))) %>% 
#   group_by(month) %>% 
#   mutate(ph1 = phase_j1end,
#          ph1 = ifelse(is.na(ph1), 
#                       mean(ph1, na.rm = T), ph1),
#          ph1 = as_date(ph1)) %>%
#   filter(contact_day < mean(phase_j1end, na.rm = T)) %>% 
#   dplyr::select(-phase_j1end) %>%
#   filter(!is.na(status)) %>% 
#   filter(contact_day > 0) %>%
#     group_by(month, hidp) %>% 
#   mutate(
#     status_p1 = ifelse(
#     max(contact_day, na.rm = T) == contact_day, 
#     status, NA),
#     status_p1 = max(status_p1, na.rm = T),
#     status_p1 = ifelse(status_p1 == -Inf, NA, status_p1),
#     days_int = min(contact_day, na.rm = T) %--% 
#       max(contact_day, na.rm = T),
#     days_int = dseconds(days_int) / ddays(1)) %>%
#    ungroup() %>% 
#     dplyr::select(matches("hidp"), ph1, 
#                 status_p1, days_int) %>% 
#    group_by(hidp) %>% 
#    filter(row_number() == 1) %>% 
#    ungroup() %>% 
#    rename_all(funs(str_c(., "_", index) %>% 
#                     str_replace(., "hidp_[0-9]", 
#                                 str_c(letters[index],
#                                       "_hidp"))))
# 
#  save(data, file = str_c("./data/call_rec_", index, ".RData"))   
#  
#  rm(data)
#  gc()
#  
#  index <<- index + 1
# }
# )

# brin in saved data

call_rec_data3 <- list(NULL)
files <- str_c("./data/", list.files("./data/", pattern = "call_rec_"))
for (i in seq_along(files)) {
  load(files[i])
  call_rec_data3[[i]] <- data
  
}






# bring call reccord together with main data
data <- usw

map(call_rec_data3, function(x) {
  data <<- left_join(data, x)
})

labs <- c("no reply", "contact made",
          "appointment made", "any interviewing done",
          "any other status" )

usw <- data %>% 
  mutate_at(vars(matches("status_p1")),
            funs(factor(., labels = labs))) 

table(usw$status_p1_2, usw$out_2, useNA = "always")
table(usw$status_p1_2, useNA = "always")
table(usw$days_int_2, usw$out_2, useNA = "always")



# code 3 as non-response
usw <- usw %>%
  mutate_at(vars(matches("out_[2-6]")),
            funs(ifelse(. == 3, 0, .)))






# make new outcomes
usw <- usw %>% 
  mutate(
    out_p1_2 = ifelse(int_phase_2 > 1 |
                        out_2 == 0, 0, out_2),
    out_p1_3 = ifelse(int_phase_3 > 1 |
                        out_3 == 0, 0, out_3),
    out_p1_4 = ifelse(int_phase_4 > 1 |
                        out_4 == 0, 0, out_4),
    out_p1_5 = ifelse(int_phase_5 > 1 |
                        out_5 == 0, 0, out_5),
    out_p1_6 = ifelse(int_phase_6 > 1 |
                        out_6 == 0, 0, out_6),
    out_p3_2 = ifelse(int_phase_2 == 1 &
                        out_2 == 1, NA, out_2),
    out_p3_3 = ifelse(int_phase_3 == 1 &
                        out_3 == 1, NA, out_3),
    out_p3_4 = ifelse(int_phase_4 == 1 &
                        out_4 == 1, NA, out_4),
    out_p3_5 = ifelse(int_phase_5 == 1 &
                        out_5 == 1, NA, out_5),
    out_p3_6 = ifelse(int_phase_6 == 1 &
                        out_6 == 1, NA, out_6)
  ) 


count(usw, int_phase_3, out_p1_3, out_p3_3, out_3)


save(usw, file = "./data/usw2.RData")

# 06. Get independent variables from each wave ----------------------------



varsi <- c("pidp", "hidp", "dvage", "jbhrs", "jbstat",
           "gor_dv", "sex", "mastat_dv",
           "hhtype_dv", "nchild_dv", "hiqual_dv",
           "urban_dv", "hhresp_dv", "sf1", "health",
           "xpmove", "ivfio", "fibenothr_dv",
           "sf3a", "fenow")

varsh <- c("hidp", "hhsize", "hsownd", 
           "fihhmnsben_dv", "fihhmnnet1_dv")

folder <- "./data/stata/"
folders <- str_c(folder, 
        list.files(folder, pattern = "_w[1-6]"),
         "/")

test <- map(folders, get_us_main_data, varsi, varsh)


# clean data

# have to import ukborn

index <- 0

test_clean <- map(test, function(x) {
  
  index <<- index + 1
  
  x %>%
    mutate(
      agecat = cut(dvage, c(15, 19, 24, 34,
                            44, 54, 64, 74,
                            105)),
      employed = case_when(jbstat == 3 ~ 1,
                           jbstat %in% c(1, 2) ~ 2,
                           jbstat > 3 ~ 0),
      employed_fct = factor(
        employed,
        labels = c("Not in labour force",
                   "Unemployed",
                   "Employed")
      ),
      jbhrs = ifelse(employed %in% c(0, 1), 0, jbhrs),
      jobhrs = cut(jbhrs, c(-1, 0, 14, 29, 39, 49, 100)),
      owner = as.factor(case_when(hsownd %in% 1:3 ~ "Yes",
                                  hsownd > 3 ~ "No")),
      relationship = case_when(mastat %in% 2:3 ~ 1,
                               mastat == 10 ~ 2,
                               mastat %in% 4:9 ~ 3,
                               mastat == 1 ~ 4),
      relationship_fct = factor(
        relationship,
        labels = c("Married",
                   "De facto",
                   "Separated",
                   "Single")
      ),
      likelymove = as.factor(case_when(xpmove == 1 ~ "Yes",
                                       xpmove == 2 ~ "No")),
      female = as.factor(case_when(sex == 2 ~ "Yes",
                                   sex == 1 ~ "No")),
      numadult = hhsize - nchild,
      childreninhh = cut(nchild, c(-1, 0, 1, 10)),
      adultsinhh = cut(numadult, c(-1, 0, 1, 2, 11)),
      long_cond = case_when(health == 1 ~ 1,
                            health == 2 ~ 0),
      work_limit = case_when(sf3a %in% c(1, 2) ~ 1,
                             sf3a > 2 ~ 0),
      benefit_prop = fihhmnsben / fihhmnnet1,
      benefit = as.factor(case_when(benefit_prop > .6 ~ "Yes",
                                    TRUE ~ "No")),
      ineducation = as.factor(ifelse(fenow == 3, "Yes", "No")),
      health_sum = as.factor(long_cond + work_limit),
      education_fct = relabel(hiqual)
    ) %>%
    dplyr::select(pidp, agecat:education_fct, hiqual) %>% 
    rename_all(funs(str_c(., "_", index) %>% 
                      str_replace(., "pidp_[0-9]", "pidp")))
    
})


wide_us <- reduce(test_clean, left_join, by = "pidp")


wide_us <- semi_join(wide_us, usw, by = "pidp")


# get info from previous wave if info is missing


vars <- unique(str_remove(colnames(wide_us), "_[0-9]"))[-1]

i <- 1

nice_data <- list(NULL)

for (i in seq_along(vars)) {
  data <- wide_us %>%
    dplyr::select("pidp", matches(str_c(vars[i], "_[0-9]$")))
  
  var_clean <- str_c(vars[i], "_", 1:6)
  
  for (z in 2:6) {
    index <- is.na(data[var_clean[z]])
    
    data[index, var_clean[z]] <- data[index, var_clean[z - 1]]
  }
  
  nice_data[[i]] <- data
  
}


wide_us <- reduce(nice_data, full_join, by = "pidp")

wide_us <- usw %>% 
  dplyr::select(pidp, countryofbirth_fct,
         matches("out|month|int_phase")) %>% 
  left_join(wide_us, by = "pidp")



save(usw, wide_us, 
     file = "./data/clean_us_v1.RData")



