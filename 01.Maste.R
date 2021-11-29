###
#
# Adaptive survye design in longitudinal studies
#       with Nicole Watson 
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

# load packages from packrat
pkg <- c("tidyverse", "haven", "lubridate", "purrr")

sapply(pkg, library, character.only = T)


# load functions
functions <- list.files("./functions/", pattern = ".R")
map(str_c("./functions/", functions), source)


# 01 Make phase variable ----------------------------------------------------



# get wave 1 data and cross wave information
us1r_full <- read_dta("./data/stata/us_w1/a_indresp.dta")
us1h_full <- read_dta("./data/stata/us_w1/a_hhresp.dta")
us1_indall <- read_dta("./data/stata/us_w1/a_indall.dta")
usxw_full <- read_dta("./data/stata/us_wx/xwavedat.dta")
usxwid_full <- read_dta("./data/stata/us_wx/xwaveid.dta")


# set up phase list
phase_list <- list(NULL)

# vars of interest
vars <- c("intdatd_dv", "intdatm_dv", "intdaty_dv",
          "month", "ivfio", "hidp")

# there are different phases for different waves
phase1 <- c(0, 6, 6, 6, 12, 10)
phase2 <- c(0, 6, 6, 6, 4, 6)
phase3 <- c(0, 4, 4, 9, 3, 4)


for (i in 2:6) {
  
  # get date data from all waves
  temp_data <- read_dta(str_c("./data/stata/us_w", i, "/",
                              letters[i], "_indall.dta"))

  # clean data
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
  
  # put in list
  phase_list[[i]] <- temp_data
    
}

# move from list to data frame
phase_data <- reduce(phase_list[2:6], full_join)



# take into account that year 2 of wave 4 had a different data collection
# period

phase_data[as.numeric(phase_data$month_4) %in% 13:24,
           c("phase_j1end_4", "phase_j2end_4", "phase_j3end_4")] <-
  phase_data %>%
  filter(as.numeric(month_4) %in% 13:24) %>%
  group_by(month_4) %>%
  mutate(
    phase_j1end_4 = min(intdate_4, na.rm = T) + weeks(phase1[5]),
    phase_j2end_4 = phase_j1end_4 + weeks(phase2[5]),
    phase_j3end_4 = phase_j2end_4 + weeks(phase3[5])
  ) %>%
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

# delete month variables as they are just for respondents
# will get them later from usxw_full
phase_data <- phase_data %>% 
  select(-matches("month"))


# 02. Select cases of interest --------------------------------------------

# select baseline respondents from the general sample of GB
us1_select <- us1r_full %>% 
  filter(a_hhorig == 1) %>% # GB general sample
  filter(a_ivfio == 1) %>% # full interview
  dplyr::select(pidp)




# 03. Make interview outcome variables ------------------------------------

# ineligible values
inel_vals <- c(-9, 54, 55, 80, 84, 99)

# make empty list
out_list <- list(NULL)

# files of interest
vars <- c("ivfio", "issued2w")


# loop to get data
for (i in 2:6) {
  
  # get the data
  data <- read_dta(str_c("./data/stata/us_w", i, "/", letters[i],
                         "_indsamp.dta"))

  # rename, filter, create varible and join
  data <- data %>%
    .[!duplicated(.$pidp), ] %>%  # delete duplicates
    rename_all(funs(str_remove(., str_c(letters[i], "_")))) %>%
    mutate(out = case_when(ivfio == 1 ~ 1,
                           ivfio %in% inel_vals ~ NA_real_, # code inel as miss
                           TRUE ~ 0),
           out = ifelse(issued2w == 0, NA, out)) %>%
    dplyr::select(pidp, out, ivfio, hidp, issued2w) %>%
    right_join(us1_select, by = "pidp") # link with respondents in wave 1
  
  # put in list
  out_list[[i]] <- data
  names(out_list)[i] <- str_c("us", i)
}


# code dead as missing
for (i in 2:6) {
  # code as missing if dead
  out_list[[i]] <- usxw_full %>%
    mutate(dead = ifelse(dcsedw_dv %in% 1:i, 1, 0)) %>%
    dplyr::select(pidp, dead) %>%
    right_join(out_list[[i]]) %>%
    mutate(out = ifelse(dead == 1, NA, out),
           not_issued = ifelse(is.na(ivfio), 1, 0),
           not_issued = ifelse(dead == 1 | ivfio %in% inel_vals, 
                               NA, not_issued), # make not issue var
           not_issued = ifelse(issued2w == 0 & not_issued == 0, 
                               1, not_issued)) %>% 
    rename_all(~ str_c(., "_", i)) %>% # give labels
    rename_all(~ str_replace(., "pidp_[0-9]", "pidp")) %>% 
    rename_all(~ str_replace(., "hidp_[0-9]", str_c(letters[i], "_hidp")))
  
}

usw <- reduce(out_list[2:6], full_join, by = "pidp") %>% 
  select(-matches("dead"), -matches("issued2w"))

# make version of outcome where not issued cases are nonrespondents
usw <- usw %>% 
  mutate(out2_2 = ifelse(not_issued_2 == 1 & is.na(out_2), 0, out_2),
         out2_3 = ifelse(not_issued_3 == 1 & is.na(out_3), 0, out_3),
         out2_4 = ifelse(not_issued_4 == 1 & is.na(out_4), 0, out_4),
         out2_5 = ifelse(not_issued_5 == 1 & is.na(out_5), 0, out_5),
         out2_6 = ifelse(not_issued_6 == 1 & is.na(out_6), 0, out_6)) 

# response rates are not linear as more cases were not issued and are treated as ineligible
sapply(select(usw, matches("out_")), summary)
sapply(select(usw, matches("out2_")), summary)


## make missing patterns but treat not issued as non-resp and save as excel
participation_patterns <- usw %>%
  count(out_2, out_3, out_4, out_5, out_6) %>% 
  mutate(prop = round(n/sum(n), 3)) %>% 
  arrange(-n) 

write_excel_csv(participation_patterns, path = "./results/us_resp.csv")



# make sum variables
usw <- usw %>% 
  mutate(sum_participate = make_rowsum(., "out2_"),
         sum_notissued = make_rowsum(., "not_issued"),
         sum_nonresp = make_rowsum(., "out_", value = 0)) 


ggplot(usw, aes(sum_participate + 1)) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:7) +
  theme_bw() +
  labs(x = "Number of waves participated",
       y = "Count")

ggsave("./results/participation.png")

ggplot(usw, aes(sum_notissued)) +
  geom_histogram() +
  scale_x_continuous(breaks = 0:4) +
  theme_bw() +
  labs(x = "Number of times not issued",
       y = "Count")

ggsave("./results/notissued.png")


## graph of response rates when non-issued are treated as ineligible

usw %>%
  select(matches("out_")) %>% 
  summarise_all(mean, na.rm = T) %>% 
  gather() %>%
  rbind(data.frame(key = "out_1", value = 1)) %>% 
  arrange(key) %>% 
  mutate(wave = 1:6) %>% 
  ggplot(aes(wave, value)) + geom_point() + geom_line(group = 1) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     limits = c(0, 1)) +
  theme_bw() + 
  labs(x = "Wave",
       y = "Response rates")


# RR when non-issued are treated as non-respondents
usw %>%
  select(matches("out2_")) %>% 
  summarise_all(mean, na.rm = T) %>% 
  gather() %>%
  rbind(data.frame(key = "out_1", value = 1)) %>% 
  arrange(key) %>% 
  mutate(wave = 1:6) %>% 
  ggplot(aes(wave, value)) + geom_point() + geom_line(group = 1) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     limits = c(0, 1)) +
  theme_bw() + 
  labs(x = "Wave",
       y = "Response rates")





# 04. Make variables of interest ------------------------------------------

# select variables of interest from cross file
usxw <- usxw_full %>% 
  dplyr::select(pidp, ukborn, plbornc)

# select variables of interest
varsi <- c("hidp", "dvage", "jbhrs", "jbstat",
           "gor_dv", "sex", "mastat_dv",
           "hhtype_dv", "nchild_dv", "hiqual_dv",
           "urban_dv", "hhresp_dv", "sf1", "health",
           "xpmove", "ivfio", "fibenothr_dv",
           "sf3a", "fenow", "jbft_dv")

varsh <- c("hidp", "hhsize", "hsownd", "numadult",
           "fihhmnsben_dv", "fihhmnnet1_dv")

# get household data
us1h <- us1h_full %>% 
  dplyr::select(str_c("a_", varsh))

# get individual data and join with household and crosswave
us1 <- us1r_full %>% 
  dplyr::select(pidp, str_c("a_", varsi)) %>% 
  left_join(us1h, by = "a_hidp") %>% 
  rename_all(funs(str_remove_all(., 'a_') %>%
                    str_remove_all(., '_dv|ff_'))) %>% 
  left_join(usxw, by = "pidp")


# remove proxy
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
         emply_type = case_when(employed_fct == "Employed" &
                                  jbft == 1 ~ "Employed full time",
                                employed_fct == "Employed" &
                                  jbft == 2 ~ "Employed part time",
                                is.na(employed_fct) &
                                  jbhrs > 29 ~ "Employed full time",
                                TRUE ~ as.character(employed_fct)),
         emply_type = ifelse(emply_type == "Employed", NA, emply_type),
         emply_type = as.factor(emply_type),
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
                                          xpmove == 2 ~ "No",
                                          TRUE ~ "Missing")),
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
         ineducation = as.factor(ifelse(fenow == 3, "Yes", "No")),
         health_sum = as.factor(long_cond + work_limit),
         urb = case_when(urban == 1 & gor == 7 ~ "London",
                         urban == 1 & gor != 7 ~ "Other urban",
                         urban == 2 ~ "Rural"),
         urb = as.factor(urb),
         education_fct = relabel(hiqual),
         education_fct = fct_recode(education_fct,
                                           NULL = "Missing",
                                           NULL = "refused",
                                           NULL = "Don't know")) %>% 
  dplyr::select(pidp, agecat:education_fct, hiqual)

# bring together with dataset 
usw <- left_join(usw, us1, by = "pidp") %>% 
  left_join(phase_data, by = "pidp")


# get month data

small_month_data <- usxwid_full %>% 
  select(pidp, matches("month")) 

# change names
names(small_month_data)[2:length(small_month_data)] <- 
  str_c("month_", 1:(length(small_month_data) - 1))

# recode month variables
small_month_data <- small_month_data %>% 
  mutate_at(vars(matches("month")),
            ~relabel(.) %>% fct_recode(., NULL = "missing")) %>% 
  mutate_at(vars(matches("month")),
            funs(missing = ifelse(is.na(.), 1, 0))) %>% 
  rename_at(vars(matches("missing")),
            ~str_replace(., "month_([0-9])_missing", "month_miss_\\1"))



# merge with cleaned wave 1 info
usw <- left_join(usw, small_month_data)


count(usw, month_6, not_issued_6, month_miss_6) %>% print(n = 51)

count(usw, month_6, not_issued_6, month_miss_6) %>% print(n = 51)


## save data

save(usw, file = "./data/usw.RData")


rm(out_list, data, usxw,
   participation_patterns, 
   temp_data, us1, us1_select, us1h_full,
   us1r_full, us1h, usxw_full,
   usxwid_full)
gc()

# 05. Import contact history ----------------------------------------------

load("./data/usw.RData")

# syntax that gets call record
# we need to run it only once and then saves outcomes
# source("./get_call_rec.R")

# bring in saved data
call_rec_data <- map(list.files("./data/",
                            pattern = "call_rec.+RDS",
                            full.names = T),
                 read_rds)

# bring call record together with main data
data <- usw

map(call_rec_data, function(x) {
  data <<- left_join(data, x)
})


# code missing for cases when they were not present in call rec data
data <- data %>% 
  mutate_at(vars(matches("phase_miss")),
            ~ ifelse(is.na(.), "Not in call rec data", .))

data %>% count(not_issued_2, month_miss_2, phase_miss_2)
data %>% count(not_issued_3,phase_miss_3)
data %>% count(not_issued_4,phase_miss_4)
data %>% count(not_issued_5,phase_miss_5)
data %>% count(not_issued_6, month_miss_6,phase_miss_6)

data %>% count(not_issued_6, out2_6, phase_miss_6, month_miss_6)



# code missing categories for call record data
# and make variable that call record is not present


fct_vars <- c("status_p1", "last_daytime", "last_weekday",
              "anyinter_ph1")

cont_vars <- c("days_int", "days_int2", "nrcalls", "nrcalls2",
               "noreply_ph1", "noreply2_ph1", "nrcalls_ph23")

# make a variable if we have missing call records (if missing both in phase 1 
# and after) code missing category for categorical and as 0 for continious

usw2 <- data %>%
  mutate(call_miss_2 = ifelse(is.na(nrcalls_2), 1, 0),
         call_miss_3 = ifelse(is.na(nrcalls_3), 1, 0),
         call_miss_4 = ifelse(is.na(nrcalls_4), 1, 0),
         call_miss_5 = ifelse(is.na(nrcalls_5), 1, 0),
         call_miss_6 = ifelse(is.na(nrcalls_6), 1, 0)) %>% 
  mutate_at(vars(str_c(fct_vars, "_", rep(2:6, each = length(fct_vars)))),
            ~fct_explicit_na(.) %>% 
              fct_recode("missing" = "(Missing)")) %>% 
  mutate_at(vars(str_c(cont_vars, "_", rep(2:6, each = length(cont_vars)))),
            ~ifelse(is.na(.), 0, .)) %>% 
  mutate(call_miss_2 = ifelse(status_p1_2 == "missing" & call_miss_2 == 0 &
                                nrcalls_ph23_2 == 0,
                              1, call_miss_2),
         call_miss_3 = ifelse(status_p1_3 == "missing" & call_miss_3 == 0 &
                                nrcalls_ph23_3 == 0,
                              1, call_miss_3),
         call_miss_4 = ifelse(status_p1_4 == "missing" & call_miss_4 == 0 &
                                nrcalls_ph23_4 == 0,
                              1, call_miss_4),
         call_miss_5 = ifelse(status_p1_5 == "missing" & call_miss_5 == 0 &
                                nrcalls_ph23_5 == 0,
                              1, call_miss_5),
         call_miss_6 = ifelse(status_p1_6 == "missing" & call_miss_6 == 0 &
                                nrcalls_ph23_6 == 0,
                              1, call_miss_6))




count(usw2, out_3, out2_3, int_phase_3)
count(usw2, out_3, out2_3, int_phase_3, ivfio_3) %>% print(n = 50)

count(usw2, out_6, out2_6, int_phase_6)
count(usw2, out_6, out2_6, int_phase_6, ivfio_6) %>% print(n = 50)

# there are a few cases that appear to have full interview in one dataset
# (indall - have ivfio == 1 and have interview date) and be non-respondents in
# another (indsamp)

# lets look where the inconsistency comes from
strange_cases <- usw2 %>% 
  filter(out_6 == 0 & int_phase_6 > 0) %>% 
  select(pidp, out_6, int_phase_6, ivfio_6)

indall6 <- read_dta("./data/stata/us_w6/f_indall.dta",
                    col_select = c("pidp", "f_ivfio"))

left_join(strange_cases, indall6) %>% 
  count(ivfio_6, f_ivfio)

# some non-respondents in indsamp are respondents in indall

# action -> code them as respondents in our outcome variables

usw3 <- usw2 %>% 
  mutate(out_2 = ifelse(int_phase_2 > 0 & !is.na(int_phase_2), 1, out_2),
         out_3 = ifelse(int_phase_3 > 0 & !is.na(int_phase_3), 1, out_3),
         out_4 = ifelse(int_phase_4 > 0 & !is.na(int_phase_4), 1, out_4),
         out_5 = ifelse(int_phase_5 > 0 & !is.na(int_phase_5), 1, out_5),
         out_6 = ifelse(int_phase_6 > 0 & !is.na(int_phase_6), 1, out_6),
         
         out2_2 = ifelse(int_phase_2 > 0 & !is.na(int_phase_2), 1, out2_2),
         out2_3 = ifelse(int_phase_3 > 0 & !is.na(int_phase_3), 1, out2_3),
         out2_4 = ifelse(int_phase_4 > 0 & !is.na(int_phase_4), 1, out2_4),
         out2_5 = ifelse(int_phase_5 > 0 & !is.na(int_phase_5), 1, out2_5),
         out2_6 = ifelse(int_phase_6 > 0 & !is.na(int_phase_6), 1, out2_6),
         
         not_issued_6 = ifelse(is.na(not_issued_6) & int_phase_6 > 0,
                               0, not_issued_6))

count(usw2, out_6, out2_6, int_phase_6)
count(usw3, out_6, out2_6, int_phase_6)


# make new outcomes at end of phase 1
usw3 <- usw3 %>% 
  mutate(
    # outcome at the end phase 1
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

    # outcome at end of phase 3 but code phase 1 resp as missing (non-elig)
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

# make outcomes where we treat not issued cases as non-respondents instead of
# ineligible

usw3 <- usw3 %>% 
  mutate(
    # outcome at the end phase 1
    out2_p1_2 = ifelse(not_issued_2 == 1, 0, out_p1_2),
    out2_p1_3 = ifelse(not_issued_3 == 1, 0, out_p1_3),
    out2_p1_4 = ifelse(not_issued_4 == 1, 0, out_p1_4),
    out2_p1_5 = ifelse(not_issued_5 == 1, 0, out_p1_5),
    out2_p1_6 = ifelse(not_issued_6 == 1, 0, out_p1_6),
    
    # outcome at end of phase 3 but code phase 1 resp as missing (non-elig)
    out2_p3_2 = ifelse(not_issued_2 == 1, 0, out_p3_2),
    out2_p3_3 = ifelse(not_issued_3 == 1, 0, out_p3_3),
    out2_p3_4 = ifelse(not_issued_4 == 1, 0, out_p3_4),
    out2_p3_5 = ifelse(not_issued_5 == 1, 0, out_p3_5),
    out2_p3_6 = ifelse(not_issued_6 == 1, 0, out_p3_6)
  ) 


# check to see if it looks reasonable
count(usw3, out_6, out2_6, not_issued_6,int_phase_6)
count(usw3, out_6, int_phase_6, out_p1_6, out_p3_6)
count(usw3, out2_6, int_phase_6, out2_p1_6, out2_p3_6)


# make variable of anyinterview done in phase 1 in the household (except that
# pers)

usw3 <- usw3 %>% 
  group_by(b_hidp) %>% 
  mutate(hhint_ph1_2 = sum(out2_p1_2, na.rm = T) - out2_p1_2,
         hhint_ph1_2 = ifelse(hhint_ph1_2 > 1, 1, hhint_ph1_2)) %>% 
  group_by(c_hidp) %>% 
  mutate(hhint_ph1_3 = sum(out2_p1_3, na.rm = T) - out2_p1_3,
         hhint_ph1_3 = ifelse(hhint_ph1_3 > 1, 1, hhint_ph1_3)) %>% 
  group_by(d_hidp) %>% 
  mutate(hhint_ph1_4 = sum(out2_p1_4, na.rm = T) - out2_p1_4,
         hhint_ph1_4 = ifelse(hhint_ph1_4 > 1, 1, hhint_ph1_4)) %>% 
  group_by(e_hidp) %>% 
  mutate(hhint_ph1_5 = sum(out2_p1_5, na.rm = T) - out2_p1_5,
         hhint_ph1_5 = ifelse(hhint_ph1_5 > 1, 1, hhint_ph1_5)) %>% 
  group_by(f_hidp) %>% 
  mutate(hhint_ph1_6 = sum(out2_p1_6, na.rm = T) - out2_p1_6,
         hhint_ph1_6 = ifelse(hhint_ph1_6 > 1, 1, hhint_ph1_6)) %>% 
  ungroup()


# code intphase as 0 if non respondents
usw3 <- usw3 %>% 
  mutate(int_phase_2 = ifelse(is.na(int_phase_2) & out_2 == 0, 0, int_phase_2),
         int_phase_3 = ifelse(is.na(int_phase_3) & out_3 == 0, 0, int_phase_3),
         int_phase_4 = ifelse(is.na(int_phase_4) & out_4 == 0, 0, int_phase_4),
         int_phase_5 = ifelse(is.na(int_phase_5) & out_5 == 0, 0, int_phase_5),
         int_phase_6 = ifelse(is.na(int_phase_6) & out_6 == 0, 0, int_phase_6))


# list the variables we want in our model
vars <- c("adultsinhh", "agecat", "benefit", "childreninhh",
          "countryofbirth_fct", "education_fct", "employed_fct",
          "ineducation", "health_sum", "likelymove",
          "owner", "relationship_fct", "female", "urb")

usw3 %>% 
  select(vars) %>% 
  summarise_all(~(sum(is.na(.), na.rm = T)/NROW(.)) * 100) %>% 
  gather() %>%
  arrange(desc(value))
  
call_rec_vars <- c(fct_vars, cont_vars) %>% 
  str_replace("anyinter_ph1", "hhint_ph1")

usw3 %>% 
  select(str_c(call_rec_vars, "_", rep(2:6, each = length(call_rec_vars)))) %>% 
  summarise_all(~(sum(is.na(.), na.rm = T)/NROW(.)) * 100) %>% 
  gather() %>%
  arrange(desc(value)) %>% print(n = 55)



# code any interviewing done if missing as no

usw3 <- usw3 %>% 
  mutate_at(vars(matches("anyinter_ph1")),
            ~fct_recode(.,"No" = "missing")) 

# in wave 6 the call records underestimate "any interview done" compared to the
# previous waves, probably because of the new data collection agency
# we use observe outcomes to recode this in wave 6 


usw3 <- usw3 %>% 
  mutate(status_p1_6 = ifelse(hhint_ph1_6 == 1 | out_p1_6 == 1,
                              as.character("any interviewing done"), 
                              as.character(status_p1_6)) %>% 
           as.factor(),
         anyinter_ph1_6 = ifelse(status_p1_6 == "any interviewing done",
                                 as.character("Yes"), 
                                 as.character(anyinter_ph1_6)) %>% 
           as.factor()) 

# save data
save(usw3, file = "./data/usw2.RData")


# 06. Get independent variables from each wave --------------------------------

# variables of interest
varsi <- c("pidp", "hidp", "dvage", "jbhrs", "jbstat",
           "gor_dv", "sex", "mastat_dv",
           "hhtype_dv", "nchild_dv", "hiqual_dv",
           "urban_dv", "hhresp_dv", "sf1", "health",
           "xpmove", "ivfio", "fibenothr_dv",
           "sf3a", "fenow", "jbft_dv")

varsh <- c("hidp", "hhsize", "hsownd",
           "fihhmnsben_dv", "fihhmnnet1_dv")


folders <- list.files("./data/stata/",
           pattern = "_w[1-6]",
           full.names = T) %>%
  str_c("/")

# old syntax, the above is shorter and should work...
# folder <- "./data/stata/"
# folders <- str_c(folder,
#         list.files(folder, pattern = "_w[1-6]"),
#          "/")

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
                   "Employed")),
      emply_type = case_when(employed_fct == "Employed" &
                               jbft == 1 ~ "Employed full time",
                             employed_fct == "Employed" &
                               jbft == 2 ~ "Employed part time",
                             is.na(employed_fct) &
                               jbhrs > 29 ~ "Employed full time",
                             TRUE ~ as.character(employed_fct)),
      emply_type = ifelse(emply_type == "Employed", NA, emply_type),
      emply_type = as.factor(emply_type),
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
                   "Single")),
      likelymove = as.factor(case_when(xpmove == 1 ~ "Yes",
                                       xpmove == 2 ~ "No",
                                       TRUE ~ "Missing")),
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
      urb = case_when(urban == 1 & gor == 7 ~ "London",
                      urban == 1 & gor != 7 ~ "Other urban",
                      urban == 2 ~ "Rural"),
      education_fct = relabel(hiqual),
      education_fct = fct_recode(education_fct,
                                 NULL = "Missing",
                                 NULL = "refused",
                                 NULL = "Don't know",
                                 NULL = "Inapplicable",
                                 "Other higher" = "Other higher degree",
                                 "A level etc" = "A-level etc",
                                 "Other qual" = "Other qualification",
                                 "No qual" = "No qualification")
    ) %>%
    dplyr::select(pidp, agecat:education_fct, hiqual, urban) %>%
    rename_all(funs(str_c(., "_", index) %>%
                      str_replace(., "pidp_[0-9]", "pidp")))
  })

# bring together all the data
wide_us <- reduce(test_clean, left_join, by = "pidp")

# keep cases that responded in wave 1
wide_us <- semi_join(wide_us, usw3, by = "pidp")


# get info from previous wave if info is missing

vars <- unique(str_remove(colnames(wide_us), "_[0-9]"))[-1]

i <- 1


nice_data <- list(NULL)

for (i in seq_along(vars)) {
  data <- wide_us %>%
    dplyr::select("pidp", matches(str_c(vars[i], "_[0-9]$")))

  var_clean <- str_c(vars[i], "_", 1:6)

  for (z in 2:6) {

    # make index if they have missing 
    index <- is.na(data[var_clean[z]]) 

    # if I want to restrict to respondents I can add:
    # & data$pidp %in% test_clean[[z]]$pidp
    
    # if not the same type make the numeric
    rule1 <- map(data[, -1], class) %>% unique() %>% length() > 1
    rule2 <- map(data[, -1], function(x) attributes(x)$labels) %>% 
      unique() %>% length() > 1 
      
    if (rule1 | rule2  == T) {
     data <- mutate_all(data, as.numeric)
    }
    
    # replace with previous wave data
      data[index, var_clean[z]] <- data[index, var_clean[z - 1]]

  }

  nice_data[[i]] <- data

}


wide_us2 <- reduce(nice_data, full_join, by = "pidp")

wide_us2 <- usw3 %>%
  dplyr::select(pidp, countryofbirth_fct,
         matches("out|month|int_phase")) %>%
  left_join(wide_us2, by = "pidp")

table(wide_us$jobhrs_3,
      wide_us2$jobhrs_3,
      useNA = "always")


######### check missing percentages

miss1 <- wide_us %>%
  summarise_all(~(sum(is.na(.), na.rm = T)/NROW(.)) * 100) %>%
  gather() %>%
  rename(prop_origin = value)

miss2 <- wide_us2 %>%
  summarise_all(~(sum(is.na(.), na.rm = T)/NROW(.)) * 100) %>%
  gather() %>%
  rename(prop_impute = value)

miss <- left_join(miss1, miss2) %>%
  mutate(diff_miss = prop_origin - prop_impute) %>%
  arrange(desc(diff_miss))

# get the attrition rates for each wave and delete that from 
# missing proportions

for (i in 1:6) {
  usw3$pidp %in% test_clean[[i]]$pidp
}



print(miss, n = 200)

write_csv(miss, "./results/miss_table.csv")



# Make descriptives -------------------------------------------------------

# descriptives of socio-dems used in models

vars_dem1 <- c("adultsinhh", "agecat", "benefit", "childreninhh",
               "countryofbirth_fct", "education_fct", "employed_fct",
               "ineducation", "health_sum", "likelymove",
               "owner", "relationship_fct", "female", "urb")

desc_demo <- sum_tab(select(usw3, vars_dem1))

write_csv(desc_demo$`Categorical variables`,
          "./results/desc_demo_cat.csv")



# descriptives of call record by wave


# eliminate not issued cases from each wave and then make statistics


call_rec_vars_small <- str_subset(call_rec_vars, 
                                  c("days_int2|nrcalls2|noreply2|hhint"),
                                  negate = T) %>% 
  c("anyinter_ph1")


eligible_calls <- map(2:6, function(x) {
  # code variables we want
  vars_sel <- str_c(call_rec_vars_small, "_", x)
  index <- str_c("not_issued_", x)
  
  # filter eligible cases
  usw3 %>% 
    filter((!!sym(index)) == 0) %>% 
    select(pidp, vars_sel)
  
})


desc_call_rec <- sum_list2(eligible_calls) 

write_csv(desc_call_rec$Continious, "./results/desc_call_noinel_cont.csv")
write_csv(desc_call_rec$Categorical, "./results/desc_call_noinel_cat.csv")


# Save the data -----------------------------------------------------------


# save data
save(usw3, wide_us2,
     file = "./data/clean_us_v1.RData")



