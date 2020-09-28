######################################################
# 
# # do regressions for R indicators 
# 
# 
# 
######################################################


# 0. Set-up ---------------------------------------------------------------


# clean memory
rm(list = ls())
gc()

# load packages from packrat
pkg <- c("tidyverse", "haven", "lubridate",
    "devtools", "pROC")

sapply(pkg, library, character.only = T)


# load functions
list.files("./functions/",
           pattern = "\\.R$",
           full.names = T) %>% 
  map(source)




# Descriptive statistics for independent variables ------------------------

# load data
load("./data/clean_us_v1.RData")


# list the variables we want in our model
vars <- c("adultsinhh", "agecat", "benefit", "childreninhh",
          "countryofbirth_fct", "education_fct", "employed_fct",
          "ineducation", "health_sum", "likelymove",
          "owner", "relationship_fct", "female", "urb")


# call record variable
call_rec_vars <- c("nrcalls", "nrcalls2", "last_weekday", "days_int", 
                   "days_int2", "status_p1", "last_daytime", "noreply_ph1",
                   "noreply2_ph1", "hhint_ph1","call_miss")

call_rec_vars_all <- str_c(rep(call_rec_vars, 5), "_", 
                           rep(2:6, each = length(call_rec_vars)))


# get vars of interest
usw_small <- usw3 %>% 
  dplyr::select(agecat:education_fct,
                call_rec_vars_all,
                starts_with("out_"),
                starts_with("int_phase"),
                starts_with("status_p1"),
                starts_with("days_int")) %>% 
  dplyr::select(-employed, -relationship,
                -benefit_prop, -countryofbirth,
                -long_cond, -work_limit)

# make outcomes
outcomes <- str_c("out_p1_", c(2:6))


# loop to explain outcomes in phase 1 waves 2-6 ---------------------------



models_p1 <- list(NULL)

for (i in seq_along(outcomes)) {
  
  print(outcomes[i])
  
  # regression model 
  response_model <- formula(str_c(outcomes[i], " ~ ",
                                  str_c(vars, collapse = " + ")))
  # delete item mising
  data <- usw_small %>%
    dplyr::select(vars, outcomes[i]) %>%
    na.omit()
  
  # run logistic regressions for phase 1 outcomes
  models_p1[[i]] <- glm(response_model, data = data,
                        family = "binomial")
  
  names(models_p1)[i] <- outcomes[i]
}


# Loop to explain outcomes in phase 3 waves 2-6 ---------------------------



outcomes <- str_c("out_p3_", c(2:6))

models_p3 <- list(NULL)

for (i in seq_along(outcomes)) {
  
  print(outcomes[i])
  
  index <- i + 1
  
  vars2 <- c(vars, str_c(call_rec_vars, "_", index))
  
  response_model <- formula(str_c(outcomes[i], " ~ ",
                                  str_c(vars2, collapse = " + ")))
  
  data <- usw_small %>%
    dplyr::select(vars2, outcomes[i]) %>%
    na.omit()
  
  models_p3[[i]] <- glm(response_model, data = data,
                        family = "binomial")
  
  names(models_p3)[i] <- outcomes[i]
}



# bring models together and look at descriptives
list_res <- c(models_p1, models_p3)

model_fit <- map(list_res, broom::glance) %>%
  reduce(rbind)

model_fit$model <- map(map(list_res, names), names) %>% 
  names()


# get regression coefficients
reg_coefs <- map(list_res, broom::tidy) %>%
  reduce(rbind)

# give model name
model_names <- map(list_res, broom::tidy) %>% 
  map(nrow) %>% unlist()

reg_coefs$model <- map2(.x = names(model_names), 
                        .y = model_names, rep) %>% unlist()




# clean results
model_fit <- model_fit %>% 
  tbl_df() %>% 
  separate(model, into = c("out", "outcome", "wave"), sep = "_") %>% 
  select(-out) %>% 
  mutate(r2 = 1 - (deviance/null.deviance),
         n = unlist(
           map(c(models_p1, models_p3), 
               function(x) length(resid(x)))))



# figure of R2 for models
model_fit %>% 
  ggplot(aes(wave, r2, colour = outcome, group = outcome)) + 
  geom_point() + geom_line() +
  theme_bw() +
  labs(title = "R2 for models for two outcomes by wave")

ggsave("./results/reg_r2.png", dpi = 500)



# visualize AUC performence
model_fit$auc <- unlist(map(c(models_p1, models_p3), easy_roc))

model_fit %>% 
  ggplot(aes(wave, auc, colour = outcome, group = outcome)) + 
  geom_point() + geom_line() +
  theme_bw() +
  labs(title = "AUC for models for two outcomes by wave")

ggsave("./results/reg_auc.png", dpi = 500)


# export regression coefficients and model fit
write_csv(model_fit, "./results/models_fit_p1p3.csv")
write_csv(reg_coefs, "./results/reg_coeffs_p1p3.csv")


# make loop to create predicted probabilities -----------------------------


new_data <- list(NULL)

for (i in 3:6) {

  # make list of variables to use in prediction
  sel_vars <- c(vars[1:14], 
                str_c(call_rec_vars, "_", i))

  # select data
  data <- usw3 %>%
    select(sel_vars, pidp) %>%
    rename_at(vars(matches("_[0-9]")),
              funs(str_replace(., 
                               str_c("_", i),
                               str_c("_", i - 1))))
  
  # predict based on model in wave previous wave
  p1 <- predict.glm(list_res[[i - 2]], data, type = "response")
  
  # predict based on model in wave previous wave
  p3 <- predict.glm(list_res[[i + 3]], data, type = "response")
  
  new_data[[i - 2]] <- cbind(data, p1, p3)
  
}

# show plots of predicted probabilities
i <- 3
map(new_data, function(x){
  print(str_c("Wave ", i))
  i <<- i + 1 
  
  qplot(p3, p1) %>% print()
})

# bring the probabilities together
new_data2 <- reduce(new_data, left_join, by = "pidp")

# rename
new_data2 <- new_data2 %>% 
  tbl_df() %>% 
  select(matches("^pidp|^p1|^p3")) %>% 
  setNames(c("pidp",
             str_c(
               rep(c("p1", "p3"), 4), "_", 
               rep(3:6, each = 2))))


# being in variables for not being issued and effort in phase 

new_data2 <- usw3 %>% 
  select(pidp, matches("not_issued"), matches("ph23"), matches("hidp")) %>% 
  right_join(new_data2) %>% 
  rename_at(vars(matches("not_issued")),
            ~str_replace(., "not_issued", "notissued")) %>% 
  rename_at(vars(matches("ph23")),
            ~str_replace(., "nrcalls_ph23", "nrcallsph23")) %>%
  rename_at(vars(matches("hidp")),
            ~move_lab(.)) %>% 
  select(pidp, everything(), -matches("_2"))


# set as dataframe
new_data2 <- as.data.frame(new_data2)

# reshape as long
new_data3 <- reshape(data = new_data2,
                     idvar = "pidp",
                     varying = 2:ncol(new_data2),
                     timevar = "wave",
                     sep = "_",
                     direction = "long") %>% 
  tbl_df()


# Make simulation cases ---------------------------------------------------



# select only cases that had effort in phase 2/3 of the wave
new_data4 <- new_data3 %>% 
  filter(notissued == 0, nrcallsph23 > 0, !is.na(p1))



# simulations 1, 2 and 4


# get statistics for cut-offs
quant <- new_data4 %>% 
  dplyr::group_by(wave) %>% 
  dplyr::summarise(p1_75 = quantile(p1, na.rm = T)[4],
                   p3_75 = quantile(p3, na.rm = T)[2],
                   cor =  round(cor(p1, p3, use = "pairwise.complete.obs"), 2))


new_data5 <- new_data4 %>% 
  left_join(quant) %>% 
  ungroup() %>% 
  mutate(wave2 = str_c("Wave ", wave),
         sum_p = p1 + (1 - p3)) %>% 
  dplyr::group_by(wave) %>% 
  dplyr::mutate(sim1 = ifelse(p1 > p1_75, "No", "Yes"),
                sim2 = ifelse(p3 < p3_75, "No", "Yes"),
                sum_out = ifelse(sum_p < quantile(sum_p, na.rm = T)[4],
                                 "Yes", "No"),
                sim4 = sum_out) %>% 
  ungroup()


# simulation 3


sim3_list <- list(NULL)

for (i in 3:6) {
 
 # select only valid data, variables of interest, reverse probs
 test_dat <- new_data5 %>%
    filter(wave == i) %>%
    ungroup() %>%
    na.omit() %>%
   arrange(sample(1:nrow(.), nrow(.))) %>% # randomize order
    mutate(p3_op = 1 - p3,
           sim3 = "Yes",
           rank = row_number()) %>%
   select(pidp, wave, rank, p1, p3_op, sim3)
   

 # starting point for simulation, no missing
 miss_prop <- 0
  
 # loop until 25% was selected
  while (miss_prop < 0.25) {
    
    
    # get maximum for p1
    max_value <- max(test_dat$p1, na.rm = T)
 
    
    # choose the first person based on the max of p1 not to follow
    # and code missing so we don't select them anymore
    test_dat <- test_dat %>%
      group_by(p1) %>%
      mutate(sim3 = ifelse(p1 == max_value &
                                rank == max(rank), "No", sim3)) %>%
      ungroup() %>%
      mutate(
        p1 = ifelse(sim3 == "No", NA, p1),
        p3_op = ifelse(sim3 == "No", NA, p3_op)
      )
    
    
    
    # get maximum for p3_op
    max_value <- max(test_dat$p3_op, na.rm = T)
    
    # choose the first person based on the max of p1 not to follow
    # and code missing so we don't select them anymore
    test_dat <- test_dat %>%
      group_by(p3_op) %>%
      mutate(sim3 = ifelse(p3_op == max_value &
                                rank == max(rank), "No", sim3)) %>%
      ungroup() %>%
      mutate(
        p1 = ifelse(sim3 == "No", NA, p1),
        p3_op = ifelse(sim3 == "No", NA, p3_op)
      )
    
    # calculate new proportion of missing
    # we get different proportions based on each probability
    # is this a problem?
    miss_prop <- test_dat %>% 
      select(p1) %>% 
      summarise_all(~mean(is.na(.))) %>% 
      unlist() %>% sum()
    
    # print to see progress
    print(str_c("wave ", i))
    print(str_c("Miss prop:", round(miss_prop, 3)))
  }
  
 # put results together
  sim3_list[[i]] <- select(test_dat, pidp, wave, sim3)
  
}

# being together all the data
new_data5 <- reduce(sim3_list, rbind) %>% 
  right_join(new_data5, by = c("pidp", "wave")) %>% 
  mutate(sim3 = ifelse(is.na(sim3), "No", sim3)) # code NAs as no followup 



# descriptives of simulations ---------------------------------------------

sim_vars <- str_c("sim", 1:4)

# how many cases do we have

map(sim_vars, function(x)
  new_data5 %>%
    group_by(wave) %>%
    count((!!sym(x))) %>%
    mutate(prop = n / sum(n)))


# how many calls saved make new call variable that takes into account multiple
# individual as same address

new_data5 <- new_data5 %>% 
  group_by(wave, hidp) %>% 
  mutate(hh_size_ph23 = max(row_number())) %>% 
  group_by(wave) %>% 
  mutate(nrcallsph23_hh = nrcallsph23 / hh_size_ph23) %>% 
  ungroup()


get_calls_saved <- function(data, var) {
  {{data}} %>% 
    group_by(wave, (!!sym(var))) %>% 
    summarise(sum_calls = sum(nrcallsph23_hh)) %>% 
    group_by(wave) %>% 
    mutate(prop = sum_calls/sum(sum_calls))
  
}


# save as datafile
nr_calls_saved <- map(sim_vars, get_calls_saved, data = new_data5) %>% 
  map(function(x) setNames(x, c("wave", "selected", "sum_calls", "prop"))) %>% 
  reduce(rbind) %>%
  ungroup() %>% 
  mutate(sim = rep(str_c("sim", 1:4), each = 8))

nr_calls_saved %>% print(n = 100)

# save as csv
write_csv(nr_calls_saved, "./results/call_save_ind.csv")



# graph

new_data5 %>%
  filter(!is.na(sim3)) %>%
  mutate(wave2 = str_c("Wave ", wave)) %>% 
  ggplot(aes(p3, p1,
             color = as.factor(sim4), alpha = as.factor(sim3))) +
  geom_point() +
  geom_hline(aes(yintercept = p1_75)) +
  geom_vline(aes(xintercept = p3_75)) +
  facet_wrap(~ wave2) +
  theme_bw() +
  labs(y = "P(R initial)",
       x = "P(R followup | NR initial)",
       color = "In Simulation 4",
       alpha = "In Simulation 3")


 ggsave("./results/prob_plots_sim3.png", dpi = 300)

 
 

# Calculate simualtion groups at household level --------------------------



new_data5 <- new_data5 %>% 
  group_by(wave, hidp) %>% 
  mutate_at(vars(p1, p3),
            funs(hh = mean(.))) %>% 
  ungroup() %>% 
  arrange(wave, hidp, p1, p3)


 
new_data6 <- new_data5 %>% 
  mutate(sum_p_hh = p1_hh + (1 - p3_hh)) %>% 
  group_by(wave) %>%
  arrange(p1_hh) %>% 
  mutate(prop = row_number()/max(row_number()),
         sim1_hh = ifelse(prop > 0.75, "No", "Yes")) %>% 
  arrange(p3_hh) %>% 
  mutate(prop = row_number()/max(row_number()),
         sim2_hh = ifelse(prop < 0.25, "No", "Yes")) %>% 
  arrange(sum_p_hh) %>% 
  mutate(prop = row_number()/max(row_number()),
         sim4_hh = ifelse(prop < 0.75, "Yes", "No")) %>%
  select(-prop) %>% 
  ungroup() 
  


# simulation 3 for hh


sim3_list_hh <- list(NULL)

for (i in 3:6) {
  
  # select only valid data, variables of interest, reverse probs
  test_dat <- new_data6 %>%
    filter(wave == i) %>%
    ungroup() %>%
    na.omit() %>%
    arrange(sample(1:nrow(.), nrow(.))) %>% # randomize order
    mutate(p3_op_hh = 1 - p3_hh,
           sim3_hh = "Yes",
           rank = row_number()) %>%
    select(pidp, wave, rank, p1_hh, p3_op_hh, sim3_hh)
  
  
  # starting point for simulation, no missing
  miss_prop <- 0
  
  # loop until 25% was selected
  while (miss_prop < 0.25) {
    
    
    # get maximum for p1
    max_value <- max(test_dat$p1_hh, na.rm = T)
    
    
    # choose the first person based on the max of p1 not to follow
    # and code missing so we don't select them anymore
    test_dat <- test_dat %>%
      group_by(p1_hh) %>%
      mutate(sim3_hh = ifelse(p1_hh == max_value &
                             rank == max(rank), "No", sim3_hh)) %>%
      ungroup() %>%
      mutate(
        p1_hh = ifelse(sim3_hh == "No", NA, p1_hh),
        p3_op_hh = ifelse(sim3_hh == "No", NA, p3_op_hh)
      )
    
    
    
    # get maximum for p3_op
    max_value <- max(test_dat$p3_op_hh, na.rm = T)
    
    # choose the first person based on the max of p1 not to follow
    # and code missing so we don't select them anymore
    test_dat <- test_dat %>%
      group_by(p3_op_hh) %>%
      mutate(sim3_hh = ifelse(p3_op_hh == max_value &
                             rank == max(rank), "No", sim3_hh)) %>%
      ungroup() %>%
      mutate(
        p1_hh = ifelse(sim3_hh == "No", NA, p1_hh),
        p3_op_hh = ifelse(sim3_hh == "No", NA, p3_op_hh)
      )
    
    # calculate new proportion of missing
    # we get different proportions based on each probability
    # is this a problem?
    miss_prop <- test_dat %>% 
      select(p1_hh) %>% 
      summarise_all(~mean(is.na(.))) %>% 
      unlist() %>% sum()
    
    # print to see progress
    print(str_c("wave ", i))
    print(str_c("Miss prop:", round(miss_prop, 3)))
  }
  
  # put results together
  sim3_list_hh[[i]] <- select(test_dat, pidp, wave, sim3_hh)
  
}

# being together all the data
new_data7 <- reduce(sim3_list_hh, rbind) %>% 
  right_join(new_data6, by = c("pidp", "wave")) %>% 
  mutate(sim3_hh = ifelse(is.na(sim3_hh), "No", sim3_hh))






# how many cases do we have
sim_vars_hh <- str_c("sim", 1:4, "_hh")

map(sim_vars_hh, function(x)
  new_data7 %>%
    group_by(wave) %>%
    count((!!sym(x))) %>%
    mutate(prop = n / sum(n)))


# smaller proportion of sim1 not chosen. I think it's because equat values

p1_75_hh <- new_data7 %>% 
  group_by(wave) %>% 
  filter(sim1_hh == "Yes") %>% 
  summarise(p1_75_hh = max(p1_hh))

p3_75_hh <- new_data7 %>% 
  group_by(wave) %>% 
  filter(sim2_hh == "No") %>% 
  summarise(p3_75_hh = max(p3_hh))


new_data7 %>%
  mutate(wave2 = str_c("Wave ", wave)) %>% 
  left_join(p1_75_hh, by = "wave") %>% 
  left_join(p3_75_hh, by = "wave") %>% 
  ggplot(aes(p3_hh, p1_hh,
             color = as.factor(sim4_hh), alpha = as.factor(sim3_hh))) +
  geom_point() +
  facet_wrap(~ wave2) +
  geom_hline(aes(yintercept = p1_75_hh)) +
  geom_vline(aes(xintercept = p3_75_hh)) +
  theme_bw() +
  labs(y = "P(R initial)",
       x = "P(R followup | NR initial)",
       color = "In Simulation 4",
       alpha = "In Simulation 3")





# save as datafile
nr_calls_saved_hh <- map(sim_vars_hh, get_calls_saved, data = new_data7) %>% 
  map(function(x) setNames(x, c("wave", "selected", "sum_calls", "prop"))) %>% 
  reduce(rbind) %>%
  ungroup() %>% 
  mutate(sim = rep(str_c("sim", 1:4, "_hh"), each = 8))

nr_calls_saved_hh %>% print(n = 100)

# save as csv
write_csv(nr_calls_saved_hh, "./results/call_save_hh.csv")


# graph with number of 

savings_data <- rbind(mutate(nr_calls_saved, level = "Individual"),
      mutate(nr_calls_saved_hh, level = "Household")) %>% 
  filter(selected == "No") %>% 
  mutate(sim = str_remove(sim , "_hh"),
         sim = str_replace(sim, "sim", "Simulation "),
         wave2 = str_c("Wave ", wave))

savings_data %>% 
  group_by(sim, level) %>% 
  summarise(prop = mean(prop)) %>% 
  ggplot(aes(sim, prop, fill = level)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(0.05, 0.25, 0.05)) +
  geom_hline(yintercept = seq(0.05, 0.25, 0.05), color = "white") + 
  theme_bw() +
  labs(x = "Simulation",
       y = "Proportion of calls saved",
       fill = "Level")

savings_data <- savings_data %>% 
  mutate(level = as.factor(level) %>% fct_rev(),
         sim = as.factor(sim) %>% fct_rev(),
         wave2 = as.factor(wave2),
         perc = prop * 100) 

savings_data %>% 
  ggplot(aes(sim, perc, fill = wave2)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(5, 25, 5)) +
  facet_wrap(~level) + 
  geom_hline(yintercept = seq(5, 25, 5), color = "white") + 
  theme_bw() +
  labs(x = "Simulation",
       y = "Percentage of calls saved",
       fill = "Level") +
  coord_flip()

ggsave("./results/savings.png", dpi = 500)
write_csv(savings_data, "./results/savings.csv")  


# calculate response rates for different scenarios ------------------------

# get outcome variables in long format

data_out <- usw3 %>% 
  select(pidp, matches("^out_|out2_")) %>% 
  rename_all(funs(str_replace(., "_p", ".p")))

data_out <- as.data.frame(data_out)


data_outl <- reshape(data = data_out,
                     idvar = "pidp",
                     varying = 2:length(data_out),
                     timevar = "wave",
                     sep = "_",
                     direction = "long") %>% 
  tbl_df()



new_data8 <- new_data7 %>%  
  dplyr::select(pidp, wave, matches("sim"), matches("p"))

# bring together outcome and simulation
out_data <- data_outl %>% 
  # filter(wave > 2) %>% 
  left_join(new_data8, by = c("pidp", "wave"))


# code outcome as non_response if we didn't followup

count(out_data, wave, out, out2, sim1_hh)



out_data <- out_data %>% 
  mutate(out2.sim1 = case_when(sim1 == "No" & out2 == 1 ~ 0,
                             TRUE ~ out2),
         out2.sim2 = case_when(sim2 == "No" & out2 == 1 ~ 0,
                               TRUE ~ out2),
         out2.sim3 = case_when(sim3 == "No" & out2 == 1 ~ 0,
                               TRUE ~ out2),
         out2.sim4 = case_when(sim4 == "No" & out2 == 1 ~ 0,
                               TRUE ~ out2),
         out2.sim1_hh = case_when(sim1_hh == "No" & out2 == 1 ~ 0,
                               TRUE ~ out2),
         out2.sim2_hh = case_when(sim2_hh == "No" & out2 == 1 ~ 0,
                               TRUE ~ out2),
         out2.sim3_hh = case_when(sim3_hh == "No" & out2 == 1 ~ 0,
                               TRUE ~ out2),
         out2.sim4_hh = case_when(sim4_hh == "No" & out2 == 1 ~ 0,
                               TRUE ~ out2))

out_data %>% 
  count(out2, out2.sim1_hh, sim1_hh)


rr <- out_data %>%
  group_by(wave) %>% 
  select(out2, matches("out2.s")) %>% 
  summarise_all(~mean(., na.rm = T))
  
rr2 <- rr %>%
  gather(-wave, value = value, key = outcome) %>% 
  mutate(outcome2 = fct_recode(outcome,
                               "No change" = "out2",
                               "Sim 1" = "out2.sim1",
                               "Sim 2" = "out2.sim2",
                               "Sim 3" = "out2.sim3",
                               "Sim 4" = "out2.sim4",
                               "Sim 1" = "out2.sim1_hh",
                               "Sim 2" = "out2.sim2_hh",
                               "Sim 3" = "out2.sim3_hh",
                               "Sim 4" = "out2.sim4_hh"),
         level = ifelse(str_detect(outcome, "hh"), "Household", "Individual")) 


w1_info <- rr2 %>%
  filter(wave == 2) %>% 
  mutate(wave = 1,
         value = 1)


rr2 %>%
  rbind(w1_info) %>% 
  ggplot(aes(wave, value, color = outcome2, 
             shape = level, linetype = level)) + 
  geom_point() + geom_line(aes(group = outcome)) +
  scale_x_continuous(breaks = 1:6) +
  theme_bw() +
  labs(x = "Wave", y = "Response rate", color = "Simulation type",
       shape = "Level", linetype = "Level")

write.csv(rr2, "./results/resp_rate.csv")
ggsave("./results/rr_selection_plots.png", dpi = 500)




# make rr balanced and delete any future  participation

out_data_balanced <- out_data %>%
  select(pidp, wave, matches("out2"), -matches("\\.p")) %>% 
  arrange(pidp, wave) %>% 
  group_by(pidp) %>% 
  mutate_at(vars(matches("out")),
            ~cummin(.))


rr <- out_data_balanced %>%
  group_by(wave) %>% 
  select(out2, matches("out2.s")) %>% 
  summarise_all(~mean(., na.rm = T))

rr2 <- rr %>%
  gather(-wave, value = value, key = outcome) %>% 
  mutate(outcome2 = fct_recode(outcome,
                               "No change" = "out2",
                               "Sim 1" = "out2.sim1",
                               "Sim 2" = "out2.sim2",
                               "Sim 3" = "out2.sim3",
                               "Sim 4" = "out2.sim4",
                               "Sim 1" = "out2.sim1_hh",
                               "Sim 2" = "out2.sim2_hh",
                               "Sim 3" = "out2.sim3_hh",
                               "Sim 4" = "out2.sim4_hh"),
         level = ifelse(str_detect(outcome, "hh"), "Household", "Individual")) 


w1_info <- rr2 %>%
  filter(wave == 2) %>% 
  mutate(wave = 1,
         value = 1)



rr2 <- rr2 %>%
  rbind(w1_info) 

rr2 %>% 
  ggplot(aes(wave, value, color = outcome2, 
             shape = level, linetype = level)) + 
  geom_point() + geom_line(aes(group = outcome)) +
  scale_x_continuous(breaks = 1:6) +
  theme_bw() +
  labs(x = "Wave", y = "Balanced response rates", color = "Simulation type",
       shape = "Level", linetype = "Level")

write.csv(rr2, "./results/resp_rate_balanced.csv")


ggsave("./results/rr_selection_plots_balaced.png")







# Export data -------------------------------------------------------------


new_out_data <- out_data %>% select(-out, -out2) %>% as.data.frame()

out_wide <- reshape(data = new_out_data,
                    idvar = "pidp",
                    timevar = "wave",
                    sep = "_",
                    direction = "wide") %>% 
  tbl_df()

usw4 <- left_join(usw3, out_wide)


save(usw4, file = "./data/usw4.RData")



