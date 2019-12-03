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

# get vars of interest
usw_small <- usw %>% 
  dplyr::select(agecat:education_fct, 
                starts_with("out_"),
                starts_with("int_phase"),
                starts_with("status_p1"),
                starts_with("days_int")) %>% 
  dplyr::select(-employed, -relationship,
                -benefit_prop, -countryofbirth,
                -long_cond, -work_limit)

# change ineligibles not non-respondents
usw_small <- usw_small %>%
  mutate_at(vars(matches("out_")),
            funs(ifelse(. == 3, 0, .)))

vars <- c("adultsinhh", "agecat", "benefit", "childreninhh",
          "countryofbirth_fct", "education_fct", "employed_fct",
          "ineducation", "health_sum", "likelymove",
          "owner", "relationship_fct", "female")

phase <- rep(c("out_p1"), 5)
waves <- rep(c(2:6))

outcomes <- str_c(phase, "_", waves)

models_p1 <- list(NULL)

for (i in seq_along(outcomes)) {
  
  print(outcomes[i])
  
  response_model <- formula(str_c(outcomes[i], " ~ ",
                                  str_c(vars, collapse = " + ")))
  
  data <- usw_small %>%
    dplyr::select(vars, outcomes[i]) %>%
    na.omit()
  
  models_p1[[i]] <- glm(response_model, data = data,
                        family = "binomial")
  
  names(models_p1)[i] <- outcomes[i]
}



phase <- rep(c("out_p3"), 5)
waves <- rep(c(2:6))

outcomes <- str_c(phase, "_", waves)


usw_small %>% 
  dplyr::count(is.na(status_p1_2), is.na(days_int_2), out_p3_2)

models_p3 <- list(NULL)

for (i in seq_along(outcomes)) {
  
  print(outcomes[i])
  
  vars2 <- c(vars, 
             str_c(c("status_p1_", "days_int_"), waves[i]))
  
  response_model <- formula(str_c(outcomes[i], " ~ ",
                                  str_c(vars2, collapse = " + ")))
  
  data <- usw_small %>%
    dplyr::select(vars2, outcomes[i]) %>%
    na.omit()
  
  models_p3[[i]] <- glm(response_model, data = data,
                        family = "binomial")
  
  names(models_p3)[i] <- outcomes[i]
}




list_res <- c(models_p1, models_p3)

results <- map(list_res, broom::glance) %>%
  reduce(rbind)

results$model <- map(map(list_res, names), names) %>% 
  names()


results <- results %>% 
  tbl_df() %>% 
  separate(model, into = c("out", "outcome", "wave"), sep = "_") %>% 
  select(-out) %>% 
  mutate(r2 = 1 - (deviance/null.deviance),
         n = unlist(
           map(c(models_p1, models_p3), 
               function(x) length(resid(x)))))


results %>% 
  ggplot(aes(wave, r2, colour = outcome, group = outcome)) + 
  geom_point() + geom_line() +
  theme_bw() +
  labs(title = "R2 for models for two outcomes by wave")

ggsave("./results/reg_r2.png", dpi = 500)






easy_roc <- function(object) {
  x <- ModelGood::Roc(object,
                      formula = object$formula,
                      data = object$data)
  
  unlist(x$Auc)
}


results$auc <- cbind(unlist(map(c(models_p1, models_p3), easy_roc)))

results %>% 
  ggplot(aes(wave, auc, colour = outcome, group = outcome)) + 
  geom_point() + geom_line() +
  theme_bw() +
  labs(title = "AUC for models for two outcomes by wave")

ggsave("./results/reg_auc.png", dpi = 500)





# missing vars
wide_us <- wide_us %>% 
  mutate(countryofbirth_fct_3 = countryofbirth_fct,
         countryofbirth_fct_4 = countryofbirth_fct,
         countryofbirth_fct_5 = countryofbirth_fct,
         countryofbirth_fct_6 = countryofbirth_fct)


# give same lables to education
wide_us <- wide_us %>% 
  mutate(education_fct_1 = fct_recode(education_fct_1,
                                      "Missing" = "refused",
                                      "Missing" = "Don't know",
                                      "Other higher degree" = "Other higher",
                                      "Other qualification" = "Other qual",
                                      "No qualification" = "No qual"))



# add process variables
wide_us <- usw %>% 
  select(pidp, matches("status_p1_|days_int_")) %>% 
  left_join(wide_us)



new_data <- list(NULL)

for (i in 3:6) {
  
  sel_vars <- str_c(
    c(vars[1:13], "status_p1", "days_int"), 
    "_",i)
  
  data <- wide_us %>% 
    select(sel_vars, pidp) %>% 
    rename_all(funs(str_remove(., "_[0-9]$"))) %>% 
    rename_at(vars(matches("status_p1|days_int")),
              funs(str_c(., "_", i - 1)))
  
  p1 <- predict.glm(list_res[[i - 2]], data, type = "response")
  p3 <- predict.glm(list_res[[i + 3]], data, type = "response")
  
  new_data[[i - 2]] <- cbind(data, p1, p3)
  
}

i <- 3
map(new_data, function(x){
  print(str_c("Wave ", i))
  i <<- i + 1 
  
  qplot(p3, p1)
})

new_data2 <- reduce(new_data, left_join, by = "pidp")

new_data2 <- new_data2 %>% 
  tbl_df() %>% 
  select(matches("^pidp|^p1|^p3")) %>% 
  setNames(c("pidp",
             str_c(
               rep(c("p1", "p3"), 4), "_", 
               rep(3:6, each = 2))))


new_data2 <- as.data.frame(new_data2)

new_data3 <- reshape(data = new_data2,
                     idvar = "pidp",
                     varying = 2:9,
                     timevar = "wave",
                     sep = "_",
                     direction = "long") %>% 
  tbl_df()



quant <- new_data3 %>% 
  dplyr::group_by(wave) %>% 
  dplyr::summarise(p1_75 = quantile(p1, na.rm = T)[4],
                   p3_75 = quantile(p3, na.rm = T)[2],
                   cor =  round(cor(p1, p3, use = "pairwise.complete.obs"), 2))


new_data4 <- new_data3 %>% 
  left_join(quant) %>% 
  ungroup() %>% 
  mutate(wave2 = str_c("Wave ", wave),
         sum_p = p1 + (1 - p3)) %>% 
  dplyr::group_by(wave) %>% 
  dplyr::mutate(sum_out = ifelse(sum_p < quantile(sum_p, na.rm = T)[4],
                                 "Yes", "No"))


new_data4 %>% 
  ungroup() %>% 
  group_by(wave2) %>%
  dplyr::count(sum_out)

new_data4 %>% 
  select(p3, p1, sum_out, wave2, 
         cor, p1_75, p3_75) %>% 
  na.omit() %>% 
  ggplot(aes(p3, p1)) +
  geom_point(aes(color = sum_out), alpha = 0.03) +
  geom_vline(aes(xintercept = p3_75), color = "red", alpha = .8) +
  geom_hline(aes(yintercept = p1_75), color = "red", alpha = .8) +
  facet_wrap(~ wave2) +
  labs(x = "Probability of answering in phase 3",
       y = "Probability of answering in phase 1",
       color = "Sum selection 75%",
       title = "Out of sample pred. of answer prop. and three selection methods") +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_text(aes(label = paste("r=", cor, sep = "")), 
            x = 0.1, y = 0.15) 

ggsave("./results/prob_plots.png", dpi = 500)








# calcualte response rates for different scenarios ------------------------

# get outcome variables in long format

data_out <- usw %>% 
  select(pidp, matches("^out_[0-6]$|^out_p1_[0-6]$")) %>% 
  rename_all(funs(str_replace(., "out_p1", "out.p1")))

data_out <- as.data.frame(data_out)

data_outl <- reshape(data = data_out,
                     idvar = "pidp",
                     varying = 2:11,
                     timevar = "wave",
                     sep = "_",
                     direction = "long") %>% 
  tbl_df()

new_data5 <- new_data4 %>% 
  mutate(sel_p1 = ifelse(p1 < p1_75, 1, 0),
         sel_p3 = ifelse(p3 > p3_75, 1, 0),
         sel_sum = ifelse(sum_out == "Yes", 1, 0)) %>% 
  dplyr::select(pidp, wave, sel_p1, sel_p3, sel_sum)

out_data <- data_outl %>% 
  filter(wave > 2) %>% 
  left_join(new_data5, by = c("pidp", "wave"))

out_data <- out_data %>% 
  mutate(out.sp1 = case_when(sel_p1 == 1 & out == 1 ~ 1,
                             TRUE ~ out.p1),
         out.sp3 = case_when(sel_p3 == 1 & out == 1 ~ 1,
                             TRUE ~ out.p1),
         out.sp13 = case_when(sel_sum == 1 & out == 1 ~ 1,
                              TRUE ~ out.p1))


out_data %>% 
  dplyr::count(sel_sum, sel_p3)

rr <- out_data %>%
  group_by(wave) %>% 
  dplyr::summarise(prop_out = mean(out, na.rm = T),
                   prop_out_p1 = mean(out.p1, na.rm = T),
                   prop_out_sp1 = mean(out.sp1, na.rm = T),
                   prop_out_sp3 = mean(out.sp3, na.rm = T),
                   prop_out_sp13 = mean(out.sp13, na.rm = T))

rr <- rr %>% 
  gather(prop_out, prop_out_p1, prop_out_sp1, prop_out_sp3, prop_out_sp13,
         value = value, key = outcome) %>% 
  mutate(outcome2 = fct_recode(outcome,
                               "Final response" = "prop_out",
                               "Response phase 1" = "prop_out_p1",
                               "Phase 1 bottom 75%" = "prop_out_sp1",
                               "Phase 3 top 75%" = "prop_out_sp3",
                               "Phase 1+3 propensity" = "prop_out_sp13")) 

rr %>% 
  ggplot(aes(wave, value, color = outcome2)) + 
  geom_point() + geom_line(aes(group = outcome)) +
  theme_bw() +
  labs(x = "Wave", y = "Response rate", color = "Group",
       title = "Response rates at different stages and different selections")

write.csv(rr, "./results/conference/resp_rate.csv")
ggsave("./results/rr_selection_plots.png", dpi = 500)








# Export data -------------------------------------------------------------




new_out_data <- out_data %>% 
  select(pidp, wave, out.sp1, out.sp3, out.sp13)

new_out_data <- as.data.frame(new_out_data)

out_wide <- reshape(data = new_out_data,
                    idvar = "pidp",
                    timevar = "wave",
                    sep = "_",
                    direction = "wide") %>% 
  tbl_df()

usw <- left_join(usw, out_wide)

save(usw, file = "./data/usw3.RData")



