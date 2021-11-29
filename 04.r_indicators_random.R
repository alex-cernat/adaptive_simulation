################
# 
# R indicators for random 25% households for JSSAM R&R
# 
################




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

source("./functions/RISQ-R-indicators-v21.R")


load("./data/usw4.RData")



# prep --------------------------------------------------------------------





# make outcomes
type_out <- c("out2.simr_hh")

outcomes <- str_c(rep(type_out, 4), "_", rep(3:6, each = length(type_out)))


# independent variables
vars <- c("adultsinhh", "agecat", "benefit", "childreninhh",
          "countryofbirth_fct", "education_fct", "employed_fct",
          "ineducation", "health_sum", "likelymove",
          "owner", "relationship_fct", "female", "urb")


# get data
usw_small <- usw4 %>% 
  select(pidp, outcomes, vars) 



# get R indicators for all the outcomes -----------------------------------

rind_run <- function(outcome) {
  response_model <- formula(str_c(outcome, " ~ ",
                                  str_c(vars, collapse = " + ")))
  
  data <- usw_small %>%
    select(vars, outcome) %>%
    na.omit()
  
  indicator <- getRIndicator(response_model, data,
                             withPartials = T)
  
  indicator
}


# run only once
# 
ran <- list.files("./data/rinds/random/") %>% str_remove("\\.RData")

outs_remain <- outcomes[!outcomes %in% ran]

map(outs_remain, function(x){
  out <- rind_run(x)
  save(out, file = str_c("./data/rinds/random/", x, ".RData"))
})







# get r indicators

rinds_files <- list.files("./data/rinds/random/", full.names = T)

rinds_list <- list(NULL)
for (i in seq_along(rinds_files)) {
  load(rinds_files[i])
  rinds_list[[i]] <- out
}


names(rinds_list) <- str_remove_all(rinds_files, 
                                    "\\./data/rinds/random/|out2\\.|\\.RData")




# do graphs ---------------------------------------------------------------




rindicators <- map_df(rinds_list, function(x) rind_result(x)[[1]]) %>% 
  mutate(nms = rep(names(rinds_list), each = 2))

rind <- rindicators %>% 
  mutate(level = case_when(str_detect(nms, "hh") ~ "Household", 
                           str_detect(nms, "out2_p1_") ~ "Observed p1",
                           str_detect(nms, "out") ~ "Observed",
                           TRUE ~ "Individual"),
         nms2 = str_remove_all(nms, "_hh|_p1")) %>% 
  separate(nms2, into = c("Simulation", "Wave")) %>%
  mutate(ValueSE = as.numeric(ValueSE),
         lci = Value - (1.96 * ValueSE),
         uci = Value + (1.96 * ValueSE),
         Simulation = str_replace(Simulation, "sim", "Simulation ")) 

rind_out <- filter(rind, str_detect(nms, "out")) %>% 
  mutate(Simulation = ifelse(str_detect(nms, "out2_p1"), 
                             "P1 data",
                             "Full data"))

rind2 <- rbind(
  mutate(rind_out, level = "Individual"),
  mutate(rind_out, level = "Household"),
  filter(rind, str_detect(nms, "sim"))
)

rind2 %>% 
  filter(Indicator == "R") %>%
  mutate(level = as.factor(level)) %>% 
  ggplot(aes(Wave, Value, 
             color = Simulation, 
             group = Simulation)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                width = 0, alpha = 0.5) +
  facet_wrap(~ fct_rev(level)) +
  theme_bw() +
  labs(y = "R indicator")

# ggsave("./results/ukhls_rindicators_overall.png", dpi = 500, width = 7)

write_csv(rind2, "./results/us_rinds_random.csv")




# (R-indicators, response rate, and attempts saved


out_data <- select(usw_small, pidp, wave, matches("out2")) 

rr <- out_data %>%
  group_by(wave) %>% 
  select(out2, matches("out2.s")) %>% 
  summarise_all(~mean(., na.rm = T))

rr2 <- rr %>%
  gather(-wave, value = value, key = outcome) %>% 
  filter(outcome != "out2.simr_hh") %>% 
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