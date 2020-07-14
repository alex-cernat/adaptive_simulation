



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

source("./functions/RISQ-R-indicators-v21.R")


load("./data/usw4.RData")



# prep --------------------------------------------------------------------





# make outcomes
type_out <- c("out2", "out2_p1", 
              "out2.sim1", "out2.sim2", "out2.sim3", "out2.sim4",
              "out2.sim1_hh", "out2.sim2_hh", "out2.sim3_hh", "out2.sim4_hh")

outcomes <- str_c(rep(type_out, 4), "_", rep(3:6, each = length(type_out)))


# indepndent variables
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


# run only once...takes around 3 days
# 
# ran <- list.files("C:/rinds/") %>% str_remove("\\.RData")
# 
# outs_remain <- outcomes[!outcomes %in% ran]
#   
# map(outs_remain, function(x){
#   out <- rind_run(x)
#   save(out, file = str_c("C:/rinds/", x, ".RData"))
# })  







# get r indicators

rinds_files <- list.files("./data/rinds/", full.names = T)

rinds_list <- list(NULL)
for (i in seq_along(rinds_files)) {
  load(rinds_files[i])
  rinds_list[[i]] <- out
}


names(rinds_list) <- str_remove_all(rinds_files, 
                                    "\\./data/rinds/|out2\\.|\\.RData")




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

ggsave("./results/ukhls_rindicators_overall.png", dpi = 500, width = 7)

write_csv(rind2, "./results/us_rinds.csv")
