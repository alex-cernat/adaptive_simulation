
# graphs for paper --------------------------------------------------------

# switch simulaiton 1 and 2 
# - redo rindicatord and rr graphs
# - hilda is twich in labels
# - make % for rr
# - hilda 12 to 16
# - response rates two versions with same scale and different
# - redo calls


# 0. Set-up ---------------------------------------------------------------


# clean memory
rm(list = ls())
gc()

# use local packages on work machine
# if (Sys.getenv("USERNAME") == "msassac6") {.libPaths(c(
#   paste0(
#     "C:/Users/",
#     Sys.getenv("USERNAME"),
#     "/Dropbox (The University of Manchester)/R/package"
#   ),
#   .libPaths()
# ))}

# load packages
pkg <- c("tidyverse", "haven", "lubridate",
         "devtools", "scales")

sapply(pkg, library, character.only = T)


# load functions
list.files("./functions/",
           pattern = "\\.R$",
           full.names = T) %>% 
  map(source)



# hilda data

hilda_rinds <- read_csv("./data/hilda_rinds.csv")
hilda_rr <- read_csv("./data/hilda_rr.csv")
hilda_costs <- read_csv("./data/hilda_costs_rvsd.csv")
hilda_costs_avg <- read_csv("./data/hilda_costs_ave.csv")


# replace sim2 with sim 1 for hilda

hilda_rinds <- hilda_rinds %>% 
  mutate(sim = case_when(sim == "sim1" ~ "sim2",
                          sim == "sim2" ~ "sim1",
                          TRUE ~ sim)) 


hilda_rr <- hilda_rr %>% 
  mutate(sim = case_when(sim == "sim1" ~ "sim2",
                         sim == "sim2" ~ "sim1",
                         TRUE ~ sim)) 

hilda_costs <- hilda_costs %>% 
  mutate(sim = case_when(sim == "sim1" ~ "sim2",
                         sim == "sim2" ~ "sim1",
                         TRUE ~ sim)) 

hilda_costs_avg <- hilda_costs_avg %>% 
  mutate(sim = case_when(sim == "sim1" ~ "sim2",
                         sim == "sim2" ~ "sim1",
                         TRUE ~ sim)) 



# ukhls data
rr_ukhls <- read_csv("./results/resp_rate.csv")


us_rr_balanced <- read_csv("./results/resp_rate_balanced.csv")
us_rr <- read_csv("./results/resp_rate.csv")
us_save <- read_csv("./results/savings.csv")  
us_rinds <- read_csv("./results/us_rinds.csv")

# rr graphs --------------------------------------------------------------------

us_rr <- us_rr %>% 
  rename(sim = outcome2, est = value) %>% 
  mutate(sample = "UKHLS",
         type = "Regular") %>% 
  select(wave, sim, est, level, sample, type, -X1, -outcome) 

us_rr_balanced <- us_rr_balanced %>% 
  rename(sim = outcome2, est = value) %>% 
  mutate(sample = "UKHLS",
         type = "Balanced") %>% 
  select(wave, sim, est, level, sample, type, -X1, -outcome) 

hilda_rr <- hilda_rr %>% 
  mutate(sample = str_c("HILDA: ", sample))

rr <- rbind(hilda_rr, us_rr, us_rr_balanced) %>% 
  mutate(sim = str_c("Simulation ", str_extract(sim, "[0-9]+")),
         sim = ifelse(is.na(sim), "No change", sim),
         level = as.factor(level) %>% fct_rev(),
         wave2 = as.character(wave) %>% str_extract("[0-9]$"),
         wave2 = str_c("Wave (1)", wave2),
         est2 = est * 100,
         grp = str_c(sim, level)) %>% 
  filter(wave != 11)


rr %>%
  filter(type == "Regular") %>% 
  filter(!(sim == "No change" & level == "Household")) %>% 
  ggplot(aes(wave2, est, color = sim, group = grp,
             shape = level, linetype = level)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ sample, nrow = 3) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Wave", y = "Response rate", color = "Simulation type",
       shape = "Level", linetype = "Level",
       caption = "Please note change of scales")

ggsave("./results/rr_plot_samescale.png", dpi = 500, height = 7)


rr %>%
  filter(type == "Regular") %>% 
  filter(!(sim == "No change" & level == "Household")) %>% 
  ggplot(aes(wave2, est, color = sim, group = grp,
             shape = level, linetype = level)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ sample, nrow = 3, scales = "free_y") +
  theme_bw() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  labs(x = "Wave", y = "Response rate", color = "Simulation type",
       shape = "Level", linetype = "Level",
       caption = "Please note change of scales")

ggsave("./results/rr_plot_difscale.png", dpi = 500, height = 7)



rr %>%
  filter(type == "Balanced") %>% 
  filter(!(sim == "No change" & level == "Household")) %>%
  filter(wave2 != "Wave (1)1") %>% 
  ggplot(aes(wave2, est, color = sim, group = grp,
             shape = level, linetype = level)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ sample, scales = "free_y", nrow = 3) +
  theme_bw() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  labs(x = "Wave", y = "Response rate", color = "Simulation type",
       shape = "Level", linetype = "Level",
       caption = "Please note change of scales")

ggsave("./results/rr_plot_balanced_difscale.png", dpi = 500, height = 7)


rr %>%
  filter(type == "Balanced") %>% 
  filter(!(sim == "No change" & level == "Household")) %>%
  filter(wave2 != "Wave (1)1") %>%
  ggplot(aes(wave2, est, color = sim, group = grp,
             shape = level, linetype = level)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ sample, nrow = 3) +
  theme_bw() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  labs(x = "Wave", y = "Response rate", color = "Simulation type",
       shape = "Level", linetype = "Level",
       caption = "Please note change of scales")

ggsave("./results/rr_plot_balanced_samescale.png", dpi = 500, height = 7)




# rinds graphs ------------------------------------------------------------


us_rinds <- us_rinds %>% 
  filter(Indicator == "R") %>% 
  rename(wave = Wave, sim = Simulation, est = Value) %>% 
  mutate(sample = "UKHLS",
         lci = est - 1.96 * ValueSE,
         uci = est + 1.96 * ValueSE) %>% 
  select(wave, sim, est, lci, uci, level, sample)

hilda_rinds2 <- hilda_rinds %>% 
  mutate(sample = str_c("HILDA: ", sample),
         sim = str_replace(sim, "sim", "Simulation "))

rinds <- rbind(hilda_rinds2, us_rinds) %>% 
  mutate(level = as.factor(level) %>% fct_rev(),
         wave2 = as.character(wave) %>% str_extract("[0-9]$"),
         wave2 = str_c("Wave (1)", wave2))


rinds %>% 
  filter(wave2 != "Wave (1)2") %>% 
  ggplot(aes(as.factor(wave2), est, 
             color = sim, 
             group = sim)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                width = 0, alpha = 0.5) +
  facet_grid(sample ~ level) +
  theme_bw() +
  labs(y = "R indicator", x = "Wave", color = "Simulation")

ggsave("./results/rindicators_samescale.png", dpi = 500, height = 8, width = 8)


rinds %>% 
  filter(wave2 != "Wave (1)2") %>% 
  ggplot(aes(as.factor(wave2), est, 
             color = sim, 
             group = sim)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                width = 0, alpha = 0.5) +
  facet_wrap(sample ~ level, scales = "free_y", ncol = 2) +
  theme_bw() +
  labs(y = "R indicator", x = "Wave", color = "Simulation",
       caption = "Please note change of scales")

ggsave("./results/rindicators_difscale.png", dpi = 500, height = 8, width = 8)




# saving data -------------------------------------------------------------



hilda_costs <- hilda_costs %>% 
  mutate(sim = str_replace(sim, "sim", "Simulation "),
         sample = str_c("HILDA: ", sample),
         est = est * 100)

us_save <- us_save %>% 
  rename(est = perc) %>% 
  mutate(sample = "UKHLS",
         mode = "F2F") %>% 
  select(wave, sim, est, level, sample, mode)


costs <- rbind(hilda_costs, us_save) %>% 
  mutate(wave2 = as.character(wave) %>% str_extract("[0-9]$"),
         wave2 = str_c("Wave (1)", wave2),
         sim = as.factor(sim) %>% fct_rev(),
         mode = as.factor(mode) %>% fct_rev())

costs %>% 
  na.omit() %>% 
  filter(level == "Individual") %>% 
  ggplot(aes(est, sim, fill = mode)) +
    geom_bar(stat = "identity") +
  facet_grid(sample~wave2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 25, by = 5)) +
  geom_vline(xintercept = seq(0, 25, by = 5), color = "white") +
  labs(fill = "Mode", y = "", x = "Percentage of calls saved")

ggsave("./results/saving_individual.png", dpi = 500, width = 7)

costs %>% 
  na.omit() %>% 
  filter(level == "Household") %>% 
  ggplot(aes(est, sim, fill = mode)) +
  geom_bar(stat = "identity") +
  facet_grid(sample~wave2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 25, by = 5)) +
  geom_vline(xintercept = seq(0, 25, by = 5), color = "white") +
  labs(fill = "Mode", y = "", x = "Percentage of calls saved")

ggsave("./results/saving_household.png", dpi = 500, width = 7)






hilda_costs_avg %>% 
  filter(sample == "Top-up") %>% 
  mutate(sim2 = str_replace(sim, "sim", "Simulation "),
         sim2 = as.factor(sim2) %>% fct_rev(),
         wave2 = as.factor(str_c("Wave ", wave)),
         level2 = as.factor(level) %>% fct_rev(),
         mode2 = as.factor(mode) %>% fct_rev()) %>% 
  mutate(est = est * 100) %>% 
  ggplot(aes(est, sim2, fill = mode2)) +
  geom_bar(stat = "identity") +
  facet_grid(~ level2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 25, by = 5)) +
  geom_vline(xintercept = seq(0, 25, by = 5), color = "white") +
  labs(fill = "Mode", y = "", x = "Proportion of calls saved")


ggsave("./results/hilda_saving_avg.png", dpi = 500, width = 7)

