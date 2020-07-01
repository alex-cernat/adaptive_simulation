

# graphs for hilda --------------------------------------------------------


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


rinds <- read_csv("./data/hilda_rinds.csv")
rr <- read_csv("./data/hilda_rr.csv")
costs <- read_csv("./data/hilda_costs.csv")


# rr graphs --------------------------------------------------------------------




rr %>%
  filter(type == "Regular") %>% 
  ggplot(aes(wave, est, color = sim
             , shape = level, linetype = level)
         ) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ sample, scales = "free") +
  theme_bw() +
  labs(x = "Wave", y = "Response rate", color = "Simulation type",
       shape = "Level", linetype = "Level")

ggsave("./results/hilda_rr_plot.png", dpi = 500, width = 7)



rr %>%
  filter(type == "Balanced") %>% 
  ggplot(aes(wave, est, color = sim, 
             shape = level, linetype = level)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ sample, scales = "free") +
  theme_bw() +
  labs(x = "Wave", y = "Response rate", color = "Simulation type",
       shape = "Level", linetype = "Level")

ggsave("./results/hilda_rr_plot_balanced.png", dpi = 500, width = 7)





# rinds graphs ------------------------------------------------------------


rinds %>% 
#  mutate(level = as.factor(level)) %>% 
  ggplot(aes(wave, est, 
             color = sim, 
             group = sim)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                width = 0, alpha = 0.5) +
  facet_grid(sample ~ level) +
  theme_bw() +
  labs(y = "R indicator")

ggsave("./results/hilda_rindicators.png", dpi = 500, width = 7)


# saving data -------------------------------------------------------------



costs %>% 
  filter(sample == "Main") %>% 
  mutate(sim2 = str_replace(sim, "sim", "Simulation "),
         sim2 = as.factor(sim2)) %>% 
  mutate(est = est * 100) %>% 
  ggplot(aes(est, as.factor(wave) %>% fct_rev(), fill = sim2)) +
    geom_bar(stat = "identity", position = "dodge") +
  facet_grid(mode~level) +
  theme_bw() +
  geom_vline(xintercept = c(2, 4), color = "white") +
  labs(fill = "Wave", y = "Simulation", x = "Proportion saved")

ggsave("./results/hilda_saving_main.png", dpi = 500, width = 7)


