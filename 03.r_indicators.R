


# setup -------------------------------------------------------------------



if (Sys.getenv("USERNAME") == "msassac6"){.libPaths(c(
  paste0(
    "C:/Users/",
    Sys.getenv("USERNAME"),
    "/Dropbox (The University of Manchester)/R/package"
  ),
  .libPaths()
))}

# install packages from CRAN
p_needed <- c("tidyverse", "haven", "lubridate", "devtools")

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]

if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

lapply(p_needed, require, character.only = TRUE)


# load functions
functions <- str_subset(list.files("./functions/"), ".R")

for (i in seq_along(functions)){
  source(str_c("./functions/", functions[i]))
}


load("./data/usw4.RData")

source("./functions/RISQ-R-indicators-v21.R")





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
  response_model <- formula(str_c(i, " ~ ",
                                  str_c(vars, collapse = " + ")))
  
  data <- usw_small %>%
    select(vars, i) %>%
    na.omit()
  
  indicator <- getRIndicator(response_model, data,
                             withPartials = T)
  
  indicator
}
  
rind_run(outcomes[1])  
  
  
  


for (i in outcomes) {
  
response_model <- formula(str_c(i, " ~ ",
                                str_c(vars, collapse = " + ")))

data <- usw_small %>%
  select(vars, i) %>%
  na.omit()

indicator <- getRIndicator(response_model, data,
                            withPartials = T)

save(indicator, file = str_c("./data/rindicator_", i, ".RData"))
gc()

}


# 
# rindicators_full <- list(NULL)
# 
# save(rindicators_full, file = "./data/rindicators_full.RData")




# 
# 
# 
# 
# rindicators_p1 <- list(NULL)
# 
# for(i in 2:6) {
# response_model <- formula(str_c("out ~ ",
#                                 str_c(vars, collapse = " + ")))
# 
# data <- usw_small %>%
#   select(vars, str_c("out_", i),
#          str_c("int_phase_", i)) %>%
#   rename_all(funs(str_remove(., str_c("_", i)))) %>%
#   mutate(out = case_when(int_phase > 1 ~ 0,
#                          int_phase == 1 ~ 1,
#                          out == 0 ~ 0),
#          int_phase = ifelse(is.na(int_phase), 0, int_phase)) %>%
#   na.omit()
# 
# indicator <- getRIndicator(response_model, data,
#                             withPartials = T)
# 
# rindicators_p1[[i]] <- indicator
# 
# }
# 
# save(rindicators_p1, file = "./data/rindicators_p1.RData")
# load( "./data/rindicators_p1.RData")



load( "./data/rindicators_full.RData")






# get r indicators

rindicators_select[[1]]








indicator <- rindicators_select[[1]]

out <- rind_result(indicator)

res <- out[[2]]


tab1 <- res %>%
  select(variable, Pu, Pc) %>% 
  gather(Pu, Pc, key = type, value = value)

res <- res %>%
  select(variable, PuSE, PcSEApprox) %>% 
  gather(PuSE, PcSEApprox, key = SE, value = valueSE) %>% 
  select(-variable, -SE) %>% 
  cbind(tab1) %>% 
  select(variable, type, value, valueSE)

r_vars <- ggplot(res, aes(x = variable, 
                          y = value, 
                          ymin = value - 1.96*valueSE, 
                          ymax = value + 1.96*valueSE,
                          color = type)) +
  geom_point() + 
  geom_errorbar(width = 0) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 0)

r_vars

# ggsave(filename = "./results/var_results.png",
#        dpi = 500)




res <- out[[3]]

# graphs <- list(NULL)

# for(i in seq_along(res)) {
# graphs[[i]] <- res[[i]] %>% 
#   select(category, PuUnadj, PcUnadj) %>% 
#   gather(PuUnadj, PcUnadj, key = type, value = value) %>% 
#   ggplot(aes(value, category, color = type)) +
#   geom_point(size = 5) +
#   geom_vline(xintercept = 0) + 
#   theme_bw() +
#   labs(title = names(res)[i]) 
# }

big_data <- matrix(nrow = 0,
                   ncol = 6)
colnames(big_data) <- c("var", 
                        "category", 
                        "PuUnadj",
                        "PcUnadj",
                        "PuUnadjSE",
                        "PcUnadjSE") 

for (i in seq_along(res)) {
  
  temp_data <- res[[i]] %>% 
    select(category, PuUnadj, PcUnadj,
           PuUnadjSE, PcUnadjSE) %>% 
    mutate(var = names(res)[i])
  
  big_data <- rbind(big_data, temp_data)
  
}

big_data <- big_data %>% 
  rename_all(funs(str_remove(., "Unadj")))

big_data1 <- big_data %>% 
  select(var, category, Pu, Pc) %>% 
  gather(Pu, Pc, key = type, value = value)

big_data <- big_data %>% 
  select(var, category, PuSE, PcSE) %>% 
  gather(PuSE, PcSE, key = typeSE, value = valueSE) %>%
  select(-var, -category) %>% 
  cbind(big_data1)


ggplot(big_data, aes(category, y = value,
                     ymin = value - 1.96*valueSE, 
                     ymax = value + 1.96*valueSE,
                     color = type)) +
  geom_point() + geom_errorbar(width = 0) + coord_flip() +
  theme_bw() +
  facet_grid(var ~ . , scales = "free") +
  geom_hline(yintercept = 0) +
  theme(strip.text.y = element_text(size = 8, angle = 360),
        axis.text.x = element_text(size = 5, vjust = 2))

ggsave(filename = "./results/cat_results.png",
       dpi = 500, width = 8, height = 12)

















# delete? seems duplicated ------------------------------------------------





# get R indicators for all the outcomes -----------------------------------
# 
# 
# for (i in outcomes) {
#   
#   response_model <- formula(str_c(i, " ~ ",
#                                   str_c(vars, collapse = " + ")))
#   
#   data <- usw_small %>%
#     select(vars, i) %>%
#     na.omit()
#   
#   indicator <- getRIndicator(response_model, data,
#                              withPartials = T)
#   
#   save(indicator, file = str_c("./data/rindicator_", i, ".RData"))
#   gc()
#   
# }


rin_files <- list.files("./data/", pattern = "rindicator_", full.names = T)

list_rind <- list(NULL)
for (i in 1:length(rin_files)) {
  load(rin_files[[i]])
  list_rind[[i]] <- indicator
}

# 
# rindicators_full <- list(NULL)
# 
# save(rindicators_full, file = "./data/rindicators_full.RData")




# 
# 
# 
# 
# rindicators_p1 <- list(NULL)
# 
# for(i in 2:6) {
# response_model <- formula(str_c("out ~ ",
#                                 str_c(vars, collapse = " + ")))
# 
# data <- usw_small %>%
#   select(vars, str_c("out_", i),
#          str_c("int_phase_", i)) %>%
#   rename_all(funs(str_remove(., str_c("_", i)))) %>%
#   mutate(out = case_when(int_phase > 1 ~ 0,
#                          int_phase == 1 ~ 1,
#                          out == 0 ~ 0),
#          int_phase = ifelse(is.na(int_phase), 0, int_phase)) %>%
#   na.omit()
# 
# indicator <- getRIndicator(response_model, data,
#                             withPartials = T)
# 
# rindicators_p1[[i]] <- indicator
# 
# }
# 
# save(rindicators_p1, file = "./data/rindicators_p1.RData")
# load( "./data/rindicators_p1.RData")



load( "./data/rindicators_full.RData")




# fun indicators for new outcomes
# 
# outcomes <- str_c(c("out.sp1", "out.sp3", "out.sp13"), "_",
#                   rep(3:6, each = 3))
# 
# rindicators_select <- list(NULL)
# index <- 1
# 
# for (i in outcomes) {
# response_model <- formula(str_c(i, " ~ ",
#                                 str_c(vars, collapse = " + ")))
# 
# data <- usw_small %>%
#   select(vars, i) %>%
#   na.omit()
# 
# indicator <- getRIndicator(response_model, data,
#                             withPartials = T)
# 
# rindicators_select[[index]] <- indicator
# 
# index <- index + 1
# 
# }
# 
# save(rindicators_select, file = "./data/rindicators_select.RData")


load("./data/rindicators_select.RData")
str(rindicators_select[[1]])


outcomes <- str_c(c("out.sp1", "out.sp3", "out.sp13"), "_",
                  rep(3:6, each = 3))






# get r indicators


rin_files <- list.files("./data/", pattern = "rindicator_", full.names = T)

list_rind <- list(NULL)
for (i in 1:length(rin_files)) {
  load(rin_files[[i]])
  list_rind[[i]] <- indicator
  names(list_rind)[i] <- str_remove_all(rin_files[i], ".+indicator_out2|\\.RData")
}

# function to get overall R indicators
get_overall_rind <- function(x) {
  tibble(
    R = x$R,
    RSE = x$RSE,
    RUnadj = x$RUnadj,
    RBiasFactor = x$RBiasFactor
  )
}

map_df(list_rind, get_overall_rind) %>% 
  mutate(model = names(list_rind),
         wave = str_extract(model, "[0-9]$"))




indicator <- list_rind[[1]]

out <- rind_result(indicator)

res <- out[[2]]


tab1 <- res %>%
  select(variable, Pu, Pc) %>% 
  gather(Pu, Pc, key = type, value = value)

res <- res %>%
  select(variable, PuSE, PcSEApprox) %>% 
  gather(PuSE, PcSEApprox, key = SE, value = valueSE) %>% 
  select(-variable, -SE) %>% 
  cbind(tab1) %>% 
  select(variable, type, value, valueSE)

r_vars <- ggplot(res, aes(x = variable, 
                          y = value, 
                          ymin = value - 1.96*valueSE, 
                          ymax = value + 1.96*valueSE,
                          color = type)) +
  geom_point() + 
  geom_errorbar(width = 0) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 0)

r_vars

# ggsave(filename = "./results/var_results.png",
#        dpi = 500)




res <- out[[3]]

# graphs <- list(NULL)

# for(i in seq_along(res)) {
# graphs[[i]] <- res[[i]] %>% 
#   select(category, PuUnadj, PcUnadj) %>% 
#   gather(PuUnadj, PcUnadj, key = type, value = value) %>% 
#   ggplot(aes(value, category, color = type)) +
#   geom_point(size = 5) +
#   geom_vline(xintercept = 0) + 
#   theme_bw() +
#   labs(title = names(res)[i]) 
# }

big_data <- matrix(nrow = 0,
                   ncol = 6)
colnames(big_data) <- c("var", 
                        "category", 
                        "PuUnadj",
                        "PcUnadj",
                        "PuUnadjSE",
                        "PcUnadjSE") 

for(i in seq_along(res)) {
  
  temp_data <- res[[i]] %>% 
    select(category, PuUnadj, PcUnadj,
           PuUnadjSE, PcUnadjSE) %>% 
    mutate(var = names(res)[i])
  
  big_data <- rbind(big_data, temp_data)
  
}

big_data <- big_data %>% 
  rename_all(funs(str_remove(., "Unadj")))

big_data1 <- big_data %>% 
  select(var, category, Pu, Pc) %>% 
  gather(Pu, Pc, key = type, value = value)

big_data <- big_data %>% 
  select(var, category, PuSE, PcSE) %>% 
  gather(PuSE, PcSE, key = typeSE, value = valueSE) %>%
  select(-var, -category) %>% 
  cbind(big_data1)


ggplot(big_data, aes(category, y = value,
                     ymin = value - 1.96*valueSE, 
                     ymax = value + 1.96*valueSE,
                     color = type)) +
  geom_point() + geom_errorbar(width = 0) + coord_flip() +
  theme_bw() +
  facet_grid(var ~ . , scales = "free") +
  geom_hline(yintercept = 0) +
  theme(strip.text.y = element_text(size = 8, angle = 360),
        axis.text.x = element_text(size = 5, vjust = 2))

ggsave(filename = "./results/cat_results.png",
       dpi = 500, width = 8, height = 12)


