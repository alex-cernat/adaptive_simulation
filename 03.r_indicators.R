

if(Sys.getenv("USERNAME") == "msassac6"){.libPaths(c(
  paste0(
    "C:/Users/",
    Sys.getenv("USERNAME"),
    "/Dropbox (The University of Manchester)/R/package"
  ),
  .libPaths()
))}

# install packages from CRAN
p_needed <-
  c("tidyverse", "haven", "lubridate",
    "devtools")

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]

if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

lapply(p_needed, require, character.only = TRUE)

functions <- str_subset(list.files("./functions/"), ".R")

for (i in seq_along(functions)){
  source(str_c("./functions/", functions[i]))
}

knitr::opts_chunk$set(echo = F)

load("./data/usw3.RData")

source("./functions/RISQ-R-indicators-v21.R")




# Descriptive statistics for independent variables



usw_small <- usw %>% 
  dplyr::select(agecat:education_fct, 
                starts_with("out_"),
                starts_with("int_phase"),
                starts_with("out.s")) %>% 
  dplyr::select(- employed, -relationship,
                -benefit_prop, -countryofbirth,
                -hiqual, -long_cond, -work_limit)

summary(usw_small)





# Descriptive statistics for outcome

Response patterns for first 6 waves:
  
  - base is respondents in wave 1
- 1 = response, 0 = not full response, 3 = not issues, NA = ineligible


nodead <- read.csv("./results/us_resp.csv")
nodead$prop <- round(nodead$prop, 2)
names(nodead)[1] <- "out_2"
print(nodead[1:25, ])




# R indicator results for wave 2



vars <- c("adultsinhh", "agecat", "benefit", "childreninhh",
          "countryofbirth_fct", "education_fct", "employed_fct",
          "ineducation", "health_sum", "likelymove",
          "owner", "relationship_fct", "female", "urb")



# # 
# # rindicators_full <- list(NULL)
# # 
# # for(i in 2:6) {
# response_model <- formula(str_c("out ~ ",
#                                 str_c(vars, collapse = " + ")))
# 
# data <- usw_small %>%
#   select(vars, str_c("out_", i)) %>%
#   rename_all(funs(str_remove(., str_c("_", i)))) %>%
#   na.omit()
# # 
# # indicator <- getRIndicator(response_model, data,
# #                             withPartials = T)
# # 
# # rindicators_full[[i]] <- indicator
# # 
# # }
# # 
# # save(rindicators_full, file = "./data/rindicators_full.RData")
# # 
# # 
# rindicators_p1 <- list(NULL)
# # 
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



load( "./data/rindicators_full.RData")
load( "./data/rindicators_p1.RData")












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

