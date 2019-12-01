load( "./data/rindicators_full.RData")
load( "./data/rindicators_p1.RData")
load("./data/rindicators_select.RData")

results <- lapply(rindicators_full[-1], rind_result)
results_p1 <- lapply(rindicators_p1[-1], rind_result)
results_select <- lapply(rindicators_select, rind_result)


R_ind <- lapply(results, `[[`, 1)
R_ind <- reduce(R_ind, rbind) %>% 
  mutate(wave = rep(2:6, each = 2))


R_ind_p1 <- lapply(results_p1, `[[`, 1)
R_ind_p1 <- reduce(R_ind_p1, rbind) %>% 
  mutate(wave = rep(2:6, each = 2))


outcomes <- str_c(c("out.sp1", "out.sp3", "out.sp13"), "_",
                  rep(3:6, each = 3))

R_ind_select <- lapply(results_select, `[[`, 1)
R_ind_select <- reduce(R_ind_select, rbind) %>% 
  mutate(wave = rep(str_extract(outcomes, "[0-9]$"), each = 2),
         group = rep(str_remove_all(outcomes, "out\\.|_[0-9]$"), each = 2))


## Rindicator and CV change in time

r_ind_data <- R_ind %>% 
  rbind(R_ind_p1) %>% 
  mutate(group = rep(c("Phase 3", "Phase 1"), each = 10)) %>%
  rbind(R_ind_select) %>% 
  mutate(ValueSE = as.numeric(ValueSE),
         group = fct_recode(group,
                    "Final response" = "Phase 3",
                    "Response phase 1" = "Phase 1",
                    "Phase 1 bottom 75%" = "sp1",
                    "Phase 3 top 75%" = "sp3",
                    "Phase 1+3 propensity" = "sp13"))

r_ind_data %>% 
  filter(wave > 2) %>% 
ggplot(aes(factor(wave), y = Value, group = group,
           color = group)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = Value - 1.96*ValueSE, 
                    ymax = Value + 1.96*ValueSE),
                width = 0) +
  theme_bw() + 
  labs(x = "Wave", 
       y = "R indicator value",
       color = "Phase") +
  facet_wrap(~ Indicator, 
             scales = "free")


write.csv(r_ind_data, file = "./results/r_results_full.csv")
ggsave(filename = "./results/r_results_full.png",
       dpi = 500, width = 9, height = 5)


## variables change in time



R_vars <- lapply(results, `[[`, 2)
R_vars <- reduce(R_vars, rbind) %>% 
  mutate(wave = rep(2:6, each = 13))

R_vars_p1 <- lapply(results_p1, `[[`, 2)
R_vars_p1 <- reduce(R_vars_p1, rbind) %>% 
  mutate(wave = rep(2:6, each = 13))


tab1 <- R_vars %>%
  select(variable, Pu, Pc, wave) %>% 
  gather(Pu, Pc, key = type, value = value)

res <- R_vars %>%
  select(variable, PuSE, PcSEApprox, wave) %>% 
  gather(PuSE, PcSEApprox, key = SE, value = valueSE) %>% 
  select(-variable, -SE, -wave) %>% 
  cbind(tab1) %>% 
  select(wave, variable, type, value, valueSE)

tab2 <- R_vars_p1 %>%
  select(variable, Pu, Pc, wave) %>% 
  gather(Pu, Pc, key = type, value = value)

res2 <- R_vars_p1 %>%
  select(variable, PuSE, PcSEApprox, wave) %>% 
  gather(PuSE, PcSEApprox, key = SE, value = valueSE) %>% 
  select(-variable, -SE, -wave) %>% 
  cbind(tab2) %>% 
  select(wave, variable, type, value, valueSE)


res <- res %>% 
  rbind(res2) %>%
  mutate(phase = rep(c("Phase 3", "Phase 1"), each = nrow(res)))


r_vars_u <- res %>% 
  filter(type == "Pu") %>% 
ggplot(aes(factor(wave),
                          y = value, 
                          ymin = value - 1.96*valueSE, 
                          ymax = value + 1.96*valueSE,
                group = phase,
           color = phase)) +
  geom_point(size = .5) + 
  geom_line() +
  geom_errorbar(width = 0) +
  theme_bw()) +
  geom_hline(yintercept = 0) +
  labs(x = "Wave", y = "Unconditional partial R indicator",
       title = "Unconditional partial R indicator by wave",
       color = "Phase") +
  facet_wrap(~ variable, ncol = 3) +
  scale_y_continuous(limits = c(-0.01, 0.1))
r_vars_u


ggsave(filename = "./results/var_results_full_uncond.png",
       height = 8, width = 8, dpi = 500)

r_vars_c <- res %>% 
  filter(type == "Pc") %>% 
  ggplot(aes(factor(wave),
             y = value, 
             ymin = value - 1.96*valueSE, 
             ymax = value + 1.96*valueSE,
             color = phase,
             group = phase)) +
  geom_point(size = 0.7) + 
  geom_line() +
  geom_errorbar(width = 0) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  labs(x = "Wave", y = "Conditional partial R indicator",
       title = "Conditional partial R indicator by wave",
       color = "Phase") +
  facet_wrap(~ variable, ncol = 3) +
  scale_y_continuous(limits = c(-0.01, 0.05))

r_vars_c


ggsave(filename = "./results/var_results_full_cond.png",
       height = 8, width = 8, dpi = 500)

## graphs by category

res <- lapply(results, `[[`, 3)
res_p1 <- lapply(results_p1, `[[`, 3)


big_data <- matrix(nrow = 0,
                   ncol = 6)
colnames(big_data) <- c("var", 
                        "category", 
                        "PuUnadj",
                        "PcUnadj",
                        "PuUnadjSE",
                        "PcUnadjSE") 


for (j in seq_along(res)) {
  for (i in seq_along(res[[j]])) {
    temp_data <- res[[j]][[i]] %>%
      mutate(var = names(res[[j]])[[i]]) %>% 
      select(var, category, PuUnadj, PcUnadj,
             PuUnadjSE, PcUnadjSE) 
    
    big_data <- rbind(big_data, temp_data)
    
  }
}

big_data <- big_data %>% tbl_df() %>% 
  rename_all(funs(str_remove(., "Unadj"))) %>% 
  mutate(wave = rep(2:6, each = nrow(big_data)/5))

big_data1 <- big_data %>% 
  select(var, wave, category, Pu, Pc) %>% 
  gather(Pu, Pc, key = type, value = value)

big_data <- big_data %>% 
  select(var, wave, category, PuSE, PcSE) %>% 
  gather(PuSE, PcSE, key = typeSE, value = valueSE) %>%
  select(-var, -category, - wave) %>% 
  cbind(big_data1)



# just phase 1


big_data_p1 <- matrix(nrow = 0,
                   ncol = 6)
colnames(big_data_p1) <- c("var", 
                        "category", 
                        "PuUnadj",
                        "PcUnadj",
                        "PuUnadjSE",
                        "PcUnadjSE") 


for (j in seq_along(res_p1)) {
  for (i in seq_along(res_p1[[j]])) {
    temp_data <- res_p1[[j]][[i]] %>%
      mutate(var = names(res_p1[[j]])[[i]]) %>% 
      select(var, category, PuUnadj, PcUnadj,
             PuUnadjSE, PcUnadjSE) 
    
    big_data_p1 <- rbind(big_data_p1, temp_data)
    
  }
}

big_data_p1 <- big_data_p1 %>% tbl_df() %>% 
  rename_all(funs(str_remove(., "Unadj"))) %>% 
  mutate(wave = rep(2:6, each = nrow(big_data_p1)/5))

big_data1 <- big_data_p1 %>% 
  select(var, wave, category, Pu, Pc) %>% 
  gather(Pu, Pc, key = type, value = value)

big_data_p1 <- big_data_p1 %>% 
  select(var, wave, category, PuSE, PcSE) %>% 
  gather(PuSE, PcSE, key = typeSE, value = valueSE) %>%
  select(-var, -category, - wave) %>% 
  cbind(big_data1)


big_data <- big_data %>% 
  rbind(big_data_p1) %>%
  mutate(phase = rep(c("Phase 3", "Phase 1"), each = nrow(big_data)))


big_data <- big_data %>% 
  tbl_df() %>% 
  mutate(cat_wave = str_c(category, "_w",wave)) 






for (i in 2:6) {
big_data %>% 
  filter(wave == i) %>% 
ggplot(aes(category, 
           y = value,
           ymin = value - 1.96*valueSE, 
           ymax = value + 1.96*valueSE,
           color = type,
           shape = phase)) +
  geom_point(alpha = 0.5) + 
  geom_errorbar(alpha = 0.5, width = 0) + 
    coord_flip() +
  theme_bw() +
  facet_grid(var ~ . , scales = "free") +
  geom_hline(yintercept = 0) +
  theme(strip.text.y = element_text(size = 8, angle = 360),
        axis.text.x = element_text(size = 5, vjust = 2)) +
  labs(x = "Category", y = "Value", color = "Type",
       shape = "Phase",
       title = str_c("R indicator by category in  wave ", i))
  
  ggsave(filename = str_c("./results/cat_results_full_w", i, ".png"),
         dpi = 500, width = 8, height = 12)
}



big_data %>% 
  filter(type == "Pu") %>%
  arrange(desc(category), desc(wave)) %>%
  mutate(cat_wave = as.factor(cat_wave)) %>% 
  ggplot(aes(fct_inorder(cat_wave), 
             y = value,
             ymin = value - 1.96*valueSE, 
             ymax = value + 1.96*valueSE,
             shape = as.factor(wave),
             group = wave,
             color = phase)) +
  geom_point(alpha = 0.5) + 
  geom_errorbar(alpha = 0.5, width = 0) + 
  coord_flip() +
  theme_bw() +
  facet_grid(var ~ . , scales = "free") +
  geom_hline(yintercept = 0) +
  theme(strip.text.y = element_text(size = 8, angle = 360),
        axis.text.x = element_text(size = 3, vjust = 2)) +
  labs(x = "Category", y = "Value", shape = "Wave",
       title = "Unconditional R indicator by category and wave",
       color = "Phase") 

ggsave(filename = "./results/cat_results_full_uncond.png",
       dpi = 800, width = 8, height = 24)

big_data %>% 
  filter(type == "Pc") %>%
  arrange(desc(category), desc(wave)) %>%
  mutate(cat_wave = as.factor(cat_wave)) %>% 
  ggplot(aes(fct_inorder(cat_wave), y = value,
             ymin = value - 1.96*valueSE, 
             ymax = value + 1.96*valueSE,
             shape = as.factor(wave),
             group = wave,
             color = phase)) +
  geom_point(alpha = 0.5) + 
  geom_errorbar(alpha = 0.5, width = 0) + 
  coord_flip() +
  theme_bw() +
  facet_grid(var ~ . , scales = "free") +
  geom_hline(yintercept = 0) +
  theme(strip.text.y = element_text(size = 8, angle = 360),
        axis.text.x = element_text(size = 3, vjust = 2)) +
  labs(x = "Category", y = "Value", shape = "Wave",
       title = "Conditional R indicator by category and wave",
       color = "Phase")

ggsave(filename = "./results/cat_results_full_cond.png",
       dpi = 800, width = 8, height = 24)
