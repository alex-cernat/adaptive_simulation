
# 05. Make fielwwork graph ------------------------------------------------



## maybe delete

# 
# us3_full <- read_dta("./data/stata/us_w3/c_indall.dta")
# 
# vars <- c("intdatd_dv", "intdatm_dv", "intdaty_dv",
#           "month", "hhorig", "ff_ivlolw", "ivfio",
#           "hidp")
# 
# us3 <- us3_full %>% 
#   dplyr::select(pidp, str_c("c_", vars)) %>% 
#   rename_all(funs(str_replace_all(., 'c_', '') %>%
#                     str_replace_all(., '_dv', ''))) 
# 
# # import wave 6
# 
# 
# us6_full <- read_dta("./data/stata/us_w6/f_indall.dta")
# 
# us6 <- us6_full %>% 
#   dplyr::select(pidp, str_c("f_", vars)) %>% 
#   rename_all(funs(str_replace_all(., 'f_', '') %>%
#                     str_replace_all(., '_dv', ''))) 
# 




# select only first wave general sample

us3 <- us3 %>% 
  filter(hhorig == 1)

# make date of interview

usl3 <- us3 %>% 
  mutate(intdate = dmy("1-1-1988"),
         intdate = update(intdate,
                          day = intdatd, 
                          month = intdatm, 
                          year = intdaty),
         month = relabel(month))



usl3 <- usl3 %>%
  filter(ivfio == 1) %>% 
  group_by(month) %>% 
  arrange(intdate, hidp) %>%
  mutate(case = 1,
         cum_case = cumsum(case)/sum(case),
         phase = case_when(cum_case > 0.9 & cum_case < 0.95 ~ 1,
                           cum_case > 0.95 ~ 2),
         phase1 = as_date(ifelse(is.na(phase), intdate, NA)),
         phase2 = as_date(ifelse(phase == 1, intdate, NA)),
         phase3 = as_date(ifelse(phase == 2, intdate, NA)),
         end = max(intdate),
         phase_j1end = min(intdate) + weeks(6),
         phase_j2end = phase_j1end + weeks(6),
         phase_j3end = phase_j2end + weeks(4)) %>%
  ungroup() %>% 
  arrange(month, intdate, cum_case)




complete_phase_w3 <- usl3 %>% 
  group_by(month) %>% 
  mutate(prop_p1 = ifelse(intdate > phase_j1end, NA, cum_case),
         prop_p2 = ifelse(intdate > phase_j2end, NA, cum_case),
         prop_p3 = ifelse(intdate > phase_j3end, NA, cum_case),
         p_p1 = max(prop_p1, na.rm = T),
         p_p2 = max(prop_p2, na.rm = T),
         p_p3 = max(prop_p3, na.rm = T)) %>% 
  count(p_p1, p_p2, p_p3)



table(usl3$month)
table(usl3$phase_j1end)
table(usl3$phase_j2end)
table(usl3$phase_j3end)







limits <- usl3 %>% group_by(month) %>% 
  summarise(start2 = min(phase2, na.rm = T),
            end2 = max(phase2, na.rm = T),
            start3 = min(phase3, na.rm = T),
            end3 = max(phase3, na.rm = T),
            max = max(intdate)) %>% 
  mutate(start3 = as_date(ifelse(start3 == end2, 
                                 start3 + days(1), 
                                 start3)))



limits2 <- usl3 %>% group_by(month) %>% 
  summarise(start1 = min(intdate, na.rm = T),
            end1 = mean(phase_j1end, na.rm = T),
            start2 = mean(phase_j1end, na.rm = T) + days(1),
            end2 = mean(phase_j2end, na.rm = T),
            start3 = mean(phase_j2end, na.rm = T) + days(1),
            end3 = mean(phase_j3end, na.rm = T),
            start4 = mean(phase_j3end, na.rm = T) + days(1),
            end4 = max(intdate, na.rm = T))

limits2 %>% 
  left_join(usl3, by = c("end1" = "intdate")) %>% 
  group_by(month.y) %>% 
  mutate(test = ifelse(intdate == end1, cum_case, NA),
         test2 = max(test, na.rm = T))


ggplot(usl3, aes(x = intdate)) +
  geom_freqpoly() + 
  geom_rect(data = limits, 
            aes(xmin = start2, 
                xmax = end2, 
                ymin = 0, ymax = Inf),
            color = "blue",
            alpha = 0.3,
            inherit.aes = FALSE) + 
  geom_rect(data = limits, 
            aes(xmin = start3, 
                xmax = end3, 
                ymin = 0, ymax = Inf),
            color = "red",
            alpha = 0.3,
            inherit.aes = FALSE) +
  facet_wrap(~month, 
             scales = "free", 
             ncol = 3)


ggplot(usl3, aes(x = intdate)) +
  geom_freqpoly() + 
  geom_rect(data = limits2, 
            aes(xmin = start1, 
                xmax = end1, 
                ymin = 0, ymax = Inf),
            color = "green",
            alpha = 0.3,
            inherit.aes = FALSE) +
  geom_rect(data = limits2, 
            aes(xmin = start2, 
                xmax = end2, 
                ymin = 0, ymax = Inf),
            color = "blue",
            alpha = 0.3,
            inherit.aes = FALSE) + 
  geom_rect(data = limits2, 
            aes(xmin = start3, 
                xmax = end3, 
                ymin = 0, ymax = Inf),
            color = "red",
            alpha = 0.3,
            inherit.aes = FALSE) + 
  geom_rect(data = limits2, 
            aes(xmin = start4, 
                xmax = end4, 
                ymin = 0, ymax = Inf),
            color = "yellow",
            alpha = 0.3,
            inherit.aes = FALSE) +
  facet_wrap(~month, 
             scales = "free",
             ncol = 3)





# make date of interview

us6 <- us6 %>% 
  filter(hhorig == 1)


usl6 <- us6 %>% 
  mutate(intdate = dmy("1-1-1988"),
         intdate = update(intdate,
                          day = intdatd, 
                          month = intdatm, 
                          year = intdaty),
         month = relabel(month))

summary(usl6[usl6$ivfio == 1, ])

usl6 <- usl6 %>%
  filter(ivfio == 1) %>% 
  group_by(month) %>% 
  arrange(intdate, hidp) %>%
  mutate(case = 1,
         cum_case = cumsum(case)/sum(case),
         phase = case_when(cum_case > 0.9 & cum_case < 0.95 ~ 1,
                           cum_case > 0.95 ~ 2),
         phase1 = as_date(ifelse(is.na(phase), intdate, NA)),
         phase2 = as_date(ifelse(phase == 1, intdate, NA)),
         phase3 = as_date(ifelse(phase == 2, intdate, NA)),
         end = max(intdate),
         phase_j1end = min(intdate) + weeks(10),
         phase_j2end = phase_j1end + weeks(9),
         phase_j3end = phase_j2end + weeks(4)) %>%
  ungroup() %>% 
  arrange(month, intdate, cum_case)

usl6 %>% 
  group_by(month) %>% 
  summarise(start = min(intdate),
            p1 = mean(phase_j1end),
            p2 = mean(phase_j2end),
            p3 = mean(phase_j3end),
            end = max(intdate))


complete_phase_w6 <- usl6 %>% 
  group_by(month) %>% 
  mutate(prop_p1 = ifelse(intdate > phase_j1end, NA, cum_case),
         prop_p2 = ifelse(intdate > phase_j2end, NA, cum_case),
         prop_p3 = ifelse(intdate > phase_j3end, NA, cum_case),
         p_p1 = max(prop_p1, na.rm = T),
         p_p2 = max(prop_p2, na.rm = T),
         p_p3 = max(prop_p3, na.rm = T)) %>% 
  count(p_p1, p_p2, p_p3)




limits <- usl6 %>% group_by(month) %>% 
  summarise(start2 = min(phase2, na.rm = T),
            end2 = max(phase2, na.rm = T),
            start3 = min(phase3, na.rm = T),
            end3 = max(phase3, na.rm = T),
            max = max(intdate)) %>% 
  mutate(start3 = as_date(ifelse(start3 == end2, 
                                 start3 + days(1), 
                                 start3)))



limits2 <- usl6 %>% group_by(month) %>% 
  summarise(start1 = min(intdate, na.rm = T),
            end1 = mean(phase_j1end, na.rm = T),
            start2 = mean(phase_j1end, na.rm = T) + days(1),
            end2 = mean(phase_j2end, na.rm = T),
            start3 = mean(phase_j2end, na.rm = T) + days(1),
            end3 = mean(phase_j3end, na.rm = T),
            start4 = mean(phase_j3end, na.rm = T) + days(1),
            end4 = max(intdate, na.rm = T))




ggplot(usl6, aes(x = intdate)) +
  geom_freqpoly() + 
  geom_rect(data = limits, 
            aes(xmin = start2, 
                xmax = end2, 
                ymin = 0, ymax = Inf),
            color = "blue",
            alpha = 0.3,
            inherit.aes = FALSE) + 
  geom_rect(data = limits, 
            aes(xmin = start3, 
                xmax = end3, 
                ymin = 0, ymax = Inf),
            color = "red",
            alpha = 0.3,
            inherit.aes = FALSE) +
  facet_wrap(~month, 
             scales = "free", 
             ncol = 3)


ggplot(usl6, aes(x = intdate)) +
  geom_freqpoly() + 
  geom_rect(data = limits2, 
            aes(xmin = start1, 
                xmax = end1, 
                ymin = 0, ymax = Inf),
            color = "green",
            alpha = 0.3,
            inherit.aes = FALSE) +
  geom_rect(data = limits2, 
            aes(xmin = start2, 
                xmax = end2, 
                ymin = 0, ymax = Inf),
            color = "blue",
            alpha = 0.3,
            inherit.aes = FALSE) + 
  geom_rect(data = limits2, 
            aes(xmin = start3, 
                xmax = end3, 
                ymin = 0, ymax = Inf),
            color = "red",
            alpha = 0.3,
            inherit.aes = FALSE) + 
  geom_rect(data = limits2, 
            aes(xmin = start4, 
                xmax = end4, 
                ymin = 0, ymax = Inf),
            color = "yellow",
            alpha = 0.3,
            inherit.aes = FALSE) +
  facet_wrap(~month, 
             scales = "free",
             ncol = 3)




