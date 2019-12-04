










x <- "out_2"
df <- usw

ukhls_out_fun <- function(df, x){
  
  var <- enquo(out_2)
  wave <- str_extract(x, "[0-9]")
  
  ph_var <- str_c("int_phase_", quo_name(x))
  ph_var <- enquo(ph_var)
  
  
  new_var1 <- str_c("out_p1_", wave)
  
  expr1 <- str_c(quo_name(ph_var), " > 1 | ",
                 quo_name(out_var), " == 0, 0, ", 
                 quo_name(out_var))
  quo(!!! expr1)
  
  mutate(df,
         !! new_var1 := ifelse(!! var == 1, 1, 0)
  ) %>% 
    count(out_p1_2, !! var)
}

quo(!! new_var1 := ifelse(!!var == 1, 1, 0))



ukhls_out_fun(usw, "out_2") %>% 
  count(out_p3_2)


testfun <- function(df, expr){
  expr <- enquo(expr)
  mean_name <- str_c("mean_", quo_name(expr))
  sum_name <- str_c("sum_", quo_name(expr))
  
  mutate(df,
         !! mean_name := mean(!! expr),
         !! sum_name := sum(!! expr))
}

quo(sum(!! expr))

testfun(mtcars, carb)

usw <- usw %>% 
  ungroup()


