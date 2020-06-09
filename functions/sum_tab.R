
# function to do sumary statistics for variables

sum_tab <- function(data) {
  # get type of variable it is
  data_desc <- rbind(map_df(data, class),
                     map_df(data, function(x)
                       length(unique(x)))) %>%
    t() %>%
    data.frame(stringsAsFactors = F) %>%
    tbl_df() %>%
    set_names(c("type", "cat")) %>%
    mutate(cat = as.numeric(cat),
           var = names(data))
  
  # separate categorical and continious variables and data
  cont_var <- filter(data_desc, type %in% c("Date", "numeric", "integer") &
                       cat > 3)$var
  cat_var <- names(data)[!names(data) %in% cont_var]
  
  cat_data <- data[cat_var]
  cont_data <- data[cont_var]
  
  # set out output
  output <- list(NULL)
  
  
  # Descriptives for continious variables -----------------------------------
  
  if (ncol(cont_data) > 0) {
    cont_desc <- cont_data %>%
      map(function(x) {
        m = round(mean(x, na.rm = T), 2)
        s = round(sd(x, na.rm = T), 2)
        miss = round(sum(is.na(x)) / length(x) * 100, 1)
        cbind(m, s, miss) %>%
          data.frame() %>%
          tbl_df() %>%
          set_names(c("Mean", "SD", "Miss"))
      }) %>%
      reduce(rbind) %>%
      mutate(Variable = names(cont_data)) %>%
      select(Variable, everything())
    
    output <- list(`Continuous variables` = cont_desc)
    
  }
  
  # Descriptives for categorical data ---------------------------------------
  
  
  if (ncol(cat_data) > 0) {
    
    # get name of data
    nm <- deparse(substitute(data))
    
    # get descriptives table
    capture.output(descriptives <- cat_data %>%
                     map(desc_tab), file = "NULL")
    
    # get variable names
    var_name <- names(descriptives)
    
    # get number of categories
    var_cat <- map(descriptives, nrow)
    
    # made datsaet with descriptives
    cat_desc <- descriptives %>%
      map(function(x)
        select(x, Code, Freq., Perc.) %>%
          mutate(Code = as.character(Code))) %>%
      reduce(rbind) %>%
      mutate(
        Variable = rep(var_name, var_cat),
        Code = case_when(
          Code == 1 &  !var_cat[Variable] > 3  ~ "Yes",
          Code == 0 & !var_cat[Variable] > 3 ~ "No",
          is.na(Code) ~ "Missing",
          TRUE ~ as.character(Code)
        )
      ) %>%
      filter(Freq. > 0) %>%
      select(Variable, everything())
    
    output <- c(output, list(`Categorical variables` = cat_desc))
  }
  
  output
}
