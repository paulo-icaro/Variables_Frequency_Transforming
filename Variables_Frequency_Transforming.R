# ======================================== #
# === VARIABLES FREQUENCY TRANSFORMING === #
# ======================================== #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
tryCatch(expr = suppressWarnings(library(dplyr)),
         error = function(e){stop('Não é possível prosseguir. Instale os pacotes dplyr.')})
tryCatch(expr = suppressWarnings(library(lubridate)),
         error = function(e){stop('Não é possível prosseguir. Instale o pacotes lubridate.')})



# ============================ #
# === Transforming Dataset === #
# ============================ #
cumulative_transform = function(transform_type, frequency, dataset, change_date = FALSE){
  
  # ---------------------------- #
  # --- Transform Type - Sum --- #
  # ---------------------------- #
  if(transform_type %in% list('soma', 'sum') ){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = 
        dataset
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(data, unit = 'bimonth')) %>%
        group_by(data) %>%
        summarise(across(colnames(dataset[names(dataset) != 'data']), sum))
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(data, unit = 'quarter')) %>%
        group_by(data) %>%
        summarise(across(colnames(dataset[names(dataset) != 'data']), sum))
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(data, unit = 'halfyear')) %>%
        group_by(data) %>%
        summarise(across(colnames(dataset[names(dataset) != 'data']), sum))
    }
  }
  
  
  # ------------------------------------ #  
  # --- Transform Type - Last Period --- #
  # ------------------------------------ #
  else if(transform_type %in% list('periodo_final', 'final_period')){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = 
        dataset
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = 
        dataset %>%
        filter(month(data) %% 2 == 0)
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = 
        dataset %>% 
        filter(month(data) %% 3 == 0)
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = 
        dataset %>% 
        filter(month(data) %% 6 == 0)
    }
  }
  
  
  # ----------------------------- #  
  # --- Transform Type - Mean --- #
  # ----------------------------- #
  else if(transform_type %in% list('media', 'average')){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = unique(
        dataset %>% 
          mutate(data = floor_date(x = data, unit = 'month')) %>%
          group_by(data) %>%
          mutate(across(colnames(dataset[names(dataset) != 'data']), mean)))
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = unique(
        dataset %>% 
          mutate(data = floor_date(x = data, unit = 'bimonth')) %>%
          group_by(data) %>%
          mutate(across(colnames(dataset[names(dataset) != 'data']), mean)))
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = unique(
        dataset %>%
          mutate(data = floor_date(x = data, unit = 'quarter')) %>%
          group_by(data) %>%
          mutate(across(colnames(dataset[names(dataset) != 'data']), mean)))
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = unique(
        dataset %>%
          mutate(data = floor_date(x = data, unit = 'halfyear')) %>%
          group_by(data) %>%
          mutate(across(colnames(dataset[names(dataset) != 'data']), mean)))
    }
  }
  
  
  # ----------------------------------- #  
  # --- Transform Type - Cumulative --- #
  # ----------------------------------- #
  else if(transform_type %in% list('tx_acumulada', 'cumulative_rate')){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(x = data, unit = 'month')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(x = data, unit = 'bimonth')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = 
        dataset %>%
        mutate(data = floor_date(x = data, unit = 'quarter')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = 
        dataset %>%
        mutate(data = floor_date(x = data, unit = 'halfyear')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
  }
  
  
  # ----------------------------- #  
  # --- Transform Type - Diff  --- #
  # ----------------------------- #
  else if(transform_type %in% list('diff_acumulado', 'cumulative_diff')){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = unique(
        dataset %>% 
          mutate(across(colnames(dataset[names(dataset) != 'data']), ~ ifelse(month(data) == 1, .x, .x - lag(.x)))))
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = unique(
        dataset %>% 
          filter(month(data) %% 2 == 0) %>%
          mutate(across(colnames(dataset[names(dataset) != 'data']), ~ ifelse(month(data) == 2, .x, .x - lag(.x)))))
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = unique(
        dataset %>% 
          filter(month(data) %% 3 == 0) %>%
          mutate(across(colnames(dataset[names(dataset) != 'data']), ~ ifelse(month(data) == 3, .x, .x - lag(.x)))))
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = unique(
        dataset %>% 
          filter(month(data) %% 6 == 0) %>%
          mutate(across(colnames(dataset[names(dataset) != 'data']), ~ ifelse(month(data) == 6, .x, .x - lag(.x)))))
    }
  }
  
  
  # --------------------------------------------- #
  # --- Change Date Column to Match Frequency --- #
  # --------------------------------------------- #
  if(change_date == FALSE){
    if(frequency %in% list('mensal','monthly')){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) == 1 ~ '_jan',
                                         month(data) == 2 ~ '_fev',
                                         month(data) == 3 ~ '_mar',
                                         month(data) == 4 ~ '_abr',
                                         month(data) == 5 ~ '_mai',
                                         month(data) == 6 ~ '_jun',
                                         month(data) == 7 ~ '_jul',
                                         month(data) == 8 ~ '_ago',
                                         month(data) == 9 ~ '_set',
                                         month(data) == 10 ~ '_out',
                                         month(data) == 11 ~ '_nov',
                                         .default = '_dez')))
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) %in% c(1,2) ~ '_fev', 
                                         month(data) %in% c(3,4) ~ '_abr',
                                         month(data) %in% c(5,6) ~ '_jun',
                                         month(data) %in% c(7,8) ~ '_ago',
                                         month(data) %in% c(9,10) ~ '_out',
                                         .default = '_dez')))
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) %in% c(1,2,3) ~ '_mar', 
                                         month(data) %in% c(4,5,6) ~ '_jun',
                                         month(data) %in% c(7,8,9) ~ '_set',
                                         .default = '_dez')))
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) - 6 >= 1 ~ '_dez', 
                                         .default = '_jun')))
    }
  }
  
  
  # ------------------------ #
  # --- Returning Output --- #
  # ------------------------ #
  return(dataset)
}