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
  if(change_date == TRUE){
    if(frequency %in% list('mensal','monthly')){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) == 1 ~ '_M01',
                                         month(data) == 2 ~ '_M02',
                                         month(data) == 3 ~ '_M03',
                                         month(data) == 4 ~ '_M04',
                                         month(data) == 5 ~ '_M05',
                                         month(data) == 6 ~ '_M06',
                                         month(data) == 7 ~ '_M07',
                                         month(data) == 8 ~ '_M08',
                                         month(data) == 9 ~ '_M09',
                                         month(data) == 10 ~ '_M10',
                                         month(data) == 11 ~ '_M11',
                                         .default = '_M12')))
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) %in% c(1,2) ~ '_B1', 
                                         month(data) %in% c(3,4) ~ '_B2',
                                         month(data) %in% c(5,6) ~ '_B3',
                                         month(data) %in% c(7,8) ~ '_B4',
                                         month(data) %in% c(9,10) ~ '_B5',
                                         .default = '_B6')))
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) %in% c(1,2,3) ~ '_Q1', 
                                         month(data) %in% c(4,5,6) ~ '_Q2',
                                         month(data) %in% c(7,8,9) ~ '_Q3',
                                         .default = '_Q4')))
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) - 6 >= 1 ~ '_H1', 
                                         .default = '_H2')))
    }
  }
  
  
  # ------------------------ #
  # --- Returning Output --- #
  # ------------------------ #
  return(dataset)
}