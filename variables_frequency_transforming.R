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
cumulative_transform = function(transform_type, frequency, dataset, groupby_variables = NULL, change_date_format = FALSE){
  
  # ---------------------------- #
  # --- Transform Type - Sum --- #
  # ---------------------------- #
  
  # Ao utilizar a funcao floor_date multiplos registros de uma mesma frequencia sao gerados. Por exemplo, se os dados sao de 
  # frequencia bimestral, a regra ira gerar a soma referente a jan-fev replicando para a data de janeiro e de fevereiro. Assim,
  # o uso da funcao unique mantem apenas um dos registros replicados.
  
  if(transform_type %in% list('soma', 'sum') ){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = 
        dataset
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = unique(
        dataset %>% 
        mutate(data = floor_date(data, unit = 'bimonth')) %>%
        group_by(data, across(all_of(groupby_variables))) %>%
        mutate(across(where(is.numeric), sum))) %>%
        ungroup()
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(data, unit = 'quarter')) %>%
        group_by(data, across(all_of(groupby_variables))) %>%
        mutate(across(where(is.numeric), sum)) %>%
        ungroup()
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(data, unit = 'halfyear')) %>%
        group_by(data, across(all_of(groupby_variables))) %>%
        mutate(across(where(is.numeric), sum)) %>%
        ungroup()
    }
  }
  
  
  # ----------------------------- #  
  # --- Transform Type - Mean --- #
  # ----------------------------- #
  
  # Ao utilizar a funcao floor_date multiplos registros de uma mesma frequencia sao gerados. Por exemplo, se os dados sao de 
  # frequencia bimestral, a regra ira gerar a media referente a jan-fev replicando para a data de janeiro e de fevereiro. Assim,
  # o uso da funcao unique mantem apenas um dos registros replicados.
  
  else if(transform_type %in% list('media', 'mean')){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = unique(
        dataset %>% 
          mutate(data = floor_date(x = data, unit = 'month')) %>%
          group_by(data, across(all_of(groupby_variables))) %>%
          mutate(across(where(is.numeric), mean))) %>%
          ungroup()
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = unique(
        dataset %>% 
          mutate(data = floor_date(x = data, unit = 'bimonth')) %>%
          group_by(data, across(all_of(groupby_variables))) %>%
          mutate(across(where(is.numeric), mean))) %>%
          ungroup()
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = unique(
        dataset %>%
          mutate(data = floor_date(x = data, unit = 'quarter')) %>%
          group_by(data, across(all_of(groupby_variables))) %>%
          mutate(across(where(is.numeric), mean))) %>%
          ungroup()
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = unique(
        dataset %>%
          mutate(data = floor_date(x = data, unit = 'halfyear')) %>%
          group_by(data, across(all_of(groupby_variables))) %>%
          mutate(across(where(is.numeric), mean))) %>%
          ungroup()
    }
  }
  
  
  # ------------------------------------ #  
  # --- Transform Type - Last Period --- #
  # ------------------------------------ #
  
  # Nessa regra nao ha segredo. O filtro de selecao dos periodos referentes a frequencia temporal desejada e o suficiente.
  
  else if(transform_type %in% list('periodo_final', 'final_period')){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = 
        dataset %>% ungroup()
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = 
        dataset %>%
        filter(month(data) %% 2 == 0) %>%
        ungroup()
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = 
        dataset %>% 
        filter(month(data) %% 3 == 0) %>%
        ungroup()
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = 
        dataset %>% 
        filter(month(data) %% 6 == 0) %>%
        ungroup()
    }
  }
  
  
  # ----------------------------------- #  
  # --- Transform Type - Cumulative --- #
  # ----------------------------------- #
  
  # A regra de transformacao aplicada gera registros da mesma frequência, em razao do uso do floor_date com uma pequena
  # diferença: a agregacao agora e representa a agregacao em cada momento do tempo. Se por exemplo, voce tenha dados mensais
  # e deseja uma serie trimestral a regra ira gerar uma taxa acumulada referente a cada um dos meses do trimestre. Em outras
  # palavras voce teria uma taxa para janeiro, outra para o acumulado jan-fev e uma terceira referente a jan-fev-mar. Assim,
  # o uso da funcao unique se torna inutil nesse cenário. Como o valor que efetivamente refletira a acumulacao correta e o 
  # ultimo registro entao a funcao slice_tail e utilizada.
  
  else if(transform_type %in% list('tx_acumulada', 'cumulative_rate')){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(x = data, unit = 'month')) %>%
        group_by(data, across(all_of(groupby_variables))) %>%
        mutate(across(where(is.numeric), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(x = data, unit = 'bimonth')) %>%
        group_by(data, across(all_of(groupby_variables))) %>%
        mutate(across(where(is.numeric), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = 
        dataset %>%
        mutate(data = floor_date(x = data, unit = 'quarter')) %>%
        group_by(data, across(all_of(groupby_variables))) %>%
        mutate(across(where(is.numeric), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = 
        dataset %>%
        mutate(data = floor_date(x = data, unit = 'halfyear')) %>%
        group_by(data, across(all_of(groupby_variables))) %>%
        mutate(across(where(is.numeric), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
  }
  
  
  # ------------------------------ #  
  # --- Transform Type - Diff  --- #
  # ------------------------------ #
  
  # Diferentemente dos casos anteriores, nao ha necessidade de utilizar a funcao unique ou slice_tail em razao do filtro de 
  # periodo implementado. Como nao ha mais possibilidade de haver registro de datas duplicadas o agrupamento diz respeito a
  # somente as variaveis de agrupamento especificadas no argumento groupby_variables.
  
  else if(transform_type %in% list('diff_acumulado', 'cumulative_diff')){
    
    if(frequency %in% list('mensal', 'monthly')){
      dataset = 
        dataset %>% 
          group_by(across(all_of(groupby_variables))) %>%
          mutate(across(where(is.numeric), ~ ifelse(month(data) == 1, .x, .x - lag(.x)))) %>%
          ungroup()
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset =
        dataset %>% 
          filter(month(data) %% 2 == 0) %>%
          group_by(across(all_of(groupby_variables))) %>%
          mutate(across(where(is.numeric), ~ ifelse(month(data) == 2, .x, .x - lag(.x)))) %>%
          ungroup()
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = 
        dataset %>% 
          filter(month(data) %% 3 == 0) %>%
          group_by(across(all_of(groupby_variables))) %>%
          mutate(across(where(is.numeric), ~ ifelse(month(data) == 3, .x, .x - lag(.x)))) %>%
          ungroup()
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = 
        dataset %>% 
          filter(month(data) %% 6 == 0) %>%
          group_by(across(all_of(groupby_variables))) %>%
          mutate(across(where(is.numeric), ~ ifelse(month(data) == 6, .x, .x - lag(.x)))) %>%
          ungroup()
    }
  }
  
  
  # --------------------------------------------- #
  # --- Adjust Date Column to Match Frequency --- #
  # --------------------------------------------- #
  if(change_date_format == FALSE){
    if(frequency %in% list('mensal','monthly')){
      next
    }
    
    else if(frequency %in% list('bimestral', 'bimonthly')){
      dataset = dataset %>% mutate(data = as.Date(
                                     paste0(
                                            year(data), 
                                            case_when(month(data) %in% c(1,2) ~ '-02-01', 
                                                      month(data) %in% c(3,4) ~ '-04-01',
                                                      month(data) %in% c(5,6) ~ '-06-01',
                                                      month(data) %in% c(7,8) ~ '-08-01',
                                                      month(data) %in% c(9,10) ~ '-10-01',
                                                      .default = '-12-01')),
                                     tryFormats = c('%Y-%m-%d')))
    }
    
    else if(frequency %in% list('trimestral', 'quartely')){
      dataset = dataset %>% mutate(data = as.Date(
                                     paste0(
                                       year(data), 
                                       case_when(month(data) %in% c(1,2,3) ~ '-03-01', 
                                                 month(data) %in% c(4,5,6) ~ '-06-01',
                                                 month(data) %in% c(7,8,9) ~ '-09-01',
                                                 .default = '12-01')),
                                     tryFormats = c('%Y-%m-%d')))
                                     
    }
    
    else if(frequency %in% list('semestral', 'halfyear')){
      dataset = dataset %>% mutate(data = as.Date(
                                     paste0(
                                       year(data), 
                                       case_when(month(data) - 6 >= 1 ~ '06-01', 
                                                 .default = '-12-01')),
                                     tryFormats = c('%Y-%m-%d')))
    }
  } else{
    
    if(change_date_format == TRUE){
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
  }
  
  
  # ------------------------ #
  # --- Returning Output --- #
  # ------------------------ #
  return(dataset)
}