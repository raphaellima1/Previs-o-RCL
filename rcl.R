library(dplyr)
pacman::p_load(dplyr, ggplot2,readxl,forecast,xgboost,tidyr,xts,TSstudio, lubrida )
library(RREORGFdataR)

rcl_df <- RREOdata(cod.ibge = 52, year = c(2015:2020), period = 6, annex = 3) 

rcl_df <- rcl_df %>%
  mutate(mes = case_when(coluna == '<MR-11>'~'01',
                         coluna == '<MR-10>'~'02',
                         coluna == '<MR-9>'~'03',
                         coluna == '<MR-8>'~'04',
                         coluna == '<MR-7>'~'05',
                         coluna == '<MR-6>'~'06',
                         coluna == '<MR-5>'~'07',
                         coluna == '<MR-4>'~'08',
                         coluna == '<MR-3>'~'09',
                         coluna == '<MR-2>'~'10',
                         coluna == '<MR-1>'~'11',
                         coluna == '<MR>'~'12'),
         valor = as.numeric(valor)) %>% 
  filter(conta == 'RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>% 
  select(exercicio, coluna, conta, valor, mes) %>% 
  mutate(Data = ymd(paste0(exercicio, mes, "01"))) %>% 
  drop_na(mes) %>% 
  select(Data, conta, valor) 
  
  
writexl::write_xlsx(rcl_df, 'RCL2015_a_2020.xlsx')  
