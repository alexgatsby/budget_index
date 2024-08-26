# config

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}

rm(list = ls())
gc()

# data index

db <- import (here ('data/indexv3total.csv'))

colunas <- c ('ano', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'index_ano')

colnames(db) <- colunas

db <- db %>% filter (ano != is.na(ano))

db$index_ano <- db$index_ano %>%
  str_replace_all(',', '.') %>%
  as.numeric()

# data orçamento

dt <- import (here ('data/siga_brasil_poder_rp_desde2010.csv'))

colunas <- c('ano', 'rp1', 'cod_rp1', 'funcao',
             'cod_rp2', 'rp2', 'cod_rp3', 'rp3',
             'cod_rp4', 'rp4',
             'poder', 'setor', 'tipo_emenda', 
             'dot_inicial_atualizada', 'val_pago_atualizado')

colnames(dt) <- colunas

ponto <- dt[2,15] %>% str_sub (start=4, end=4)
virgula <- dt[1,14] %>% str_sub (start=2, end=2)

dt$dot <- dt$dot_inicial_atualizada %>%
  str_replace_all(virgula, 'oi') %>%
  str_remove_all('[[:punct:]]') %>%
  str_replace_all('oi', '.') %>%
  as.numeric()

dt$pag <- dt$val_pago_atualizado %>%
  str_replace_all(virgula, 'oi') %>%
  str_remove_all('[[:punct:]]') %>%
  str_replace_all('oi', '.') %>%
  as.numeric()


## verificando integridade dos gastos do legislativo

### a variável "poder" não designa as emendas orçamentárias do 
### legislativo antes da inauguração dos RPs

dt %>% count (cod_rp1) # não é esse

dt %>% count (cod_rp2) # pode ser esse

dt %>%
  filter (ano >= 2020) %>%
  filter (cod_rp2 %in% c("6", "7", "8", "9")) %>%
  count (poder) # nao designa o legislativo

dt %>% count (cod_rp3) # pode ser esse

dt %>%
  filter (ano >= 2020) %>%
  filter (cod_rp3 %in% c("6", "7", "8", "9")) %>%
  count (poder) # nao designa o legislativo

dt %>% count (cod_rp4) # não é esse

### e a variável setor?

dt %>% 
  filter (poder=='PODER LEGISLATIVO') %>%
  count (setor)

### e a variável rp?

dt %>%
  count (rp3, poder)

### e a variável tipo_emenda?

dt %>%
  filter (ano < 2014) %>%
  count (tipo_emenda)


# fazendo por rp e poder

dt1 <- dt %>%
  filter (ano < 2014 |
            poder == 'PODER LEGISLATIVO')

dt3 <- dt %>%
  filter (ano == 2015 |
            ano == 2016) %>%
  filter (cod_rp3 %in% c('6') |
            poder == 'PODER LEGISLATIVO')

dt4 <- dt %>%
  filter (ano >= 2017 &
            ano < 2020) %>%
  filter (cod_rp3 %in% c ('6', '7') |
            poder == 'PODER LEGISLATIVO')

dt5 <- dt %>%
  filter (ano >= 2020) %>%
  filter (cod_rp3 %in% c ('6', '7', '8', '9'))

db_gastos_leg <- bind_rows(dt1, dt2, dt3, dt4, dt5)

db_gastos_leg$dot_ok <- ifelse (db_gastos_leg$dot == 0, 1, db_gastos_leg$dot)

db_gastos_leg %>%
  group_by(ano) %>%
  summarise (dot=sum(dot),
             pag=sum(pag)) %>%
  mutate (taxa_execucao = 100*pag/dot) %>% View ()

# fazendo por rp e ignorando as enmendas sem rp

dt1 <- dt %>%
  filter (ano >= 2014 &
            cod_rp3 == '6')

dt2 <- dt %>%
  filter (ano >= 2017 &
            cod_rp3 == '7')

dt3 <- dt %>%
  filter (ano >= 2020 &
            cod_rp3 == '8')

dt4 <- dt %>%
  filter (ano >= 2020 &
            cod_rp3 == '9')


db_gastos_leg <- bind_rows(dt1, dt2, dt3, dt4)

db_gastos_leg %>%
  group_by(ano) %>%
  summarise (dot=sum(dot),
             pag=sum(pag)) %>%
  mutate (taxa_execucao = 100*pag/dot) %>% View ()
