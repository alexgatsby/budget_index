# config ------------

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}
if (require (polycor) == FALSE) {install.packages("polycor"); require (polycor)}
if (require (openxlsx) == FALSE) {install.packages("openxlsx"); require (openxlsx)}
if (require (ggrepel) == FALSE) {install.packages("ggrepel"); require (ggrepel)}
if (require (memisc) == FALSE) {install.packages("memisc"); require (memisc)}
if (require (corrplot) == FALSE) {install.packages("corrplot"); require (corrplot)}

rm(list = ls())
gc()

# data -------

# PIB a valores correntes, em milhões de reais

pib <- import ('data/tabela6784.xlsx', skip=3)

pib <- pib %>%
  dplyr::select (-c(`...1`)) %>%
  pivot_longer(`1996`:`2021`) %>%
  filter (value!=is.na(value)) %>%
  dplyr::rename ('ano'=name,
          'pib'=value)

# restos a pagar a valores correntes, em milhões de reais

rp <- import ('data/Restos-a-Pagar-da-Uniao.csv', encoding='Latin-1')

colnames (rp) <- c('ano', 'nptotal', 'nppessoal', 'npjurosenc', 'npoutras', 
                   'npinvest', 'npinvers', 'npamort', 'ptotal', 'ppessoal', 
                   'pjurosenc', 'poutras', 'pinvest', 'pinvers', 'pamort')

rp <- rp %>%
  mutate (rptotal=nptotal+ptotal) %>%
  dplyr::select (ano, rptotal) %>%
  mutate (ano=as.character(ano))

# juntando

db <- full_join(pib, rp)

glimpse (db)

db$perc_rap_pib <- db$rptotal/db$pib

# gráficos -------

db %>%
  ggplot (aes(x=ano)) +
  geom_line (aes(y=pib, group=1)) +
  geom_line (aes(y=rptotal, group=1))

db %>%
  ggplot (aes(x=ano, y=perc_rap_pib*100)) +
  geom_line (aes(group=1))

summary(db$perc_rap_pib*100)
View (db)

