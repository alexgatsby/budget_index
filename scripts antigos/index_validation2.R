# config

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}
if (require (polycor) == FALSE) {install.packages("polycor"); require (polycor)}
if (require (corrplot) == FALSE) {install.packages("corrplot"); require (corrplot)}


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

db$ano <- as.character(db$ano)

# data orçamento

dt <- import (here ('data/siga_brasil_emendas_tipo.xlsx'))

colunas <- c('tipo_autor','ano', 'dot', 'pag')

colnames(dt) <- colunas

dt <- dt %>%
  group_by(ano) %>%
  summarise(dot=sum(dot),
            pag=sum(pag)) %>%
  mutate (tax_execution = 100*pag/dot)

# juntando

total <- full_join(db, dt, by='ano')

# gráfico

total %>%
  drop_na() %>%
  ggplot () +
  geom_point(aes(x=index_ano, y=tax_execution)) +
  geom_smooth(aes(x=index_ano, y=tax_execution))

total$c1 <- total$c1 %>%
  str_replace_all(',', '.') %>%
  as.numeric()

total$c2 <- total$c2 %>%
  str_replace_all(',', '.') %>%
  as.numeric()

total$c3 <- total$c3 %>%
  str_replace_all(',', '.') %>%
  as.numeric()

total$c4 <- total$c4 %>%
  str_replace_all(',', '.') %>%
  as.numeric()

total$c5 <- total$c5 %>%
  str_replace_all(',', '.') %>%
  as.numeric()

total$c6 <- total$c6 %>%
  str_replace_all(',', '.') %>%
  as.numeric()

total2 <- total %>% drop_na ()

total3 <- total %>% select(-c(ano, dot, pag)) %>% drop_na()

cor_cont <- cor(total3)

corrplot(cor_cont)

total %>%
  select (ano, index_ano, tax_execution) %>%
  mutate (index_ano=10*index_ano) %>%
  data.table::melt (id.vars='ano') %>%
  ggplot (aes(x=as.numeric(ano))) +
  geom_line (aes(y=value, group=variable)) +
  geom_point (aes(y=value, shape=variable))


# correlation

cor(total2$index_ano, total2$tax_execution) #0,797

cor.test(total2$index_ano, total2$tax_execution, 
         method='spearman', data=total2) #0,780
