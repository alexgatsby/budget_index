# config

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}

rm(list = ls())
gc()

# data

db <- import (here ('data/indexv3total.csv'))

# cleaning

colunas <- c ('ano', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'index_ano')

colnames(db) <- colunas

db <- db %>% filter (ano != is.na(ano))

db$index_ano <- db$index_ano %>%
  str_replace_all(',', '.') %>%
  as.numeric()

dblong <- db %>% 
  data.table::melt (id.vars='ano', variable.name='criterio')

dblong$value <- dblong$value %>%
  str_replace_all(',', '.') %>%
  as.numeric()


# graph

db %>%
  ggplot () +
  geom_line (aes (x=ano, y=index_ano)) +
  geom_point (aes (x=ano, y=index_ano),
              size=2) +
  geom_text (aes (x=ano, y=index_ano,
                  label=round(index_ano, 2)),
             angle=90,
             hjust=1.5) +
  scale_y_continuous(limits = c(0,10)) +
  scale_x_continuous(n.breaks=2024-1990) +
  theme_bw() +
  theme (axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (title='Índice de Poder Orçamentário do Legislativo',
        subtitle='Metodologia: médias aritméticas.',
        caption='\n\nElaboração Própria.\n',
        x=NULL,
        y=NULL)


# graph - critérios

dblong %>%
  ggplot () +
  geom_line (aes (x=ano, y=value, group=criterio),
             size=1) +
  geom_point (aes (x=ano, y=value, shape=criterio),
             size=2, alpha=0.6) +
  scale_y_continuous(limits = c(0,10)) +
  scale_x_continuous(n.breaks=2024-1990) +
  scale_shape_manual(labels=c('Critério 1', 'Critério 2', 'Critério 3', 'Critério 4', 'Critério 5', 'Critério 6', 'Índice Anual'),
                       values=c(1,2,4,5,8,0,NA)) +
  theme_bw() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (title='Critérios do Índice de Poder Orçamentário do Legislativo',
        subtitle='Metodologia: médias aritméticas.',
        caption='\n\nElaboração Própria.\n',
        x=NULL,
        y=NULL,
        shape='Critério')

