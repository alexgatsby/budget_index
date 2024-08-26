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

db <- import (here ('data/rps_ano_novo.xlsx'))

colunas <- c('year', 'rp', 'proj_lei', 'dot_inicial', 
             'dot_atual', 'empenhado', 'liquidado', 'pago')
colnames(db) <- colunas

db <- db %>% filter (year != 'Total') %>% mutate (year=as.numeric(year))


db$rp_simples <- ifelse (str_detect(db$rp, '6 -'), 'RP-6 - Emendas Individuais',
                 ifelse (str_detect(db$rp, '7 -'), 'RP-7 - Emendas de Bancada Estadual',
                         ifelse (str_detect(db$rp, '8 -'), 'RP-8 - Emendas de Comissão',
                                 'RP-9 - Emendas de Relator-Geral')))

glimpse (db)
db %>% count (rp_simples, rp)
db %>% count (rp_simples, year)

#graph

db %>%
  drop_na () %>%
  filter (year != 2024) %>%
  ggplot (aes(x=as.character(year))) +
  geom_linerange (aes(ymax=dot_atual, ymin=0, color=rp_simples),
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point (aes(y=dot_atual, color=rp_simples), size=2,
                  position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_col (aes(y=pago, fill=rp_simples),
            position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=c('black', 'lightblue', '#999999', '#ef8a62'))+
  scale_color_manual(values=c('black', 'lightblue', '#999999', '#ef8a62'))+
  #coord_flip() +
  scale_y_continuous(labels = c('0bi', '5bi', '10bi', '15bi', '20bi')) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (title='Valores de Emendas Parlamentares Dotados e Pagos por Ano e Resultado Primário',
        subtitle = 'Fonte: SIOP.',
        caption='Elaboração própria.',
        fill=NULL,
        y=NULL,
        x=NULL,
        color=NULL)

db %>% filter (year==2014)
