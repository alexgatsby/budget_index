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

db <- import (here ('data/rps-ano.xlsx'))

glimpse (db)

cols <- c('ano', 'rp', 'tipo_autor', 'dot_inicial', 'dot_atual', 'empenhado', 'liquidado', 'pago')

colnames(db) <- cols

db <- db %>% filter (ano != 'Total') %>% mutate (ano=as.numeric(ano))

db <- db %>%
  group_by(rp, ano) %>%
  summarize(sum_pago=sum(pago),
            sum_dotatual=sum(dot_atual)) %>%
  as_tibble()

#graph


db %>%
  filter (ano != 2024) %>%
  ggplot (aes(x=as.character(ano))) +
  geom_linerange (aes(ymax=sum_dotatual, ymin=0, color=rp),
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point (aes(y=sum_dotatual, color=rp), size=2,
                  position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_col (aes(y=sum_pago, fill=rp),
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

# checando notícia
#https://www.cnnbrasil.com.br/politica/governo-lula-mais-que-dobrou-empenho-de-emendas-parlamentares-na-comparacao-com-ultimo-ano-de-bolsonaro/#:~:text=Orçamento%20secreto&text=Agora%2C%20os%20recursos%20seguem%20sendo,para%20R%245%2C3%20bilhões.

db <- import (here ('data/rps-ano.xlsx'))

glimpse (db)

db %>%
  group_by(Ano) %>%
  summarise(pago=sum(Pago),
            dotado=sum(`Dotação Atual Emenda`),
            empenhado=sum(Empenhado))

db %>%
  filter (RP=='6 - Emendas Individuais') %>%
  group_by(Ano) %>%
  summarise(pago=sum(Pago),
            dotado=sum(`Dotação Atual Emenda`),
            empenhado=sum(Empenhado))

