#config

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}
if (require (corrplot) == FALSE) {install.packages("corrplot"); require (corrplot)}
if (require (knitr) == FALSE) {install.packages("knitr"); require (knitr)}

rm(list = ls())
gc()

# data

db <- import (here ('data/index v3 - Summary.csv'))

db <- db %>% rename ('ano'=V1)

db$q1 <- db$q1 %>%
  str_replace_all(',', 'oi') %>%
  str_remove_all('[[:punct:]]') %>%
  str_replace_all('oi', '.') %>%
  as.numeric()

db$q5 <- db$q5 %>%
  str_replace_all(',', 'oi') %>%
  str_remove_all('[[:punct:]]') %>%
  str_replace_all('oi', '.') %>%
  as.numeric()

db$q7 <- db$q7 %>%
  str_replace_all(',', 'oi') %>%
  str_remove_all('[[:punct:]]') %>%
  str_replace_all('oi', '.') %>%
  as.numeric()

db$q8 <- db$q8 %>%
  str_replace_all(',', 'oi') %>%
  str_remove_all('[[:punct:]]') %>%
  str_replace_all('oi', '.') %>%
  as.numeric()

db$q10 <- db$q10 %>%
  str_replace_all(',', 'oi') %>%
  str_remove_all('[[:punct:]]') %>%
  str_replace_all('oi', '.') %>%
  as.numeric()

db$q13 <- db$q13 %>%
  str_replace_all(',', 'oi') %>%
  str_remove_all('[[:punct:]]') %>%
  str_replace_all('oi', '.') %>%
  as.numeric()

# indexes per methodology

## m1 - means

db$c1 <- (db$q1 + db$q2)/2
db$c2 <- (db$q3 + db$q4)/2
db$c3 <- (db$q5)
db$c4 <- (db$q6 + db$q7 + db$q8)/3
db$c5 <- (db$q9 + db$q10 + db$q11)/3
db$c6 <- (db$q12 + db$q13)/2

db$i1 <- (db$c1 + db$c2 + db$c3 + db$c4 + db$c5 + db$c6)/6

## m2 - sums, j=1

db <- db %>%
  mutate (i2 = q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13)

## m3 - sums, j=0.5

j=.5

db <- db %>%
  mutate (i3 = q1^j+q2^j+q3^j+q4^j+q5^j+q6^j+q7^j+
            q8^j+q9^j+q10^j+q11^j+q12^j+q13^j)


## m4 - sums, j=0,5

j=2

db <- db %>%
  mutate (i4 = q1^j+q2^j+q3^j+q4^j+q5^j+q6^j+q7^j+
            q8^j+q9^j+q10^j+q11^j+q12^j+q13^j)


## m5 - subindexes

db$sub1 <- (db$q1 + db$q2 + db$q3 + db$q4 + db$q12 + db$q13)
db$sub2 <- (db$q5 + db$q6 + db$q7 + db$q8 + db$q9 + db$q10 + db$q11)

db$i5 <- db$sub1*db$sub2


# spearman ranking correlations

dbcor <- db %>% select (i1, i2, i3, i4, i5)

cor (dbcor, method='spearman') %>% corrplot()

cor (dbcor, method='spearman') %>% 
  kable () %>% 
  kableExtra::kable_styling()

glimpse (dbcor)

# index plot

qest <- paste0('q', 1:13)

db %>%
  select (ano, q1:q13, i5) %>%
  pivot_longer(q1:i5) %>%
  filter (name!='i5') %>%
  ggplot (aes(x=ano, y=value)) +
  geom_line (aes(group=factor(name, qest)), alpha=.5, color='black', size=1) +
  geom_point (aes(group=factor(name, qest), shape=factor(name, qest)),
              size=2, alpha=.8) +
  scale_shape_manual(values=3:20) +
  scale_x_continuous(limits=c(1990,2023), n.breaks = length(1990:2023))+
  theme_bw () +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y=NULL,
        title='Questões e Pontuação Anual',
        subtitle='Elaboração própria.',
        shape='Questão')


# index plot


db %>%
  mutate (ifinal = i5/42) %>%
  select (ano, q1:q13, ifinal) %>%
  pivot_longer(q1:ifinal) %>%
  filter (name=='ifinal') %>%
  ggplot (aes(x=ano, y=value)) +
  geom_line (color='black', size=1) +
  geom_point (shape=21, size=2) +
  geom_text(aes(label=prettyNum(round(value,2), decimal.mark = ',', big.mark = '.')),
            size=3,
            vjust=-2) +
  scale_x_continuous(limits=c(1990,2023), n.breaks = length(1990:2023))+
  scale_y_continuous(limits=c(0,100))+
  theme_bw () +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y=NULL,
        title='Pontuação Anual do Índice de Poder Orçamentário do Poder Legislativo Brasileiro',
        subtitle='Elaboração própria.')


# validation data

em <- import (here ('data/grafico02.csv')) %>% 
  rename ('ano'=Ano) %>%
  filter (Fase=='Valores pagos (incluindo restos a pagar)')

db <- left_join(db, em, by='ano')

db$ifinal = db$i5/42

dbclean <- db %>% drop_na()
external_validation_cor <- cor (dbclean$ifinal, dbclean$Porcentagem)

db %>%
  ggplot (aes(x=ifinal,y=Porcentagem)) +
  geom_point(alpha=.5) +
  geom_smooth(method = 'lm') +
  theme_bw () +
  theme () +
  labs (x='Índice de Poder Orçamentário do Legislativo',
        y='Emendas Individuais Pagas\n(% da Receita Corrente Líquida)\n',
        title='Relação entre Índice e Execução de Emendas Individuais',
        subtitle='Elaboração própria.',
        caption = paste('Correlação de Pearson =', round(external_validation_cor,3)))
