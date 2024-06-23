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

db <- import (here ('data/solicitacoes.xlsx'))

inicio_2020 <- '01/01/2020' %>% as.Date('%d/%m/%Y')
fim_2022 <- '31/12/2022' %>% as.Date('%d/%m/%Y')

# cleaning

colunas <- c('n', 'ano', 'data_cadastro', 'autor', 'tipo_autor', 
             'cnpj_benf', 'nome_benef', 'uf_benef', 
             'cod_orgao', 'nome_orgao', 'cod_uo', 'nome_uo',
             'cod_acao', 'n_proposta', 'indicador_fnde', 'n_cnes', 
             'tipo_objeto', 'objeto', 'justificativa', 'gnd', 
             'tipo_execucao', 'ma', 'valor_solicitado', 'situacao')

colnames(db) <- colunas

# figura 3: quantidade e percentual de pedidos por tipo de usuário

db %>%
  filter (data_cadastro <= fim_2022 &
            data_cadastro >= inicio_2020) %>%
  group_by(tipo_autor, situacao) %>%
  summarise(quant=n(),
            valortotal=sum(valor_solicitado)) %>%
  as_tibble() %>%
  mutate (perc=100*quant/sum(quant)) %>%
  ggplot (aes(x=tipo_autor)) +
  geom_col (aes(y=quant, fill=situacao), position = position_dodge()) +
  geom_text(aes (y=100,
                 label=paste(prettyNum(quant, big.mark = '.', decimal.mark = ','),
                             'solicitações |',
                             round (perc, 2),
                             '%'),
                 group=situacao),
            position = position_dodge(width=0.9),
            hjust=0,
            color='black')+
  scale_fill_manual(values = c('#999999', '#ef8a62')) +
  coord_flip() +
  theme_bw() +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y='Quantidade de Solicitações',
        fill='Status das Solicitações',
        title='Quantidade e Percentual de Pedidos por Tipo de Usuário',
        subtitle = 'Fonte: SINDORC. Dados extraídos em 07/01/2024.',
        caption = 'O gráfico compreende todas as solicitações cadastradas entre 01/01/2020 e 31/12/2022.')

###no paint: 4.697 solicitações | 7.53% + 1.156 solicitações | 2.41 % + 4.160 solicitações | 6.67%


# figura 4: valur vs perc

db %>%
  filter (data_cadastro <= fim_2022 &
            data_cadastro >= inicio_2020) %>%
  group_by(tipo_autor, situacao) %>%
  summarise(quant=n(),
            valortotal=sum(valor_solicitado)) %>%
  as_tibble() %>%
  mutate (perc=100*quant/sum(quant),
          perc_valor=100*valortotal/sum(valortotal)) %>%
  ggplot (aes(x=tipo_autor)) +
  geom_col (aes(y=valortotal, fill=situacao), position = position_dodge()) +
  geom_text(aes (y=10000,
                 label=paste('R$',
                             prettyNum(valortotal, big.mark = '.', decimal.mark = ','),
                             '|',
                             round (perc_valor, 2),
                             '%'),
                 group=situacao),
            position = position_dodge(width=0.9),
            hjust=0,
            color='black')+
  scale_fill_manual(values = c('#999999', '#ef8a62')) +
  scale_y_continuous(labels = comma, n.breaks=6) +
  coord_flip() +
  theme_bw() +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y='Valor Solicitado',
        fill='Status das Solicitações',
        title='Valor e Percentual dos Pedidos por Tipo de Usuário',
        subtitle = 'Fonte: SINDORC. Dados extraídos em 07/01/2024.',
        caption = 'O gráfico compreende todas as solicitações cadastradas entre 01/01/2020 e 31/12/2022.')


#figura 5: data e quant de solicitações

db %>%
  filter (data_cadastro <= fim_2022 &
            data_cadastro >= inicio_2020) %>%
  ggplot (aes(x=data_cadastro)) +
  geom_histogram(aes(fill=tipo_autor)) +
  scale_fill_manual(values = c('#999999', '#ef8a62', '#404040')) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%m/%y') +
  theme_bw() +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y=NULL,
        fill='Tipo de Usuário',
        title='Quantidade de Solicitações por Data e Tipo de Usuário',
        subtitle = 'Fonte: SINDORC. Dados extraídos em 07/01/2024.',
        caption = 'O gráfico compreende todas as solicitações cadastradas entre 01/01/2020 e 31/12/2022.')

db %>%
  filter (data_cadastro <= fim_2022 &
            data_cadastro >= inicio_2020) %>%
  summary()
