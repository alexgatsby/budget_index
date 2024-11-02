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


rm(list = ls())
gc()

# funções

numviz <- function (data, dec=2){
  
  i <- data %>%
    round ({dec}) %>%
    prettyNum(decimal.mark = ',', big.mark = '.')
}

numlimpo <- function (n){
  data <- {{n}} %>%
    str_replace_all(',', 'oi') %>%
    str_remove_all('[[:punct:]]') %>%
    str_replace_all('oi', '.') %>%
    as.numeric()
  
  return(data)
}

# dados para a visualização

mypal <- c('#999999', 'lightgrey', '#404040', '#ef8a62', 'lightblue') %>% rep(5)


# valores de emendas por modalidade de execução (fig:dual_rp6) --------------
## dados extraídos em 06/07/2024, do painel TransfereGov (https://clusterqap2.economia.gov.br/extensions/painel-gestao-transferencias/painel-gestao-transferencias.html)

db <- import (here ('884b29a0-5458-4a5a-a628-d7df19f9debd.xlsx'))

glimpse (db)
db$is_epix <- ifelse (db$Modalidade=='ESPECIAL', 1, 0)

colnames (db) <- c('autor', 'nemenda', 'ano', 'ex1', 'ex2', 'sitconv',
                   'obj', 'modalidade', 'natjur', 'cnpj', 'ex3', 'siguf', 
                   'nomemun_siconv', 'ex4', 'ex5', 'ex6', 'ex7', 'ex8', 'total', 
                   'empenhado','liberado', 'ex9', 'ppol', 'ex10', 'is_epix')
db %>% count (is_epix)

bilhoes <- c('0', '1bi', '2bi', '3bi', '4bi', '5bi', '6bi', '7bi')

db %>%
  group_by(ano, is_epix) %>%
  summarize(lib=sum(liberado, na.rm=T)) %>%
  mutate (perc=100*prop.table(lib),
          is_epix=as.character(is_epix),
          ano=as.character(ano)) %>%
  ggplot (aes(x=ano, y=lib)) +
  geom_col (aes(fill=is_epix), 
            position=position_dodge2(preserve='single', width=.9)) +
  geom_text (aes(y=0,
                 label=paste('R$', numviz(lib/1000000), 'milhões',
                             '|', numviz(perc,2), '%'), 
                 group=is_epix),
             angle=90,
             position=position_dodge2(preserve='single', width=.9),
             hjust=-0.01) +
  scale_y_continuous(n.breaks=8, labels=bilhoes) +
  scale_fill_manual(values=mypal, labels=c('Finalidade Definida', 'Especial')) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y=NULL,
        title='Valores de Emendas Individuais Executados por Modalidade de Transferência\n',
        fill='Tipo de\nTransferência',
        caption='Data de atualização: 06/07/2024. Fonte: Painel TransfereGov.')

ggsave ('results/dual_rp6.png', width=8, height = 6)

# quantidade de funcionários do TCU (fig:tcu_funcionarios) --------------------------

db <- import (here ('data/quadro_pessoal_tcu.csv'))


db %>%
  dplyr::select (ano, `total - corpo técnico`) %>%
  dplyr::rename ('nfunc' = `total - corpo técnico`) %>%
  filter (ano >= 2005)%>%
  mutate (nfunc = ifelse (ano%in%2017:2018, NA, nfunc)) %>%
  ggplot (aes(x=reorder(as.character(ano), rev(2005:2022)), y=nfunc)) +
  geom_col (fill=mypal[2]) +
  geom_text (aes (y=0, label=numviz(nfunc)),
             hjust=0,
             size=3) +
  coord_flip() +
  theme_classic() +
  labs (title='Quantidade de funcionários técnicos ocupados do TCU por ano\n',
        x=NULL,
        y='Nº de analistas, técnicos ou auditores federais de controle externo',
        caption='Fonte: relatórios anuais de atividades do TCU.')

ggsave ('results/nfunc_tcu.png', width=8, height = 6)

# valores dotados e pagos por resultado primário (fig:val_dot_pagos) ------------------

db <- import (here ('data/3fbcec80262d4c7e828fb9139e8fa148.csv'))

colunas <- c('year', 'rp', 'dot_atual', 'pago')
colnames(db) <- colunas

db <- db %>% 
  filter (year != 'Total') %>% 
  mutate (year=numlimpo(year))

db$rp_simples <- paste0 ('RP-', str_sub (db$rp, start=1, end=1))

db$pago <- db$pago %>% str_remove_all ('[[:punct:]]') %>% as.numeric()
db$dot_atual <- db$dot_atual  %>% str_remove_all ('[[:punct:]]') %>% as.numeric()

rpleg <- c('RP-6', 'RP-7', 'RP-8', 'RP-9')

labels <- c('0bi', '5bi', '10bi', '15bi', '20bi', '25bi')

db %>%
  drop_na () %>%
  filter (rp_simples %in% rpleg) %>%
  ggplot (aes(x=as.character(year))) +
  geom_linerange (aes(ymax=dot_atual, ymin=0, color=rp_simples),
                  position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point (aes(y=dot_atual, color=rp_simples), size=2,
              position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_col (aes(y=pago, fill=rp_simples),
            position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=mypal)+
  scale_color_manual(values=mypal)+
  #coord_flip() +
  scale_y_continuous(labels=labels) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (title='Valores de Emendas Parlamentares Dotados e Pagos por Ano e Resultado Primário\n',
        caption='Data de extração: 09/08/2024. Valores corrigidos automaticamente pelo sistema. Fonte: SIOP.',
        fill=NULL,
        y=NULL,
        x=NULL,
        color=NULL)

ggsave ('results/rps_dot_pagos.png', width=8, height = 5)


# quantidade de solicitações de transferências no sindorc por autor (fig:sindor_quant)------------

db <- import (here ('data/solicitacoes.xlsx'))

inicio_2020 <- '01/01/2020' %>% as.Date('%d/%m/%Y')
fim_2022 <- '31/12/2022' %>% as.Date('%d/%m/%Y')

colunas <- c('n', 'ano', 'data_cadastro', 'autor', 'tipo_autor', 
             'cnpj_benf', 'nome_benef', 'uf_benef', 
             'cod_orgao', 'nome_orgao', 'cod_uo', 'nome_uo',
             'cod_acao', 'n_proposta', 'indicador_fnde', 'n_cnes', 
             'tipo_objeto', 'objeto', 'justificativa', 'gnd', 
             'tipo_execucao', 'ma', 'valor_solicitado', 'situacao')

colnames(db) <- colunas

mil <- c('0', '5 mil', '10 mil', '15 mil', '20 mil', '25 mil')

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
  scale_fill_manual(values = mypal) +
  scale_y_continuous(labels=mil)+
  coord_flip() +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y=NULL,
        fill='Status das Solicitações',
        title='Quantidade e Percentual de Pedidos por Tipo de Usuário e Status',
        caption = 'Fonte: SINDORC. Data de extração: 07/01/2024. O gráfico compreende todas as solicitações cadastradas entre 01/01/2020 e 31/12/2022.')

ggsave ('results/sindorc_quant_pedidos_perc_tipousu.png', width=8, height = 5)
  

# valores das solicitações de transferências no sindorc por autor (fig:sindor_valor)------------

bilhoes <- c('0', '20 bi', '40 bi', '60 bi', '80 bi', '100 bi')

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
  scale_fill_manual(values = mypal) +
  scale_y_continuous(labels = bilhoes, n.breaks=6) +
  coord_flip() +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y='Valor Solicitado',
        fill='Status das Solicitações',
        title='Valor e Percentual dos Pedidos por Tipo de Usuário',
        caption = 'Fonte: SINDORC. Data de extração: 07/01/2024. O gráfico compreende todas as solicitações cadastradas entre 01/01/2020 e 31/12/2022.')

ggsave ('results/sindorc_valor_pedidos_tipousu.png', width=8, height = 5)

# quantidade de pedidos por no SINDORC por data e tipo de usuário (fig:sindorc_data) -------------------

mil <- c('0', '3 mil', '6 mil', '9 mil', '12 mil')

db %>%
  filter (data_cadastro <= fim_2022 &
            data_cadastro >= inicio_2020) %>%
  ggplot (aes(x=data_cadastro)) +
  geom_histogram(aes(fill=tipo_autor)) +
  scale_fill_manual(values = mypal) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%m/%y') +
  scale_y_continuous(labels = mil) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y=NULL,
        fill='Tipo de Usuário',
        title='Quantidade de Solicitações por Data e Tipo de Usuário',
        caption = 'Fonte: SINDORC. Data de extração: 07/01/2024. O gráfico compreende todas as solicitações cadastradas entre 01/01/2020 e 31/12/2022.')

ggsave ('results/sindorc_quant_data_tipousu.png', width=8, height = 5)
