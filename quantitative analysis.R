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

`%ni%` <- Negate(`%in%`)

# dados para a visualização

mypal <- c('#999999', 'lightgrey', '#404040', '#ef8a62', 'lightblue') %>% rep(5)

myshapes <- c(0,1,2,4,8,10) %>% rep(3)

# indexes data -----------

db <- import (here ('data/index - final - Data.csv'))

meta <- import (here ('data/index - final - Metadata - English.csv'))

db$score <- db$score %>% numlimpo ()

glimpse (db)

# cav index --------

db$is_cav <- ifelse (str_detect(db$question_code, "cav_"), 1, 0)

cav <- db %>%
  filter (is_cav == 1) %>%
  left_join(meta, by='question_code')

glimpse (cav)

# methodologies comparision ------------------

## I1

i1 <- cav %>%
  group_by(dimension2,year) %>%
  summarise(media_q=mean(score)) %>%
  group_by(year) %>%
  summarise(score_year_i1=mean(media_q))

## I2

i2 <- cav %>%
  mutate (score_j=score^1)%>%
  group_by(year) %>%
  summarise(score_year_i2=mean(score_j))

## I3

i3 <- cav %>%
  mutate (score_j=score^.5)%>%
  group_by(year) %>%
  summarise(score_year_i3=mean(score_j))

## I4

i4 <- cav %>%
  mutate (score_j=score^2)%>%
  group_by(year) %>%
  summarise(score_year_i4=mean(score_j))

## I5

cols <- c('year', 'cav_subindex2', 'cav_subindex1', 'score_year_i5')

i5 <- cav %>%
  group_by(dimension1, year) %>%
  summarize (score_subindex=sum(score))%>%
  pivot_wider(names_from = dimension1, values_from = score_subindex) %>%
  mutate (score_year_i5=`Capacidade organizacional do Legislativo em relação ao orçamento`*`Poder do Legislativo face ao Executivo`)

colnames(i5) <- cols

glimpse (i5)


# spearman ranking correlations ---------------------

cav_met <- bind_cols(i1, i2, i3, i4, i5%>%dplyr::select(year, score_year_i5)) %>%
  mutate (year=year...1) %>%
  dplyr::select (year,score_year_i1,score_year_i2,score_year_i3,score_year_i4,score_year_i5)


cor (cav_met%>%dplyr::select(-year), method = 'spearman') %>% 
  as_tibble() %>%
  write.xlsx('data/spearman_cav_indexes.xlsx')


# cav index questions visualization --------------

glimpse (cav)
qorder <- cav$question %>% unique()
qorder <- c(qorder[1:3], qorder[5:10], qorder[4])
cav$question <- factor (cav$question, qorder)

cav %>%
  dplyr::select (year, question, score, dimension2, dimension1) %>%
  ggplot (aes(x=year, y=score)) +
  geom_line (aes(group=question, color=dimension1)) +
  geom_point (aes(shape=dimension2, color=dimension1)) +
  scale_color_manual(values=mypal[3:4]) +
  scale_shape_manual(values=myshapes) +
  theme_classic() +
  facet_wrap(vars(question), nrow=2, labeller = label_wrap_gen(width=45))+
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Pontuações do Índice por Ano, Questão e Critério')

ggsave ('results/facetquestions.png', width=15, height=8)

# cav final index visualization ---------------

cav_met$i_final <- (cav_met$score_year_i5*100)/(50*50)

cav_met %>%
  ggplot (aes(x=as.character(year), y=i_final)) +
  geom_line (aes(group=1)) +
  geom_point (color=mypal[1]) +
  geom_text (aes(label=prettyNum(round(i_final,2), 
                                 decimal.mark = ',', big.mark = '.')),
             vjust=-1,
             hjust=-.1,
             size=3,
             angle=45) +
  scale_y_continuous(limits=c(0,100)) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Índice de Poder Orçamentário do Poder Legislativo Brasileiro')

ggsave ('results/cavindex_main.png', width=9, height=5)

# cav subindexes visualization ------------------

glimpse (cav)
glimpse (i5)
glimpse (cav_met)
cav_met <- full_join(cav_met, i5)

labs <- c(cav$dimension1 %>% unique(), 'Índice final')

cav_met %>%
  dplyr::select (year, cav_subindex1, cav_subindex2, i_final) %>%
  pivot_longer(cav_subindex1:i_final) %>%
  ggplot (aes (x=as.character(year), y=value)) +
  geom_line (aes(group=name, color=name)) +
  geom_point (aes(color=name)) +
  scale_color_manual(values=mypal[3:6],
                     labels=labs) +
  scale_y_continuous(n.breaks=10) +
  theme_classic () +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Índice Principal e Subíndices')

ggsave ('results/cavindex_sub.png', width=9, height=5)

# cav index validation - execution tax - simple correlation ----------------

dt <- import (here ('data/rps_ano_novo.xlsx'))

colunas <- c('year', 'rp', 'proj_lei', 'dot_inicial', 
             'dot_atual', 'empenhado', 'liquidado', 'pago')
colnames(dt) <- colunas

glimpse (dt)

db <- dt %>%
  mutate(year=numlimpo(year))%>%
  filter (rp=='6 - Primária discricionária, decorrente de emendas individuais, considerada no cálculo do RP') %>%
  left_join(cav_met%>%dplyr::select(year,i_final),by='year') %>%
  mutate (tax_execution = 100*pago/dot_inicial)

cor_spearman <- db %>% drop_na () %>% dplyr::select (i_final, tax_execution) %>% cor (method = 'spearman')
cor_pearson <- db %>% drop_na () %>% dplyr::select (i_final, tax_execution) %>% cor (method = 'pearson')

db %>%
  ggplot (aes(x=i_final, y=tax_execution)) +
  geom_smooth(color=mypal[3],fill=mypal[1], alpha=.5,
              method='lm', level=.99) +
  geom_point (color=mypal[4], size=5, alpha=.7) +
  geom_text_repel (aes(label=year)) +
  annotate('text', x=32, y=15, label='Intervalo\nde Confiança:\n99%') +
  theme_classic() +
  labs (x='Índice de Poder Orçamentário do Legislativo Brasileiro',
        y='Taxa de Execução das\nEmendas Individuais (RP-6)',
        title='Relação entre Índice e Execução de Emendas Individuais',
        caption = paste('Coeficiente de Correlação de Pearson =',round(cor_pearson[1,2],5),
                        '\nCoeficiente de Correlação de Spearman =',round(cor_spearman[1,2],5),
                        '\nFonte: SIOP. Data de extração: 18/05/2024.'))

ggsave ('results/validation_taxexecrp6.png', width=9, height=5)

# cav index validaton - rcl - simple correlation -------

vc <- import (here ('data/volpecamb_data.txt'))

rcl <- import (here ('data/rcl_anoanterior_2015a2024.csv')) %>% 
  dplyr::select (-V1) %>%
  dplyr::rename ('year'=ano)

colnames (vc) <- c('year', 'valpago_rp6', 'restos_rp6', 
                   'totalpago_rp6', 'percrcl_valpago_rp6', 
                   'percrcl_restos_rp6', 'percrcl_totalpago_rp6')

glimpse (vc)
glimpse (rcl)
glimpse (dt)

vc$percrcl_totalpago_rp6 <- vc$percrcl_totalpago_rp6 %>%
  str_remove_all('%') %>% as.numeric()

dt <- dt %>%
  filter (rp=='6 - Primária discricionária, decorrente de emendas individuais, considerada no cálculo do RP') %>%
  dplyr::select (year, pago) %>%
  mutate (year=as.numeric(year)) %>%
  left_join(rcl, by='year') %>%
  drop_na () %>%
  mutate (percrcl_totalpago_rp6=100*pago/rcl_ano_anterior) %>%
  dplyr::select(year, percrcl_totalpago_rp6)

db2 <- vc %>%
  dplyr::select (year, percrcl_totalpago_rp6) %>%
  bind_rows(dt) %>%
  left_join(cav_met%>%dplyr::select(year, i_final), by='year')

cor_spearman <- db2 %>% drop_na () %>% dplyr::select (i_final, percrcl_totalpago_rp6) %>% cor (method = 'spearman')
cor_pearson <- db2 %>% drop_na () %>% dplyr::select (i_final, percrcl_totalpago_rp6) %>% cor (method = 'pearson')

db2 %>%
  ggplot (aes(x=i_final, y=percrcl_totalpago_rp6)) +
  geom_smooth(color=mypal[3],fill=mypal[1], alpha=.5,
              method='lm', level=.99) +
  geom_point (color=mypal[4], size=5, alpha=.7) +
  geom_text_repel (aes(label=year)) +
  annotate('text', x=20, y=1, label='Intervalo de Confiança: 99%') +
  theme_classic() +
  labs (x='Índice de Poder Orçamentário do Legislativo Brasileiro',
        y='Valor Pago das Emendas Individuais (RP-6)\ncomo Percentual da RCL do ano anterior',
        title='Relação entre Índice e Montante Executado para Emendas Individuais',
        caption = paste('Coeficiente de Correlação de Pearson =',round(cor_pearson[1,2],5),
                        '\nCoeficiente de Correlação de Spearman =',round(cor_spearman[1,2],5),
                        '\nFontes: SIOP (extração em 18/05/2024), Volpe e Cambraia (2015) e Secretaria do Tesouro Nacional (extração em 25/05/2024).'))


ggsave ('results/validation_percrclpago.png', width=9, height=5)

# cav index validation - all models ----------------

## cleaning

glimpse (cav)
glimpse (db)

cav_especial <- cav %>%
  filter (question_code!='cav_exec_imp') %>% 
  group_by(dimension1, year) %>%
  summarize (score_subindex=sum(score))%>%
  pivot_wider(names_from = dimension1, values_from = score_subindex) %>%
  mutate (i_final_modif=`Capacidade organizacional do Legislativo em relação ao orçamento`*`Poder do Legislativo face ao Executivo`) %>%
  dplyr::select (year, i_final_modif)

cav_especial$i_final_modif <- (cav_especial$i_final_modif*100)/(40*50)

db <- left_join(db, cav_especial, by='year')

glimpse (db2)
db <- full_join(db2, db)
glimpse (db)
db$orcimp <- ifelse (db2$year>=2014,1,0)

ip <- import (here ('data/ipca_ibge.csv'))
colnames(ip) <- c('year', 'ipca_br')
ip$year <- str_sub(ip$year, start=-4, end=-1) %>% as.numeric()

glimpse (ip)
glimpse (db)

db <- left_join(db, ip, by='year')

db <- db %>%
  dplyr::rename ('exec_rp6_rcl' = percrcl_totalpago_rp6,
          'taxexec_rp6' = tax_execution)

db$i_final_sq <- db$i_final*db$i_final


covid_year <- c(2020, 2021)
db$covid <- ifelse (db$year%in%covid_year, 1, 0)

dbrcl <- db %>%
  dplyr::select (-c(rp:i_final_modif))

# models

m1 <- lm (taxexec_rp6 ~ i_final + covid + ipca_br, data=db)
m2 <- lm (taxexec_rp6 ~ i_final_modif + covid + ipca_br, data=db)
m3 <- lm (exec_rp6_rcl ~ i_final + covid + orcimp + ipca_br, data=dbrcl)
m4 <- lm (exec_rp6_rcl ~ i_final_sq + covid + orcimp + ipca_br, data=dbrcl)

tb <- mtable (m1, m2, m3, m4)
write.mtable(tb, format="LaTeX", 
             file='results/regressions_validation_table_final.tex')

tb

# other indexes - data --------

db <- import (here ('data/indexes - Data.csv'))

meta <- import (here ('data/index - final - Metadata - English.csv'))

db$score <- db$score %>% numlimpo ()

glimpse (db)

# alesina et al (1998) -----------------

db$is_ale <- ifelse(str_detect(db$question_code, 'ale_'), 1, 0)

ale <- db %>%
  filter (is_ale==1) %>%
  filter (subcomponent==0) %>%
  left_join(meta%>%dplyr::select(question_code, dimension1, dimension2), 
            by='question_code') %>% 
  dplyr::select (year, score, question_code, dimension2) %>%
  group_by (year,dimension2) %>%
  dplyr::summarise (subindex_score=base::sum(score)) %>%
  pivot_wider (names_from = dimension2, values_from = subindex_score)


ale$`Agenda-setting subindex normalized` <- (ale$`Agenda-setting subindex`*100)/(10*3)
ale$`Borrowing constraints subindex normalized` <- (ale$`Borrowing constraints subindex`*100)/(10*5)
ale$`Borrowing practicies subindex normalized` <- (ale$`Borrowing practicies subindex`*100)/(10*2)
ale$`Composite index` <- (ale$`Agenda-setting subindex`+ale$`Borrowing constraints subindex`+ale$`Borrowing practicies subindex`)
ale$`Composite index normalized` <- (ale$`Composite index`*100)/(10*10)

ale <- ale %>%
  dplyr::select(year, 
                `Agenda-setting subindex normalized`:`Borrowing practicies subindex normalized`,
                `Composite index normalized`) %>%
  pivot_longer(`Agenda-setting subindex normalized`:`Composite index normalized`) %>%
  dplyr::rename ('index'=name,
          'score'=value)

glimpse (ale)

labsale <- c('Índice Composto\n(normalizado)',
             'Subíndice 1: restrições\naos empréstimos\n(normalizado)',
             'Subíndice 2: hierarquização\ndos procedimentos\n(normalizado)',
             'Subíndice 3: práticas de\nempréstimo dos entes\npúblicos dependentes\n(normalizado)')

orderale <- c('Composite index normalized',
              'Borrowing constraints subindex normalized',
              'Agenda-setting subindex normalized',
              'Borrowing practicies subindex normalized')

ale %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=factor(index,orderale), color=factor(index,orderale)),
             size=1, alpha=1) +
  geom_point(aes(color=factor(index,orderale))) +
  scale_y_continuous(limits=c(0,100)) +
  scale_color_manual(values=mypal,
                     labels=labsale) +
  scale_alpha_manual (values=c(0,1)) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Reprodução dos Índices de Alesina et al. (1998)')

ggsave ('results/alesina_rep.png', width=10, height = 6)

# wehner (2006) -------------------

db$is_weh <- ifelse(str_detect(db$question_code, 'weh_'), 1, 0)

weh <- db %>%
  filter (is_weh==1) %>%
  filter (subcomponent==0) %>%
  left_join(meta%>%dplyr::select(question_code, dimension1, dimension2), 
            by='question_code') %>% 
  dplyr::select (year, score, question_code, dimension1) %>%
  group_by (year,dimension1) %>%
  summarise (subindex_score=sum(score)) %>%
  pivot_wider (names_from = dimension1, values_from = subindex_score)

weh$`Composite index` <- weh$`Formal legislative authority vis-à-vis the executive`*weh$`Organisational capacity of the legislature`
weh$`Formal legislative authority vis-à-vis the executive normalized` <- weh$`Formal legislative authority vis-à-vis the executive`*100/30
weh$`Organisational capacity of the legislature normalized` <- weh$`Organisational capacity of the legislature`*100/30
weh$`Composite index normalized` <- weh$`Composite index`*100/900

weh <- weh %>%
  dplyr::select(year, `Formal legislative authority vis-à-vis the executive normalized`:`Composite index normalized`) %>%
  pivot_longer(`Formal legislative authority vis-à-vis the executive normalized`:`Composite index normalized`) %>%
  dplyr::rename ('index'=name,
                 'score'=value)

glimpse (weh)

labsweh <- c('Índice Composto\n(normalizado)',
             'Subíndice 1: poder formal do\nLegislativo face ao Executivo\n(normalizado)',
             'Subíndice 2: capacidade\norganizacional do Legislativo\n(normalizado)')

weh %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=index),
             size=1, alpha=1) +
  geom_point (aes(color=index)) +
  scale_y_continuous(limits=c(0,100)) +
  scale_color_manual(values=mypal,
                     labels=labsweh) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Reprodução dos Índices de Wehner (2006)')

ggsave ('results/wehner_rep.png', width=10, height = 6)

# dabla-norris et al (2010) --------------------

db$is_dab <- ifelse(str_detect(db$question_code, 'dab_'), 1, 0)

dab <- db %>%
  filter (is_dab==1) %>%
  filter (subcomponent==0) %>%
  left_join(meta%>%dplyr::select(question_code, dimension1, dimension2), 
            by='question_code') %>%
  dplyr::select (year, score, question_code, dimension1, dimension2)

glimpse (dab)

dab_stage <- dab %>%
  group_by(dimension1, year) %>%
  summarise (s_index=mean(score)) %>%
  pivot_wider (names_from = dimension1, values_from = s_index) %>%
  dplyr::rename ('S1'=`Budget Planning and Negotiation`,
                 'S2'=`Budget Approval`,
                 'S3'=`Budget Implementation`) %>%
  dplyr::mutate (stage_index=(S1+S2+S3)/3)

dab_category <- dab %>%
  group_by(dimension2, year) %>%
  summarise (c_index=mean(score)) %>%
  pivot_wider (names_from = dimension2, values_from = c_index) %>%
  dplyr::rename ('C1'=Comprehensiveness,
                 'C2'=`Rules and Controls`,
                 'C3'=`Sustainability and Credibility`,
                 'C4'=`Top-down Budgeting`,
                 'C5'=Transparency) %>%
  dplyr::mutate (category_index=(C1+C2+C3+C4+C5)/5)


dab <- full_join(dab_stage, dab_category, by='year') %>%
  pivot_longer(S2:category_index) %>%
  dplyr::rename ('index'=name,'score'=value)


dab %>%
  filter (index == 'stage_index' |
            index == 'category_index') %>%
  mutate (score=score*25) %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=index),
             size=1, alpha=1) +
  geom_point (aes(color=index)) +
  scale_color_manual(values=mypal,
                     labels=c('Índice de categorias\n(normalizado)', 'Índice de estágios\n(normalizado)')) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Reprodução dos Índices Principais de Dabla-Norris et al. (2010)')

ggsave ('results/dab_rep_mainindexes.png', width=10, height=6)

cindexes <- c('Compreensividade',
              'Regras e controles',
              'Durabilidade e credibilidade',
              'Procedimentos hierárquicos',
              'Transparência',
              'Planejamento e negociação',
              'Aprovação',
              'Implementação')

dab$main_index <- ifelse (dab$index%in%c('S1','S2','S3'), 'Subíndice de Estágios',
                          ifelse (dab$index%in%c('stage_index','category_index'), 'Main Index',
                                  'Subíndice de Categorias'))

glimpse (dab)

dab %>%
  filter (main_index!='Main Index') %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=index),
             size=1, alpha=1) +
  geom_point (aes(color=index, shape=index), size=2) +
  scale_color_manual(name='',
                     labels=cindexes,
                     values=mypal) +
  scale_shape_manual(name='',
                     values=c(rep(myshapes[1],5), rep(myshapes[2],3)),
                     labels=cindexes) +
  facet_wrap(vars(main_index)) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Reprodução dos Subíndices de Dabla-Norris et al. (2010)',
        subtitle='Subíndices de categorias e estágios institucionais.')

ggsave ('results/dab_rep_subindexes.png', width=12, height=6)

# análises dos índices em conjunto ------------------

# cav 

glimpse (cav_met)
cav_met$cav_subindex1nor <- cav_met$cav_subindex1*100/50
cav_met$cav_subindex2nor <- cav_met$cav_subindex2*100/50

cav <- full_join(cav_met%>%dplyr::select(year, i_final, cav_subindex1nor, cav_subindex2nor),
                 cav_especial,
                 by='year')
cav <- cav %>%
  pivot_longer(i_final:i_final_modif) %>%
  dplyr::rename ('index'=name,
                 'score'=value) %>%
  mutate (source='Cavalcanti')

cav$index <- case_when(cav$index=='cav_subindex1nor'~'Subíndice 1: poder do Legislativo face ao Executivo (normalizado)',
                       cav$index=='cav_subindex2nor'~'Subíndice 2: capacidade organizacional do Legislativo em relação ao orçamento (normalizado)',
                       cav$index=='i_final'~'Índice composto (normalizado)',
                       cav$index=='i_final_modif'~'Índice composto modificado (normalizado)')

glimpse (cav)
cav %>% count (index)

# ale

ale$source <- 'Alesina et al (1998)'

ale$score <- 100-ale$score

ale$index <- case_when(ale$index=='Agenda-setting subindex normalized'~'Subíndice 2: hierarquização dos procedimentos (normalizado e invertido)',
                       ale$index=='Borrowing constraints subindex normalized'~'Subíndice 1: restrições aos empréstimos (normalizado e invertido)',
                       ale$index=='Borrowing practicies subindex normalized'~'Subíndice 3: práticas de empréstimo dos entes públicos dependentes (normalizado e invertido)',
                       ale$index=='Composite index normalized'~'Índice composto (normalizado e invertido)')



# dab

dab <- as_tibble(dab)

dab <- dab %>%
  filter (main_index=='Main Index' | index=='C4') %>%
  dplyr::select (-main_index)

dab$score <- 100-(dab$score*100/4)

dab %>% count (index)

dab$index <- case_when(dab$index=='stage_index'~'Índice de estágios (normalizado e invertido)',
                       dab$index=='category_index'~'Índice de categorias (normalizado e invertido)',
                       dab$index=='C4'~'Subíndice de categorias: procedimentos hierárquicos (normalizado e invertido)')

dab$source <- 'Dabla-Norris et al (2010)'

glimpse (dab)

# weh

weh$index <- case_when(weh$index=='Composite index normalized'~'Índice Composto (normalizado)',
                       weh$index=='Formal legislative authority vis-à-vis the executive normalized'~'Subíndice 1: poder formal do Legislativo face ao Executivo (normalizado)',
                       weh$index=='Organisational capacity of the legislature normalized'~'Subíndice 2: capacidade organizacional do Legislativo (normalizado)')

weh$source <- 'Wehner (2006)'

glimpse (weh)

# juntando

dtall <- bind_rows(ale, weh, dab, cav) %>% as_tibble()

dtall %>% count (index, source)

dtall$index %>% unique()

dtall$subindex <- ifelse (str_detect(dtall$index, 'Subíndice'), 1, 0)

glimpse (dtall)


# índices principais

mypal <- c('#999999', '#404040', '#ef8a62', 'lightblue') %>% rep(5)

dtall %>%
  dplyr::filter (subindex==0) %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=source)) +
  geom_point (aes(group=index, color=source, shape=index)) +
  scale_color_manual(values=mypal) +
  scale_shape_manual(values=myshapes) +
  theme_classic() +
  theme (legend.position = 'right',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color='Fonte',
        shape='Índice',
        x=NULL,
        y=NULL,
        title='Comparações entre Índices Principais')

ggsave ('results/comp_main_indexes.png', width=12, height=6)


# índices semelhantes

dtall$looklike <- case_when(dtall$index=='Índice composto (normalizado)'&dtall$source=='Cavalcanti'~1,
                            dtall$index=='Subíndice 1: poder do Legislativo face ao Executivo (normalizado)'~1,
                            dtall$index=='Subíndice 1: poder formal do Legislativo face ao Executivo (normalizado)'~1,
                            dtall$index=='Subíndice 2: capacidade organizacional do Legislativo (normalizado)'~1,
                            dtall$index=='Subíndice 2: capacidade organizacional do Legislativo em relação ao orçamento (normalizado)'~1,
                            dtall$index=='Índice Composto (normalizado)'&dtall$source=='Wehner (2006)'~1,
                            dtall$index=='Subíndice 2: hierarquização dos procedimentos (normalizado e invertido)'~1,
                            dtall$index=='Subíndice de categorias: procedimentos hierárquicos (normalizado e invertido)'~1,
                            .default=0)

glimpse (dtall)

dtall %>%
  filter (looklike==1) %>%
  mutate (index=paste(index, '-', source)) %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=index)) +
  geom_point (aes(group=index, color=index, shape=index)) +
  scale_color_manual(values=mypal) +
  scale_shape_manual(values=myshapes) +
  #scale_y_continuous(limits = c(0,100)) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(color = guide_legend(nrow = 4),
         shape = guide_legend(nrow = 4)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Comparações entre Índices Semelhantes')

ggsave ('results/comp_looklike_indexes.png', width=12, height=6)

# tabela de estatísticas descritivas

dtall %>%
  dplyr::filter (subindex==0 | looklike==1) %>%
  group_by(index, source) %>%
  summarise (`Máximo`=max(score),
             `Mínimo`=min(score),
             `Média`=mean(score),
             `Mediana`=median(score)) %>%
  dplyr::rename ('Índice'=index,
                 'Fonte'=source) %>%
  write.xlsx('results/descriptive_stats_indexes.xlsx')

