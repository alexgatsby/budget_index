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

# functions -------------

numlimpo <- function(data){
  results <- data %>%
    str_replace_all(',', 'oi') %>%
    str_remove_all('[[:punct:]]') %>%
    str_replace_all('oi','.') %>%
    as.numeric()
  return(results)
}

# indexes data -----------

db <- import (here ('data/indexes - Data.csv'))

meta <- import (here ('data/indexes - Metadata.csv'))

db$score <- db$score %>% numlimpo ()

# cav index --------

db$is_cav <- ifelse (str_detect(db$question_code, "cav_"), 1, 0)

cav <- db %>%
  filter (is_cav == 1) %>%
  left_join(meta, by='question_code')

glimpse (cav)

## methodologies comparision ------------------

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
  mutate (score_year_i5=`Capacidade organizacional do Legislativo em relação ao orçamento`*`Poder formal do Legislativo face ao Executivo`)

colnames(i5) <- cols

glimpse (i5)

## spearman ranking correlations ---------------------

cav_met <- bind_cols(i1, i2, i3, i4, i5%>%dplyr::select(year, score_year_i5)) %>%
  mutate (year=year...1) %>%
  dplyr::select (year,score_year_i1,score_year_i2,score_year_i3,score_year_i4,score_year_i5)


cor (cav_met%>%dplyr::select(-year), method = 'spearman') %>% 
  as_tibble() %>%
  write.xlsx('data/spearman_cav_indexes.xlsx')

# cav index questions visualization --------------

mypal <- c('#999999', '#ef8a62', '#404040', 'lightblue') %>% rep(5)
myshapes <- c(0,1,2,4,8) %>% rep(3)
glimpse (cav)

qorder <- paste('Questão',1:15)
cav$qlabel <- str_sub(cav$question, start=1, end=11) %>% 
  str_remove_all('[[:punct:]]') %>%
  str_remove_all('[[:blank:]]') %>%
  str_replace_all('o', 'o ')

cav %>%
  dplyr::select (year, qlabel, score, dimension2) %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=factor(qlabel, qorder), color=factor(qlabel, qorder))) +
  geom_point (aes(shape=factor(qlabel, qorder), color=factor(qlabel, qorder))) +
  scale_color_manual(values=mypal) +
  scale_shape_manual(values=myshapes) +
  theme_classic() +
  facet_wrap(vars(dimension2))+
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Pontuações do Índice por Ano, Questão e Critério')

cav %>%
  dplyr::select (year, question_code, score) %>%
  pivot_wider(names_from = question_code, values_from = score) %>%
  write.xlsx('data/index_questions_datawrapper.xlsx')

# cav final index visualization ---------------

cav_met$i_final <- (cav_met$score_year_i5*100)/5600

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

# cav index validation - execution tax - simple correlation ----------------

dt <- import (here ('data/rps_ano_novo.xlsx'))

colunas <- c('year', 'rp', 'proj_lei', 'dot_inicial', 
             'dot_atual', 'empenhado', 'liquidado', 'pago')
colnames(dt) <- colunas

db <- dt %>%
  mutate(year=numlimpo(year))%>%
  filter (rp=='6 - Primária discricionária, decorrente de emendas individuais, considerada no cálculo do RP') %>%
  left_join(cav_met%>%dplyr::select(year,i_final),by='year') %>%
  mutate (tax_execution = 100*pago/dot_inicial)

cor_spearman <- db %>% drop_na () %>% dplyr::select (i_final, tax_execution) %>% cor (method = 'spearman')
cor_pearson <- db %>% drop_na () %>% dplyr::select (i_final, tax_execution) %>% cor (method = 'pearson')

db %>%
  ggplot (aes(x=i_final, y=tax_execution)) +
  geom_smooth(color=mypal[3],fill=mypal[1], alpha=.5) +
  geom_point (color=mypal[2]) +
  geom_text_repel (aes(label=year)) +
  theme_classic() +
  labs (x='Índice de Poder Orçamentário do Legislativo Brasileiro',
        y='Taxa de Execução das\nEmendas Individuais (RP-6)',
        title='Relação entre Índice e Execução de Emendas Individuais',
        subtitle='Fonte: SIOP.',
        caption = paste('Coeficiente de Correlação de Pearson =',round(cor_pearson[1,2],5),
                         '\nCoeficiente de Correlação de Spearman =',round(cor_spearman[1,2],5)))

# cav index validaton - rcl - simple correlation -------

vc <- import (here ('data/volpecamb_data.txt'))

rcl <- import (here ('data/rcl_anoanterior_2015a2024.csv')) %>% 
  dplyr::select (-V1) %>%
  dplyr::rename ('year'=ano)

colnames (vc) <- c('year', 'valpago_rp6', 'restos_rp6', 
                   'totalpago_rp6', 'percrcl_valpago_rp6', 
                   'percrcl_restos_rp6', 'percrcl_totalpago_rp6')

glimpse (vc)

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
  geom_smooth(color=mypal[3],fill=mypal[1], alpha=.5) +
  geom_point (color=mypal[2]) +
  geom_text_repel (aes(label=year)) +
  theme_classic() +
  labs (x='Índice de Poder Orçamentário do Legislativo Brasileiro',
        y='Valor Pago das Emendas Individuais (RP-6)\ncomo Percentual da RCL do ano anterior',
        title='Relação entre Índice e Montante Executado para Emendas Individuais',
        subtitle='Fonte: SIOP.',
        caption = paste('Coeficiente de Correlação de Pearson =',round(cor_pearson[1,2],5),
                        '\nCoeficiente de Correlação de Spearman =',round(cor_spearman[1,2],5)))

db2 %>% filter (year==2023)

# cav index validation - all models ----------------

cav_especial <- cav %>%
  filter (question_code!='cav_exec_imp') %>% 
  group_by(dimension1, year) %>%
  summarize (score_subindex=sum(score))%>%
  pivot_wider(names_from = dimension1, values_from = score_subindex) %>%
  mutate (score_year_especial=`Capacidade organizacional do Legislativo em relação ao orçamento`*`Poder formal do Legislativo face ao Executivo`) %>%
  dplyr::select (year, score_year_especial)

cav_especial$score_year_especial<- (cav_especial$score_year_especial*100)/(70*70)

db <- left_join(db, cav_especial, by='year')

covid_year <- c(2020, 2021)
db$covid <- ifelse (db$year%in%covid_year, 1, 0)

m1 <- lm (tax_execution ~ i_final + covid, data=db)
m2 <- lm (tax_execution ~ score_year_especial + covid, data=db)

db2 <- left_join(db2, cav_especial, by='year')

db2$ano_orçimp <- ifelse (db2$year>=2014,1,0)

db2$covid <- ifelse (db2$year%in%covid_year, 1, 0)

m3 <- lm (percrcl_totalpago_rp6 ~ i_final + covid + ano_orçimp, data=db2)

tb <- mtable (m1, m2, m3)
write.mtable(tb, format="LaTeX", 
             file='results/regressions_validation_table_v3.tex')


# other indexes ----------

db <- import (here ('data/indexes - Data.csv'))

meta <- import (here ('data/indexes - Metadata.csv'))

db$score <- db$score %>% numlimpo ()

## alesina et al (1998) -----------------

db$is_ale <- ifelse(str_detect(db$question_code, 'ale_'), 1, 0)

ale <- db %>%
  filter (is_ale==1) %>%
  filter (subcomponent==0) %>%
  left_join(meta%>%dplyr::select(question_code, dimension1, dimension2), 
            by='question_code') %>% 
  dplyr::select (year, score, question_code, dimension2) %>%
  group_by (year,dimension2) %>%
  summarise (subindex_score=sum(score)) %>%
  pivot_wider (names_from = dimension2, values_from = subindex_score) %>%
  mutate (`Composite Index` = sum(`Agenda-setting subindex`, 
                                  `Borrowing constraints subindex`,
                                  `Borrowing practicies subindex`)) %>%
  pivot_longer(`Agenda-setting subindex`:`Composite Index`) %>%
  dplyr::rename ('index'=name,
          'score'=value)

labsale <- c('Índice Composto',
             'Subíndice 1: restrições aos empréstimos',
             'Subíndice 2: hierarquização dos procedimentos',
             'Subíndice 3: práticas de empréstimo dos entes públicos dependentes')

orderale <- c('Composite Index',
              'Borrowing constraints subindex',
              'Agenda-setting subindex',
              'Borrowing practicies subindex')

ale %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=factor(index,orderale), color=factor(index,orderale)),
             size=1, alpha=1) +
  geom_point(aes(color=factor(index,orderale))) +
  geom_text_repel (aes(label=prettyNum(round(score,2), 
                                 decimal.mark = ',', big.mark = '.')),
             size=3, hjust=0) +
  scale_y_continuous(limits=c(0,100)) +
  scale_color_manual(values=c('#999999', '#ef8a62', '#404040', 'lightblue'),
                     labels=labsale) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Reprodução dos Índices de Alesina et al. (1998)')

ggsave ('results/alesina_rep.png', width=10, height = 6)


## wehner (2006) -------------------

db$is_weh <- ifelse(str_detect(db$question_code, 'weh_'), 1, 0)

weh <- db %>%
  filter (is_weh==1) %>%
  filter (subcomponent==0) %>%
  left_join(meta%>%dplyr::select(question_code, dimension1, dimension2), 
            by='question_code') %>% 
  dplyr::select (year, score, question_code, dimension1) %>%
  group_by (year,dimension1) %>%
  summarise (subindex_score=sum(score)) %>%
  pivot_wider (names_from = dimension1, values_from = subindex_score) %>%
  mutate (`Composite Index` = `Formal legislative authority vis-à-vis the executive`*`Organisational capacity of the legislature`) %>%
  mutate (`Composite Index Normalized` = (`Composite Index`*100)/900) %>%
  pivot_longer(`Formal legislative authority vis-à-vis the executive`:`Composite Index Normalized`) %>%
  dplyr::rename ('index'=name,
          'score'=value)

labsweh <- c('Índice Composto Normalizado',
             'Subíndice 1: poder formal do Legislativo face ao Executivo',
             'Subíndice 2: capacidade organizacional do Legislativo')

weh %>%
  filter (index != 'Composite Index') %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=index),
             size=1, alpha=1) +
  geom_point (aes(color=index)) +
  geom_text_repel (aes(label=prettyNum(round(score,2), 
                                 decimal.mark = ',', big.mark = '.')),
             hjust=0,
             size=3) +
  scale_y_continuous(limits=c(0,100)) +
  scale_color_manual(values=c('#999999', '#ef8a62', '#404040', 'lightblue'),
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

## dabla-norris (2010) ---------------------

db$is_dab <- ifelse(str_detect(db$question_code, 'dab_'), 1, 0)

dab <- db %>%
  filter (is_dab==1) %>%
  filter (subcomponent==0) %>%
  left_join(meta%>%dplyr::select(question_code, dimension1, dimension2), 
            by='question_code') %>%
  dplyr::select (year, score, question_code, dimension1, dimension2)

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
                 'C5'=Transparency ) %>%
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
  geom_text_repel (aes(label=prettyNum(round(score,2), 
                                       decimal.mark = ',', big.mark = '.')),
                   hjust=0,
                   size=3) +
  scale_y_continuous(limits=c(0,100)) +
  scale_color_manual(values=c('#999999', '#ef8a62', '#404040', 'lightblue'),
                     labels=c('Índice de Categorias', 'Índice de Estágios')) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Reprodução dos Índices de Dabla-Norris et al. (2010)',
        caption='Para fins de apresentação, os índices foram normalizados para uma escala de 0 a 100.')

ggsave ('results/dab_rep_mainindexes.png', width=10, height=6)

cindexes <- c('Compreensividade',
              'Regras e controles',
              'Durabilidade e credibilidade',
              'Procedimentos hierárquicos',
              'Transparência')

dab %>%
  filter (str_detect(index, 'C')) %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=index),
             size=1, alpha=1) +
  geom_point (aes(color=index, shape=index), size=2) +
  geom_text_repel (aes(label=prettyNum(round(score,2), 
                                       decimal.mark = ',', big.mark = '.')),
                   hjust=0,
                   size=3) +
  #scale_y_continuous(limits=c(0,100)) +
  scale_color_manual(values=c('#999999', '#ef8a62', '#404040', 
                              'lightblue', '#999999'),
                     labels=cindexes) +
  scale_shape_manual(values=rep(c(1,2),3),
                     labels=cindexes) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Reprodução dos Índices de Dabla-Norris et al. (2010)',
        subtitle='Subíndices de categorias institucionais.')

ggsave ('results/dab_rep_categoryindexes.png', width=10, height=6)

sindexes <- c('Planejamento e negociação',
              'Aprovação',
              'Implementação')

dab %>%
  filter (str_detect(index, 'S')) %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=index),
             size=1, alpha=1) +
  geom_point (aes(color=index, shape=index), size=2) +
  geom_text_repel (aes(label=prettyNum(round(score,2), 
                                       decimal.mark = ',', big.mark = '.')),
                   hjust=0,
                   size=3) +
  #scale_y_continuous(limits=c(0,100)) +
  scale_color_manual(values=c('#999999', '#ef8a62', '#404040', 
                              'lightblue', '#999999'),
                     labels=sindexes) +
  scale_shape_manual(values=rep(c(1,2),3),
                     labels=sindexes) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Reprodução dos Índices de Dabla-Norris et al. (2010)',
        subtitle='Subíndices de estágios do processo orçamentário.')

ggsave ('results/dab_rep_stageindexes.png', width=10, height=6)

## análises ------------------

glimpse (db)
db %>%
  filter (is_weh==1 &
            subcomponent==0) %>%
  ggplot (aes(y=score, x=year)) +
  geom_line (aes(group=question_code, color=question_code))

db %>%
  filter (is_dab==1 &
            subcomponent==0) %>%
  ggplot (aes(y=score, x=year)) +
  geom_line (aes(group=question_code, color=question_code))

db %>%
  filter (is_ale==1) %>%
  filter (subcomponent==0) %>%
  left_join(meta%>%dplyr::select(question_code, dimension1, dimension2), 
            by='question_code') %>%
  count (question_code, dimension2)


## comparações -------------

cav <- data.frame (year=1990:2023,
                   i_final=cav_met$i_final,
                   cav_subindex1=i5$cav_subindex1,
                   cav_subindex2=i5$cav_subindex2) %>% 
  pivot_longer(i_final:cav_subindex2) %>%
  dplyr::rename ('index'=name, 'score'=value) %>%
  dplyr::mutate (source='Cavalcanti')

ale$source <- 'Alesina et al (1998)'
weh$source <- 'Wehner (2006)'
dab$source <- 'Dabla-Norris et al (2010)'
dab$score_old <- dab$score
ale$score_old <- ale$score
dab$score <- 100-dab$score*25
ale$score <- 100-ale$score
weh <- weh %>% filter (index != 'Composite Index')

dtall <- bind_rows(ale, weh, dab, cav)

subindexes_list <- c('Agenda-setting subindex',
                     'Borrowing constraints subindex',
                     'Borrowing practicies subindex',
                     'C1', 'C2', 'C3', 'C4', 'C5',
                     'S1', 'S2', 'S3',
                     'cav_subindex1', 'cav_subindex2',
                     'Formal legislative authority vis-à-vis the executive',
                     'Organisational capacity of the legislature')

dtall$subindex <- ifelse (dtall$index %in% subindexes_list, 1, 0)

order <- c('Composite Index', 'Composite Index Normalized', 'stage_index',
           'category_index', 'i_final')

labels <- c('Índice Composto -\nAlesina et al (1998)',
            'Índice Composto -\nWehner (2006)',
            'Índice de Estágios -\nDabla-Norris et al (2010)',
            'Índice de Categorias -\nDabla-Norris et al (2010)',
            'Índice de\nCavalcanti (2024)')

mypal2 <- c("#999999", "#404040", 'lightblue', 'lightblue', "#ef8a62")

dtall %>%
  dplyr::filter (subindex==0) %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=index)) +
  geom_point (aes(group=index, color=index, shape=index)) +
  scale_color_manual(values=mypal2, labels=labels,
                     breaks=order) +
  scale_shape_manual(values=myshapes, labels=labels,
                     breaks=order) +
  scale_y_continuous(limits = c(0,100)) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Comparações entre Índices Principais')

ggsave ('results/comp_main_indexes.png', width=10, height=6)

dtall %>% group_by (index, source) %>% summarise (quant=n())
looklike <- c('i_final',
              'cav_subindex1',
              'cav_subindex2',
              'C4',
              'Composite Index Normalized',
              'Formal legislative authority vis-à-vis the executive',
              'Organisational capacity of the legislature',
              'Agenda-setting subindex')
labels2 <- c('Índice de\nCavalcanti (2024)',
             'Subíndice 1 -\nCavalcanti (2024)',
             'Subíndice 2 -\nCavalcanti (2024)',
             'Subíndice C4 -\nDabla-Norris et al (2010)',
             'Índice Composto -\nWehner (2006)',
             'Subíndice 1 -\nWehner (2006)',
             'Subíndice 2 -\nWehner (2006)',
             'Subíndice 2 -\nAlesina et al (1998)')
mypal3 <- c("#ef8a62", "#ef8a62", "#ef8a62",
            'lightblue',
            "#404040", "#404040", "#404040",
            "#999999")


dtall %>%
  filter (index %in% looklike) %>%
  mutate (index = factor(index,looklike)) %>%
  ggplot (aes(x=as.character(year), y=score)) +
  geom_line (aes(group=index, color=index)) +
  geom_point (aes(group=index, color=index, shape=index)) +
  scale_color_manual(values=mypal3, labels=labels2) +
  scale_shape_manual(values=myshapes, labels=labels2) +
  scale_y_continuous(limits = c(0,100)) +
  theme_classic() +
  theme (legend.position = 'bottom',
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (color=NULL,
        shape=NULL,
        x=NULL,
        y=NULL,
        title='Comparações entre Índices Semelhantes')

ggsave ('results/comp_looklike_indexes.png', width=10, height=6)

# dívida pública ------------------

dp <- import (here ('data/IMF - Brazil Data - Página1.csv'))

dp$CGD <- numlimpo (dp$CGD)
dp$year <- dp$year-1

dtall$score_old <- ifelse (is.na(dtall$score_old), dtall$score, dtall$score_old)

dt <- full_join(dtall%>%filter(subindex==0),
                dp%>%filter(year>=1990),
                by='year') %>%
  mutate (indexsource=paste(index, '|', source)) %>%
  dplyr::select (-c(index, source, subindex,year, score)) %>%
  pivot_wider(names_from = indexsource, values_from = score_old) %>%
  drop_na () %>%
  ungroup () %>%
  dplyr::select (-year)
colnames (dt) <- c('CGD_as', 'ale', 'weh', 'dab_s', 'dab_c', 'cav')

glimpse (dt)

M <- cor (dt, method = 'pearson')

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(dt)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(M, method="color", type="upper",
         col=col(200),
         tl.col="black", tl.srt=45,
         sig.level = 0.05, p.mat=p.mat,
         addCoef.col = "white", diag=T)


         