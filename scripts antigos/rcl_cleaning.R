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

# data 1 ------------------

rcl14 <- import (here ('data/rcl/RCL3Q2014.xls'), skip=9, which=2) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2014)

rcl15 <- import (here ('data/rcl/RCL3Q2015.xls'), skip=9, which=2) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2015)

rcl16 <- import (here ('data/rcl/RCL3Q2016.xls'), skip=9, which=2) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2016)

rcl17 <- import (here ('data/rcl/RCL3Q2017.xls'), skip=9, which=2) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2017)

rcl18 <- import (here ('data/rcl/RCL3Q2018.xls'), skip=9, which=2) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2018)

rcl19 <- import (here ('data/rcl/RCL3Q2019.xlsx'), skip=9, which=2) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2019)

rcl20 <- import (here ('data/rcl/RCL3Q2020.xlsx'), skip=9, which=4) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2020)

rcl21 <- import (here ('data/rcl/RCL3Q2021.xlsx'), skip=9, which=3) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2021)

rcl22 <- import (here ('data/rcl/RCL3Q2022.xlsx'), skip=9, which=3) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2022)

rcl23 <- import (here ('data/rcl/RCL3Q2023.xlsx'), skip=9, which=4) %>%
  filter (...1=='RECEITA CORRENTE LÍQUIDA (III) = (I - II)') %>%
  dplyr::select(`12 MESES`) %>%
  mutate (ano=2023)

# juntando

db <- bind_rows(rcl14, rcl15, rcl16, rcl17, rcl18, rcl19, rcl20, rcl21, rcl22, rcl23)

db$ano = db$ano+1

colnames (db) <- c('rcl_ano_anterior', 'ano')

ano_nao1000 <- c(2021)

db$rcl_ano_anterior <- ifelse(db$rcl_ano_anterior%in%ano_nao1000, 
                              db$rcl_ano_anterior,
                              db$rcl_ano_anterior*1000)

write.csv (db, 'data/rcl_anoanterior_2015a2024.csv')


# data 2 --------------------------

rcl14 <- 641578197.33*1000
rcl15 <- 674522742.0497*1000
rcl16 <-  709929575.00*1000
rcl17 <- 727254323.97132*1000
rcl18 <- 805348403.46657*1000
rcl19 <- 905658589.594291*1000
rcl20 <- 651943266.03115*1000
rcl21 <- 1062519047775.45
rcl22 <- 1253427306.53263*1000
rcl23 <- 1233714884.82018*1000

db2 <- data.frame(rcl_ano_anterior=c(rcl14, rcl15, rcl16, rcl17, rcl18, rcl19, rcl20, rcl21, rcl22, rcl23),
                  ano=2015:2024)

glimpse (db)

a <- full_join(db, db2, by='ano')
glimpse (a)

ggplot (a) +
  geom_line (aes(x=ano, y=rcl_ano_anterior.x, group=1)) +
  geom_line (aes(x=ano, y=rcl_ano_anterior.y, group=1), color='red')

write.csv (db2, 'data/rcl_anoanterior_2015a2024.csv')
