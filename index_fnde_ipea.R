# Dicionário FNDE X IPEA

library(writexl)
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(fuzzyjoin)

# Bases de dados:
load("C:/Users/Yasmin/Documents/NEPA-TCC/Bases de dados FNDE - SIGPC/dicipea.Rda") ## referência IPEA (observações únicas da primeira palavra do item de referência)
load("C:/Users/Yasmin/Documents/NEPA-TCC/Bases de dados FNDE - SIGPC/dicfnde.Rda") ## base fnde (observações únicas da primeira palavra do item)
colnames(dicfnde)
colnames(dicipea)=c("ipea_raw")
colnames(dicipea)

# Incluir regionalismos na base do IPEA
# Bergamota e ponkan é mexerica! (tangerina também, mas já tem na base)
# Macaxeira e aipim é mandioca 
# jerimum é abobora
# colorau é urucum
# mugunza é canjica

regional=c("bergamota", "ponkan", "macaxeira", "aipim", "jerimum", "colorau", "mugunza")
regional_table=tibble(regional)
View(regional_table)
colnames(regional_table)=c("ipea_raw")

dicipea=rbind(dicipea, regional_table)
View(dicipea)

# Primeira Rodada: max.dist=0
fnde_ipea=dicfnde%>%stringdist_left_join(dicipea, by=c("fnde_raw"="ipea_raw"), max_dist=0)
nrow(dicfnde)
nrow(dicipea)
nrow(fnde_ipea)
colnames(fnde_ipea)

## Sucesso na primeira rodada com max.dist=0
fnde_ipea_0_sucesso=fnde_ipea%>%filter(!is.na(ipea_raw))
### Salvar sucesso para etapa de supervisão
write.csv(fnde_ipea_0_sucesso, "fnde_ipea_0_sucesso.csv")

## Fracasso na primeira rodada
fnde_ipea_0_fracasso=fnde_ipea%>%filter(is.na(ipea_raw))

## Checagem de linhas primeira rodada
nrow(fnde_ipea)
nrow(fnde_ipea_0_sucesso)
nrow(fnde_ipea_0_fracasso) ## ok!

View(fnde_ipea_0_fracasso)

## corrigir moranga (abóbora -> indexação com max_dist=1 altera para morango!)
fnde_ipea_0_fracasso$fnde_raw=str_replace_all(fnde_ipea_0_fracasso$fnde_raw, "moranga", "abobora")

# Segunda rodada: max.dist=1 
# Na segunda rodada eu considero apenas os fracassos da primeira rodada
fnde_ipea_1=fnde_ipea_0_fracasso%>%stringdist_left_join(dicipea, by=c("fnde_raw"="ipea_raw"), max_dist=1)

## Sucesso na segunda rodada
colnames(fnde_ipea_1)
fnde_ipea_1_sucesso=fnde_ipea_1%>%filter(!is.na(ipea_raw.y))
### Salvar sucesso para etapa de supervisão
colnames(fnde_ipea_1_sucesso)
fnde_ipea_1_sucesso=fnde_ipea_1_sucesso%>%select(1,3)
colnames(fnde_ipea_1_sucesso)=c("fnde_raw", "ipea_raw")
write.csv(fnde_ipea_1_sucesso, "fnde_ipea_1_sucesso.csv")

## Fracasso na segunda rodada
fnde_ipea_1_fracasso=fnde_ipea_1%>%filter(is.na(ipea_raw.y))
colnames(fnde_ipea_1_fracasso)
fnde_ipea_1_fracasso=fnde_ipea_1_fracasso%>%select(1,3)
colnames(fnde_ipea_1_fracasso)=c("fnde_raw", "ipea_raw")

## Checagem de linhas segunda rodada
nrow(fnde_ipea_1)
nrow(fnde_ipea_1_sucesso)
nrow(fnde_ipea_1_fracasso) ## ok!

# Terceira rodada: max.dist=2
# Na terceira rodada eu considero apenas os fracassos da segunda rodada
fnde_ipea_2=fnde_ipea_1_fracasso%>%stringdist_left_join(dicipea, by=c("fnde_raw"="ipea_raw"), max_dist=2)

## Sucesso na terceira rodada
colnames(fnde_ipea_2)
fnde_ipea_2_sucesso=fnde_ipea_2%>%filter(!is.na(ipea_raw.y))
nrow(fnde_ipea_2_sucesso)
### Salvar sucesso para etapa de supervisão
colnames(fnde_ipea_2_sucesso)
fnde_ipea_2_sucesso=fnde_ipea_2_sucesso%>%select(1,3)
colnames(fnde_ipea_2_sucesso)=c("fnde_raw", "ipea_raw")
write.csv(fnde_ipea_2_sucesso, "fnde_ipea_2_sucesso.csv")

## Fracasso na terceira rodada
fnde_ipea_2_fracasso=fnde_ipea_2%>%filter(is.na(ipea_raw.y))
colnames(fnde_ipea_2_fracasso)
fnde_ipea_2_fracasso=fnde_ipea_2_fracasso%>%select(1,3)
colnames(fnde_ipea_2_fracasso)=c("fnde_raw", "ipea_raw")

## Checagem de linhas terceira rodada
nrow(fnde_ipea_2)
nrow(fnde_ipea_2_sucesso)
nrow(fnde_ipea_2_fracasso) ## ok!


# SUPERVISÃO (manual via excel)
# Sucesso supervisionado -> check=1

# Rodada 1:
fnde_ipea_0_sucesso_check=read.csv("C:/Users/Yasmin/Documents/NEPA-TCC/Bases de dados FNDE - SIGPC/sucesso_check/fnde_ipea_0_sucesso_check.csv", sep=";", de=",")
colnames(fnde_ipea_0_sucesso_check)
fnde_ipea_0_sucesso_check=fnde_ipea_0_sucesso_check%>%select(1,2,4)
## Percentual de sucesso supervisionado
sucesso_supervis_0=fnde_ipea_0_sucesso_check%>%filter(check==1)
fracasso_supervis_0=fnde_ipea_0_sucesso_check%>%filter(check==0)
perc_sucesso_0=(nrow(fnde_ipea_0_sucesso_check)/nrow(sucesso_supervis_0))*100
perc_sucesso_0 ## 100%

# Rodada 2:
fnde_ipea_1_sucesso_check=read.csv("C:/Users/Yasmin/Documents/NEPA-TCC/Bases de dados FNDE - SIGPC/sucesso_check/fnde_ipea_1_sucesso_check.csv", sep=";", de=",")
colnames(fnde_ipea_1_sucesso_check)
## Percentual de sucesso supervisionado
sucesso_supervis_1=fnde_ipea_1_sucesso_check%>%filter(check==1)
fracasso_supervis_1=fnde_ipea_1_sucesso_check%>%filter(check==0)
perc_sucesso_1=(nrow(sucesso_supervis_1)/nrow(fnde_ipea_1_sucesso_check))*100
perc_sucesso_1 ## 91,5%

# Rodada 3:
fnde_ipea_2_sucesso_check=read.csv("C:/Users/Yasmin/Documents/NEPA-TCC/Bases de dados FNDE - SIGPC/sucesso_check/fnde_ipea_2_sucesso_check.csv", sep=";", de=",")
colnames(fnde_ipea_2_sucesso_check)
## Percentual de sucesso supervisionado
sucesso_supervis_2=fnde_ipea_2_sucesso_check%>%filter(check==1)
fracasso_supervis_2=fnde_ipea_2_sucesso_check%>%filter(check==0)
perc_sucesso_2=(nrow(sucesso_supervis_2)/nrow(fnde_ipea_2_sucesso_check))*100
perc_sucesso_2 ## 20% (é esperado que quanto mais a distância permitida menor o sucesso)


# Juntar bases de sucesso!
# sucesso_supervis_0 + sucesso_supervis_1 + sucesso_supervis_2
nrow(sucesso_supervis_0)
nrow(sucesso_supervis_1)
nrow(sucesso_supervis_2)

# Juntar bases de fracasso
# fracasso na indexação (fnde_ipea_2_fracasso) + fracasso na supervisão (fracasso_supervis_0 + fracasso_supervis_1 + fracasso_supervis_2)

