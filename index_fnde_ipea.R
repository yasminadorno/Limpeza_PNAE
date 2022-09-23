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
load("C:/Users/Yasmin/Documents/Projetos_GITHUB/Limpeza_PNAE/dicipea.Rda") ## referência IPEA (observações únicas da primeira palavra do item de referência)
load("C:/Users/Yasmin/Documents/Projetos_GITHUB/Limpeza_PNAE/dicfnde.Rda") ## base fnde (observações únicas da primeira palavra do item)
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
nrow(fnde_ipea_0_sucesso) ## 196
nrow(fnde_ipea_0_fracasso) ## 877 ok!

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
nrow(fnde_ipea_1_sucesso) ## 391
nrow(fnde_ipea_1_fracasso) ## 507 ok!

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
nrow(fnde_ipea_2) ## 698
nrow(fnde_ipea_2_sucesso) ## 365
nrow(fnde_ipea_2_fracasso) ## 333 ok!

# Quarta rodada: max.dist=3
# Na quarta rodada eu considero apenas os fracassos da terceira rodada
fnde_ipea_3=fnde_ipea_2_fracasso%>%stringdist_left_join(dicipea, by=c("fnde_raw"="ipea_raw"), max_dist=3)

## Sucesso na quarta rodada
colnames(fnde_ipea_3)
fnde_ipea_3_sucesso=fnde_ipea_3%>%filter(!is.na(ipea_raw.y))
nrow(fnde_ipea_3_sucesso)
### Salvar sucesso para etapa de supervisão
colnames(fnde_ipea_3_sucesso)
fnde_ipea_3_sucesso=fnde_ipea_3_sucesso%>%select(1,3)
colnames(fnde_ipea_3_sucesso)=c("fnde_raw", "ipea_raw")
write.csv(fnde_ipea_3_sucesso, "fnde_ipea_3_sucesso.csv")

## Fracasso na quarta rodada
fnde_ipea_3_fracasso=fnde_ipea_3%>%filter(is.na(ipea_raw.y))
colnames(fnde_ipea_3_fracasso)
fnde_ipea_3_fracasso=fnde_ipea_3_fracasso%>%select(1,3)
colnames(fnde_ipea_3_fracasso)=c("fnde_raw", "ipea_raw")

## Checagem de linhas quarta rodada
nrow(fnde_ipea_3) ## 754
nrow(fnde_ipea_3_sucesso) ## 563
nrow(fnde_ipea_3_fracasso) ## 191 ok!

# Quinta rodada: max_dist=4
# Apenas fracassos da quarta rodada
fnde_ipea_4=fnde_ipea_3_fracasso%>%stringdist_left_join(dicipea, by=c("fnde_raw"="ipea_raw"), max_dist=4)

## Sucesso na quinta rodada
colnames(fnde_ipea_4)
fnde_ipea_4_sucesso=fnde_ipea_4%>%filter(!is.na(ipea_raw.y))
nrow(fnde_ipea_4_sucesso)
### Salvar sucesso para etapa de supervisão
colnames(fnde_ipea_4_sucesso)
fnde_ipea_4_sucesso=fnde_ipea_4_sucesso%>%select(1,3)
colnames(fnde_ipea_4_sucesso)=c("fnde_raw", "ipea_raw")
write.csv(fnde_ipea_4_sucesso, "fnde_ipea_4_sucesso.csv")

## Fracasso na quinta rodada
fnde_ipea_4_fracasso=fnde_ipea_4%>%filter(is.na(ipea_raw.y))
colnames(fnde_ipea_4_fracasso)
fnde_ipea_4_fracasso=fnde_ipea_4_fracasso%>%select(1,3)
colnames(fnde_ipea_4_fracasso)=c("fnde_raw", "ipea_raw")

## Checagem de linhas quinta rodada
nrow(fnde_ipea_4) ## 388
nrow(fnde_ipea_4_sucesso) ## 278
nrow(fnde_ipea_4_fracasso) ## 110 ok!

# SUPERVISÃO (manual via excel)
# Sucesso supervisionado -> check=1

# Rodada 1:
fnde_ipea_0_sucesso_check=read.csv("C:/Users/Yasmin/Documents/Projetos_GITHUB/Limpeza_PNAE/sucesso_check/fnde_ipea_0_sucesso_check.csv", sep=";", de=",")
colnames(fnde_ipea_0_sucesso_check)
fnde_ipea_0_sucesso_check=fnde_ipea_0_sucesso_check%>%select(1,2,4)
## Percentual de sucesso supervisionado
sucesso_supervis_0=fnde_ipea_0_sucesso_check%>%filter(check==1)
fracasso_supervis_0=fnde_ipea_0_sucesso_check%>%filter(check==0)
perc_sucesso_0=(nrow(fnde_ipea_0_sucesso_check)/nrow(sucesso_supervis_0))*100
perc_sucesso_0 ## 100%

# Rodada 2:
fnde_ipea_1_sucesso_check=read.csv("C:/Users/Yasmin/Documents/Projetos_GITHUB/Limpeza_PNAE/sucesso_check/fnde_ipea_1_sucesso_check.csv", sep=";", de=",")
colnames(fnde_ipea_1_sucesso_check)
## Percentual de sucesso supervisionado
sucesso_supervis_1=fnde_ipea_1_sucesso_check%>%filter(check==1)
fracasso_supervis_1=fnde_ipea_1_sucesso_check%>%filter(check==0)
perc_sucesso_1=(nrow(sucesso_supervis_1)/nrow(fnde_ipea_1_sucesso_check))*100
perc_sucesso_1 ## 90,3%

# Rodada 3:
fnde_ipea_2_sucesso_check=read.csv("C:/Users/Yasmin/Documents/Projetos_GITHUB/Limpeza_PNAE/sucesso_check/fnde_ipea_2_sucesso_check.csv", sep=";", de=",")
colnames(fnde_ipea_2_sucesso_check)
## Percentual de sucesso supervisionado
sucesso_supervis_2=fnde_ipea_2_sucesso_check%>%filter(check==1)
fracasso_supervis_2=fnde_ipea_2_sucesso_check%>%filter(check==0)
perc_sucesso_2=(nrow(sucesso_supervis_2)/nrow(fnde_ipea_2_sucesso_check))*100
perc_sucesso_2 ## 20,3% (é esperado que quanto mais a distância permitida menor o sucesso)

# Rodada 4:
fnde_ipea_3_sucesso_check=read.csv("C:/Users/Yasmin/Documents/Projetos_GITHUB/Limpeza_PNAE/sucesso_check/fnde_ipea_3_sucesso_check.csv", sep=";", de=",")
colnames(fnde_ipea_3_sucesso_check)
## Percentual de sucesso supervisionado
sucesso_supervis_3=fnde_ipea_3_sucesso_check%>%filter(check==1)
fracasso_supervis_3=fnde_ipea_3_sucesso_check%>%filter(check==0)
perc_sucesso_3=(nrow(sucesso_supervis_3)/nrow(fnde_ipea_3_sucesso_check))*100
perc_sucesso_3 ## 5,8% (é esperado que quanto mais a distância permitida menor o sucesso)

# Rodada 5:
fnde_ipea_4_sucesso_check=read.csv("C:/Users/Yasmin/Documents/Projetos_GITHUB/Limpeza_PNAE/sucesso_check/fnde_ipea_4_sucesso_check.csv", sep=";", de=",")
colnames(fnde_ipea_4_sucesso_check)
## Percentual de sucesso supervisionado
sucesso_supervis_4=fnde_ipea_4_sucesso_check%>%filter(check==1)
fracasso_supervis_4=fnde_ipea_4_sucesso_check%>%filter(check==0)
perc_sucesso_4=(nrow(sucesso_supervis_4)/nrow(fnde_ipea_4_sucesso_check))*100
perc_sucesso_4 ## 10% 


# Juntar bases de sucesso!
# sucesso_supervis_0 + sucesso_supervis_1 + sucesso_supervis_2 + sucesso_supervis_3 +sucesso_supervis_4
nrow(sucesso_supervis_0)
nrow(sucesso_supervis_1)
nrow(sucesso_supervis_2)
nrow(sucesso_supervis_3)
nrow(sucesso_supervis_4)

sucesso_supervis_total=rbind(sucesso_supervis_0,sucesso_supervis_1,sucesso_supervis_2, sucesso_supervis_3, sucesso_supervis_4)
nrow(sucesso_supervis_total) ## 684 itens indexados com sucesso, que corresponde a 65% dos itens da base do PNAE
colnames(sucesso_supervis_total)
sucesso_supervis_total=sucesso_supervis_total%>%select(1,2)

# Juntar bases de fracasso
# fracasso na indexação (fnde_ipea_4_fracasso) + fracasso na supervisão (fracasso_supervis_0 + fracasso_supervis_1 + fracasso_supervis_2)
# essa base será indexada manualmente via excel

fracasso_supervis_0=fracasso_supervis_0%>%select(1,2)
fracasso_supervis_1=fracasso_supervis_1%>%select(1,2)
write.csv(fracasso_supervis_1, "fracasso_supervis_1.csv")
fracasso_supervis_2=fracasso_supervis_2%>%select(1,2)
fracasso_supervis_3=fracasso_supervis_3%>%select(1,2)
fracasso_supervis_4=fracasso_supervis_4%>%select(1,2)

fracasso_total=rbind(fnde_ipea_4_fracasso, fracasso_supervis_0,fracasso_supervis_1,fracasso_supervis_2,fracasso_supervis_3, fracasso_supervis_4)
nrow(fracasso_total)
unique(fracasso_total$fnde_raw)
write.csv(fracasso_total, "fracasso_total.csv")

# Após indexação manual...
fracassos_index_manual=read.csv("C:/Users/Yasmin/Documents/Projetos_GITHUB/Limpeza_PNAE/index_manual/fracassos_index_manual.csv", sep=";", de=",")
colnames(fracassos_index_manual)
nrow(fracassos_index_manual)

## Observação: alguns itens da base do PNAE são indecifráveis (ex.: cxx, monte, acc, agroin, brasileiro) e não tem correspondência com a base do ipea
## outros itens estão faltando na base do ipea (camomila, canela), e outros a primeira palavra (que foi usada nesse script) torna o indexador muito amplo (ex.: carne, que inclui carne de frango, vaca, porco...)
## portanto, na indexação manual alguns itens foram adicionados à coluna "ipea_raw", inclusive as palavras non sense do PNAE

# Juntar sucessos + indexação manual (feita em excel sobre a base fracasso_total)
dici_final=rbind(fracassos_index_manual, sucesso_supervis_total)
dici_final=distinct(dici_final, fnde_raw, ipea_raw) ## excluir eventuais sobreposições de indexação
nrow(dici_final)

# MERGE entre a base do PNAE e o dicionário que criamos
## A base do PNAE está como itens_separados, conforme script limpeza_dados_2019_tcc.R
load("C:/Users/Yasmin/Documents/Projetos_GITHUB/Limpeza_PNAE/itens_separados.Rda")
colnames(itens_separados)
itens_separados=itens_separados%>%rename("fnde_raw"="palavra1")

## Checagem de itens listados
unique(itens_separados$fnde_raw) ## 1073 itens únicos na base
unique(dici_final$fnde_raw) ## 1070 itens únicos no dicionário ... 3 itens faltantes (?) 

fnde_clean_2019=itens_separados%>%left_join(dici_final, by="fnde_raw")
nrow(fnde_clean_2019) ## 2.983.721
nrow(itens_separados) ## 2.983.329 ### a função left join adicionou 392 linhas...

# Salvar!
save(fnde_clean_2019, file="fnde_clean_2019.Rda")
save(dici_final, file="dici_final.Rda")
