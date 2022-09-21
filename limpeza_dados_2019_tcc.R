# BASE DE DADOS - TCC - COMPRAS AF 2019 BR
# LIMPEZA DE DADOS

library(writexl)
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("lubridate")
library(lubridate)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("fuzzyjoin")
library(fuzzyjoin)

######
## BASE DO FNDE 2019
######
fnde_2019_001 = fnde_geocod %>% dplyr::filter(ano==2019)

fnde_2019_unique = unique(fnde_2019_001$item)
length(fnde_2019_unique)

# Tratamento inicial - retirar termos frequentes
## Retirar números
fnde_2019_001$item = gsub("[0-9]","", fnde_2019_001$item)

unique(fnde_2019_001$item)

## Retirar: xkg, xml, xg, xgs, x tp , cxkg, x kg, x gg
fnde_2019_001$item = gsub(" xkg", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" xml", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" xg", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" xgs", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" x tp ", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" cxkg", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" x gg", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" x kg", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" x", "", fnde_2019_001$item)


## Retirar: kg,  kg, g, ml, fd, un, qtd, pct, c, gr, cx, sh, cp, gx, 
fnde_2019_001$item = gsub(" kg", "", fnde_2019_001$item)
fnde_2019_001$item = gsub("  kg", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" ml", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" fd", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" un", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" qtd", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" pct", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" gr", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" cx", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" sh", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" gx", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" cp", "", fnde_2019_001$item)

unique(fnde_2019_001$item)

## Retirar: lt, r, rs, d, rs, rsd, tp , desc, pic, x
fnde_2019_001$item = gsub(" lt", "", fnde_2019_001$item)
de_2019_001$item = gsub(" rs", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" rsd", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" tp ", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" tp", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" desc", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" pic", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" kq", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" g ", "", fnde_2019_001$item)


fnde_2019_001$item = gsub(" d$", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" r$", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" g$", "", fnde_2019_001$item)
fnde_2019_001$item = gsub(" c$", "", fnde_2019_001$item)

unique(fnde_2019_001$item) ## 70 mil itens

fnde_2019_002 = fnde_2019_001
head(fnde_2019_002)
unique(fnde_2019_002$item)

#Salvar base 
save(fnde_2019_002, file = 'fnde_2019_002.Rda')

# Selecionar apenas fornecedores com informação do município de origem (TCC/Yasmin)
fnde_2019_003=fnde_2019_002%>%filter(fornmunicipio_uf != "na_na") ## redução para 12.000 itens únicos

# Unir algumas palavras duplas
fnde_2019_003$item=stringr::str_replace_all(fnde_2019_003$item, "cheiro verde", "cheiroverde")
fnde_2019_003$item=stringr::str_replace_all(fnde_2019_003$item, "couve flor", "couveflor")
fnde_2019_003$item=stringr::str_replace_all(fnde_2019_003$item, "milho pipoca", "milhopipoca")
fnde_2019_003$item=stringr::str_replace_all(fnde_2019_003$item, "bebida lactea", "bebidalactea")

# Separar os itens -> trataremos apenas a primeira palavra, que geralmente já contem a informação do item
itens_separados=separate(fnde_2019_003, item, into = c("palavra1", "palavra2", "palavra3"), sep=" ")
head(itens_separados)
unique(itens_separados$palavra1) ## redução para 1.000 itens únicos, considerando primeira palavra apenas

save(itens_separados, file="itens_separados.Rda")
matriz=matrix(unique(itens_separados$fnde_palavra1))
dicfnde=as.data.frame(matriz)
View(dicfnde)
colnames(dicfnde)=c("fnde_raw")
save(dicfnde, file="dicfnde.Rda")

######
## TABELA DE REFERÊNCIA DO IPEA
######

ref_ipea=read_excel("C:/Users/Yasmin/Documents/NEPA-TCC/Bases de dados FNDE - SIGPC/referencia_IPEA_limpeza_fnde.xlsx", sheet=2)
head(ref_ipea)

# Letras minúsculas
ref_ipea$Produto_simplificado=tolower(ref_ipea$Produto_simplificado)

# Unir algumas palavras duplas
ref_ipea$Produto_simplificado=stringr::str_replace_all(ref_ipea$Produto_simplificado, "bebida lactea", "bebidalactea")
ref_ipea$Produto_simplificado=stringr::str_replace_all(ref_ipea$Produto_simplificado, "cheiro verde", "cheiroverde")
ref_ipea$Produto_simplificado=stringr::str_replace_all(ref_ipea$Produto_simplificado, "couve flor", "couveflor")
ref_ipea$Produto_simplificado=stringr::str_replace_all(ref_ipea$Produto_simplificado, "milho pipoca", "milhopipoca")

unique(ref_ipea$Produto_simplificado)

# Separar os itens -> trataremos apenas a primeira palavra
ref_ipea=separate(ref_ipea, Produto_simplificado, into = c("palavra1", "palavra2", "palavra3"), sep=" ")
head(ref_ipea)
unique(ref_ipea$palavra1)

