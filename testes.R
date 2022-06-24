library(readxl)
library(httr)
url <- "http://dados.mj.gov.br/dataset/210b9ae2-21fc-4986-89c6-2006eb4db247/resource/feeae05e-faba-406c-8a4a-512aec91a9d1/download/indicadoressegurancapublicauf.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
segur <- read_xlsx(tf, sheet = "Ocorrências")
str(segur)
head(segur)

library(dplyr)
glimpse(segur)
unique(segur$"Tipo Crime")

library(tidyr)
segur2 <- pivot_wider(segur, names_from="Tipo Crime", values_from="Ocorrências")
str(segur2)
summary(segur2)

unique(segur2$UF)
segurRJ <- segur2 %>% filter(UF=="Rio de Janeiro", Ano==2021)
summary(segurRJ)


library(ggplot2)
ggplot(segurRJ, aes(x=as.factor(Mês), y=Estupro)) + geom_point()
segurRJ$Mês
segurRJ <- segurRJ %>% mutate( Mês =  case_when(
                                     Mês=="janeiro" ~ 1,
                                     Mês=="fevereiro" ~ 2,
                                     Mês=="março" ~ 3,
                                     Mês=="abril" ~ 4,
                                     Mês=="maio" ~ 5,
                                     Mês=="junho" ~ 6,
                                     Mês=="julho" ~ 7,
                                     Mês=="agosto" ~ 8,
                                     Mês=="setembro" ~ 9,
                                     Mês=="outubro" ~ 10,
                                     Mês=="novembro" ~ 11,
                                     Mês=="dezembro" ~ 12))
summary(segurRJ)
ggplot(segurRJ, aes(x=as.factor(Mês), y=Estupro)) + geom_point() + geom_line()
