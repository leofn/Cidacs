
library(tm)
library(dplyr)
library(tidyr)
library(purrr)
library(tidyverse)
library(xml2)
library(stringr)
library(rvest)
library(SnowballC)
library(wordcloud)
library(webshot)

palavras <- c("de",	"a",	"o",	"que",	"e",	"do",	"da",	"em",	"um",	"para",	"é",
              "com",	"não",	"uma",	"os",	"no",	"se",	"na",	"por",	"mais",	"as",
              "dos",	"como",	"mas",	"foi",	"ao",	"ele",	"das",	"tem",	"à",	"seu",
              "sua",	"ou",	"ser",	"quando",	"muito",	"há",	"nos",	"já",	"está",	"eu",
              "também",	"só",	"pelo",	"pela",	"até",	"isso",	"ela",	"entre",	"era",
              "depois",	"sem",	"mesmo",	"aos",	"ter",	"seus",	"quem",	"nas",	"me",
              "esse",	"eles",	"estão",	"você",	"tinha",	"foram",	"essa",	"num",	"nem",
              "suas",	"meu",	"às",	"minha",	"têm",	"numa",	"pelos",	"elas",	"havia",
              "seja",	"qual",	"será",	"nós",	"tenho",	"lhe",	"deles",	"essas",
              "esses",	"pelas",	"este",	"fosse",	"dele",	"tu",	"te",	"vocês",	"vos",
              "lhes",	"meus",	"minhas",	"teu",	"tua",	"teus",	"tuas",	"nosso",	"nossa",
              "nossos",	"nossas",	"dela",	"delas",	"esta",	"estes",	"estas",
              "aquele",	"aquela",	"aqueles",	"aquelas",	"isto",	"aquilo",	"estou",
              "está",	"estamos",	"estão",	"estive",	"esteve",	"estivemos",	"estiveram",
              "estava",	"estávamos",	"estavam",	"estivera",	"estivéramos",	"esteja",
              "estejamos",	"estejam",	"estivesse",	"estivéssemos",	"estivessem",	
              "estiver",	"estivermos",	"estiverem",	"hei",	"há",	"havemos",	"hão",
              "houve",	"houvemos",	"houveram",	"houvera",	"houvéramos",	"haja",
              "hajamos",	"hajam",	"houvesse",	"houvéssemos",	"houvessem",	"houver",
              "houvermos",	"houverem",	"houverei",	"houverá",	"houveremos",	"houverão",
              "houveria",	"houveríamos",	"houveriam",	"sou",	"somos",	"são",
              "era",	"éramos",	"eram",	"fui",	"foi",	"fomos",	"foram",	"fora",
              "fôramos",	"seja",	"sejamos",	"sejam",	"fosse",	"fôssemos",	"fossem",
              "for",	"formos",	"forem",	"serei",	"será",	"seremos",	"serão",	"seria",
              "seríamos",	"seriam",	"tenho",	"tem",	"temos",	"tém",	"tinha",
              "tínhamos",	"tinham",	"tive",	"teve",	"tivemos",	"tiveram",	"tivera",
              "tivéramos",	"tenha",	"tenhamos",	"tenham",	"tivesse",	"tivéssemos",
              "tivessem",	"tiver",	"tivermos",	"tiverem",	"terei",	"terá",	"teremos",
              "terão",	"teria",	"teríamos",	"teriam")

# caminho
setwd("C:\\Users\\pc\\Desktop\\Cidacs\\ETL")

# dataset
Decretos_Covid_Brasil_FED <- read.csv2("Decretos_Covid_Brasil.csv")
Decretos_Covid_Brasil_FED <- Decretos_Covid_Brasil_FED %>% filter(Tipo == "Federal")

# ######################## Gráficos - Decretos por mês ################################
# # Recortando apenas o mês do decreto
# Decretos_Covid_Brasil_FED$mes_dec <- format(as.Date(Decretos_Covid_Brasil_FED$Decretos_date), "%m")
# # Nome do mês
# Decretos_Covid_Brasil_FED$mes_dec <- factor(Decretos_Covid_Brasil_FED$mes_dec, levels = c("01","02","03","04",
#                                                                                           "05","06","07","08","09","10","11"),
#                                             labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho",
#                                                        "Agosto","Setembro","Outubro","Novembro"))
# 
# 
# Decretos_Covid_Brasil_FED_Março <- Decretos_Covid_Brasil_FED %>% filter(mes_dec=="Março")
# Decretos_Covid_Brasil_FED_Maio <- Decretos_Covid_Brasil_FED %>% filter(mes_dec=="Maio")
# Decretos_Covid_Brasil_FED_Junho <- Decretos_Covid_Brasil_FED %>% filter(mes_dec=="Junho")
# Decretos_Covid_Brasil_FED_Julho <- Decretos_Covid_Brasil_FED %>% filter(mes_dec=="Julho")
# Decretos_Covid_Brasil_FED_Agosto <- Decretos_Covid_Brasil_FED %>% filter(mes_dec=="Agosto")
# Decretos_Covid_Brasil_FED_Setembro <- Decretos_Covid_Brasil_FED %>% filter(mes_dec=="Setembro")
# Decretos_Covid_Brasil_FED_Outubro <- Decretos_Covid_Brasil_FED %>% filter(mes_dec=="Outubro")

# selecionando apenas ementas
Ementa <- Decretos_Covid_Brasil_FED %>% select(Ementa)
# Ementa_abril <- Decretos_Covid_Brasil_FED_Abril %>% select(Ementa)
# Ementa_março <- Decretos_Covid_Brasil_FED_Março %>% select(Ementa)
# Ementa_Maio <- Decretos_Covid_Brasil_FED_Maio %>% select(Ementa)
# Ementa_Junho <- Decretos_Covid_Brasil_FED_Junho %>% select(Ementa)
# Ementa_Julho <- Decretos_Covid_Brasil_FED_Julho %>% select(Ementa)
# Ementa_Agosto <- Decretos_Covid_Brasil_FED_Agosto %>% select(Ementa)
# Ementa_Setembro <- Decretos_Covid_Brasil_FED_Setembro %>% select(Ementa)
# Ementa_Outubro <- Decretos_Covid_Brasil_FED_Outubro %>% select(Ementa)




# convertendo em caracter
# Ementa_março <- as.character(Ementa_março$Ementa)
# Ementa_abril <- as.character(Ementa_abril$Ementa)
# Ementa_Maio <- as.character(Ementa_Maio$Ementa)
# Ementa_Junho <- as.character(Ementa_Junho$Ementa)
# Ementa_Julho <- as.character(Ementa_Julho$Ementa)
# Ementa_Agosto <- as.character(Ementa_Agosto$Ementa)
# Ementa_Setembro <- as.character(Ementa_Setembro$Ementa)
# Ementa_Outubro <- as.character(Ementa_Outubro$Ementa)
Ementa <- as.character(Ementa$Ementa)



# convertendo em Corpus
auxCorpus <- Corpus(VectorSource(Ementa)) #Corpus
# auxCorpus_abril <- Corpus(VectorSource(Ementa_abril)) #Corpus
# auxCorpus_março <- Corpus(VectorSource(Ementa_março)) #Corpus
# auxCorpus_maio <- Corpus(VectorSource(Ementa_Maio)) #Corpus
# auxCorpus_junho <- Corpus(VectorSource(Ementa_Junho)) #Corpus
# auxCorpus_julho <- Corpus(VectorSource(Ementa_Julho)) #Corpus
# auxCorpus_agosto <- Corpus(VectorSource(Ementa_Agosto)) #Corpus
# auxCorpus_setembro <- Corpus(VectorSource(Ementa_Setembro)) #Corpus
# auxCorpus_outubro <- Corpus(VectorSource(Ementa_Outubro)) #Corpus



# eliminando pontuacao
auxCorpus <- tm_map(auxCorpus, removePunctuation)
# auxCorpus_abril <- tm_map(auxCorpus_abril, removePunctuation)
# auxCorpus_março <- tm_map(auxCorpus_março, removePunctuation)
# auxCorpus_maio <- tm_map(auxCorpus_maio, removePunctuation)
# auxCorpus_junho <- tm_map(auxCorpus_junho, removePunctuation)
# auxCorpus_julho <- tm_map(auxCorpus_julho, removePunctuation)
# auxCorpus_agosto <- tm_map(auxCorpus_agosto, removePunctuation)
# auxCorpus_setembro <- tm_map(auxCorpus_setembro, removePunctuation)
# auxCorpus_outubro <- tm_map(auxCorpus_outubro, removePunctuation)




# eliminando espacos
auxCorpus <- tm_map(auxCorpus, stripWhitespace) 
# auxCorpus_abril <- tm_map(auxCorpus_abril, stripWhitespace) 
# auxCorpus_março <- tm_map(auxCorpus_março, stripWhitespace) 
# auxCorpus_maio <- tm_map(auxCorpus_maio, stripWhitespace) 
# auxCorpus_junho <- tm_map(auxCorpus_junho, stripWhitespace) 
# auxCorpus_julho <- tm_map(auxCorpus_julho, stripWhitespace) 
# auxCorpus_agosto <- tm_map(auxCorpus_agosto, stripWhitespace) 
# auxCorpus_setembro <- tm_map(auxCorpus_setembro, stripWhitespace) 
# auxCorpus_outubro <- tm_map(auxCorpus_outubro, stripWhitespace) 




# retirando preposições
auxCorpus <- tm_map(auxCorpus, removeWords, stopwords("pt"))
# auxCorpus_abril <- tm_map(auxCorpus_abril, removeWords, stopwords("pt"))
# auxCorpus_março <- tm_map(auxCorpus_março, removeWords, stopwords("pt"))
# auxCorpus_maio <- tm_map(auxCorpus_maio, stripWhitespace) 
# auxCorpus_junho <- tm_map(auxCorpus_junho, stripWhitespace) 
# auxCorpus_julho <- tm_map(auxCorpus_julho, stripWhitespace) 
# auxCorpus_agosto <- tm_map(auxCorpus_agosto, stripWhitespace) 
# auxCorpus_setembro <- tm_map(auxCorpus_setembro, stripWhitespace) 
# auxCorpus_outubro <- tm_map(auxCorpus_outubro, stripWhitespace) 


# layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5, y=0.5, "Abril")
# wordcloud(auxCorpus_abril,main="Title",colors=brewer.pal(3, "Dark2"),max.words=200,size = 0.7, shape = 'pentagon', main="Title")


# criando nuvem
 wordcloud(auxCorpus,colors=brewer.pal(3, "Dark2"),max.words=200,size = 1, shape = 'pentagon')
 # abril <- wordcloud(auxCorpus_abril,main="Title",colors=brewer.pal(3, "Dark2"),max.words=200,size = 0.7, shape = 'pentagon')
 # março <- wordcloud(auxCorpus_março,colors=brewer.pal(3, "Dark2"),max.words=200,size = 0.7, shape = 'pentagon')
 # maio <- wordcloud(auxCorpus_maio,colors=brewer.pal(3, "Dark2"),max.words=200,size = 0.7, shape = 'pentagon')
 # junho <- wordcloud(auxCorpus_junho,colors=brewer.pal(3, "Dark2"),max.words=200,size = 0.7, shape = 'pentagon')
 # julho <- wordcloud(auxCorpus_julho,colors=brewer.pal(3, "Dark2"),max.words=200,size = 0.7, shape = 'pentagon')
 # agosto <- wordcloud(auxCorpus_agosto,colors=brewer.pal(3, "Dark2"),max.words=200,size = 0.7, shape = 'pentagon')
 # setembro <- wordcloud(auxCorpus_setembro,colors=brewer.pal(3, "Dark2"),max.words=200,size = 0.7, shape = 'pentagon')
 # outubro <- wordcloud(auxCorpus_outubro,colors=brewer.pal(3, "Dark2"),max.words=200,size = 0.7, shape = 'pentagon')
 # 

#saveWidget(nuvem,"nuvem.html",selfcontained = F)
#webshot::webshot("nuvem.html","nuvem.png",vwidth = 1992, vheight = 1744, delay =10)
# 
# par(mfrow=c(1,2)) # for 1 row, 2 cols
# wordcloud(d1$word, d1$freq, max.words=100)
# wordcloud(d2$word, d2$freq, max.words=100)



