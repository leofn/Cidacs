

library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)
library(stringr)

#Pasta de Trabalho
setwd("C:\\Users\\pc\\Desktop\\Cidacs\\Decretos")

# Dataset
dec_ba <- read_excel("Decretos_em_Salvador_05082020_v2.xlsx",sheet = 1)

dec_ba$Data <- gsub(".*de (.+) de.*", "\\1", dec_ba$Data)

# Criando coluna para contagem e mês
dec_ba <- dec_ba %>% mutate(n = 1)
#dec_ba <- dec_ba %>% mutate(month = format(Data, "%m"))


############## Decretos SSA - Por Tipo ############
Conteudo_ssa_tipo <-  dec_ba %>% filter(Codigo_Municipio == "2927408") %>% 
  group_by(`Conteúdo`) %>% summarise(total_decs = sum(n))


# gráfico
ggplot(Conteudo_ssa_tipo, aes(fill = `Conteúdo`, y = total_decs, x =reorder(`Conteúdo`, +total_decs))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=total_decs),size = 3, position =position_dodge(width=0.9),
            hjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  coord_flip() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "",
        legend.background = element_rect(fill="ghostwhite",size=0.7, linetype="blank")) +
  labs(caption = "Fonte: https://leismunicipais.com.br/\nDados coletados em 05/08/2020 ",x = "Conteúdo", fill = "Tipo do Conteúdo: ", y = "Quantidade",
       title = "Contéudo dos decretos - Salvador/BA") +
  scale_fill_viridis(discrete=TRUE,option = "B")


############## Decretos SSA - Por Data ############

Conteudo_ssa_data <-  dec_ba %>% filter(Codigo_Municipio == "2927408") %>% 
  group_by(`Conteúdo`, Data) %>% summarise(total_decs = sum(n)) #%>% mutate(mes_escrito = case_when(
#  Data == "03" ~ "março",
# Data == "04" ~ "abril",
#Data == "05" ~ "maio",
#Data == "06" ~ "junho",
#Data == "07" ~ "julho",
#Data == "08" ~ "agosto"
#))

# ordendando o eixo do mês
Conteudo_ssa_data$Data <- factor(Conteudo_ssa_data$Data,
                                 levels = c("março","abril","maio","junho","julho","agosto"))



# gráfico
ggplot(Conteudo_ssa_data, aes(fill = `Conteúdo`, y = total_decs, x =`Conteúdo`)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  geom_text(aes(label=total_decs),size = 2, position =position_dodge(width=0.9),
            hjust=-0.5, color = 'black',fontface='bold') +
  facet_wrap( ~ Data, ncol = 2) +
  theme_classic() +
  coord_flip() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=6),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite",size=0.7, linetype="blank")) +
  labs(caption = "Fonte: https://leismunicipais.com.br/\nDados coletados em 05/08/2020 ",x = "Conteúdo", fill = "Tipo do Conteúdo: ", y = "Quantidade",
       title = "Contéudo dos decretos, por mês - Salvador/BA")
