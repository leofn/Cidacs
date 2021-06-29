
library(tidyverse)
library(Cairo)

setwd("C:\\Users\\pc\\Desktop\\Cidacs\\Grafico Rafael")


paleta <- c("#ffcccc", "#ff6666", "#ff3232", "#ff0800")


##########################################################################################
##################### Excesso de mortes por decil do IBP ---------------------------------
##########################################################################################

mortalide_decil_ibp <- read.csv("excesso_mortal_decil_ibp_bra_2015_2020.csv")

# mortalide_ibp_resum <- mortalide_decil_ibp %>%
#                        group_by(MES_OBITO, DECIL_IBP) %>%
#                        summarise(media_excesso = mean(`X..EXCESSO_MORTALIDADE_MORT_M1`,
#                                                       na.rm = TRUE))


mortalide_ibp_resum <- mortalide_decil_ibp %>% dplyr::filter(FAIXA_ETARIA == "TOTAL")



mortalidade_uf <- read.csv("excesso_mortal_uf_bra_2015_2020.csv")

# paleta
#paleta <- c("#d0efff", "#187bcd", "#1167b1", "#03254c")


# gráfico
graph_mort_ibp <- ggplot(mortalide_ibp_resum, aes(y = MES_OBITO, x = reorder(DECIL_IBP, +DECIL_IBP))) +   
  geom_tile(aes(fill = `X..EXCESSO_MORTALIDADE_MORT_M1`)) + 
  geom_text(aes(label = format(round(`X..EXCESSO_MORTALIDADE_MORT_M1`, 2),big.mark = ".", decimal.mark=",")),
            colour="black", size = 4) +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y = element_text(color="#000000", size=13),
        axis.text.x = element_text(color="#000000", size=13),
        axis.title.x = element_text(size = 14,  colour = "black"),
        axis.line = element_line(colour = "white", 
                                 size = 0, linetype = "solid"),
        plot.title = element_text(face="bold", colour = "black", size = 18, hjust=0.5),
        plot.caption = element_text(size = 12),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  scale_y_continuous(breaks = seq(1,12,1), labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                                      "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  labs(x="Decil do IBP",y="", color="", title = "Média do excesso de mortes, por mês, de acordo com o decil do IBP. 2020.",
       caption = "Elaboração: Cidacs/Fiocruz", fill="Média do excesso de mortes: ") +
  scale_fill_gradientn(colours = paleta)


### Salvando em PNG ###
ggsave(plot = graph_mort_ibp, "graph_mort_ibp.png",
       width = 13, height = 6.5, dpi = 200, units = "in",type = "cairo-png")

##########################################################################################
##################### Excesso de mortes por UF -------------------------------------------
##########################################################################################


mortalide_uf <- read.csv("excesso_mortal_uf_bra_2015_2020.csv")


mortalide_uf_resum <- mortalide_uf %>% dplyr::filter(FAIXA_ETARIA == "TOTAL")


mortalide_uf_resum <- mortalide_uf_resum %>% mutate(Estado = case_when(CODUFRES == 11 ~ "Rondônia" , 
                                                CODUFRES == 12 ~ "Acre" , 
                                                CODUFRES == 13 ~ "Amazonas" , 
                                                CODUFRES == 14 ~ "Roraima" , 
                                                CODUFRES == 15 ~ "Pará" , 
                                                CODUFRES == 16 ~ "Amapá" , 
                                                CODUFRES == 17 ~ "Tocantins" , 
                                                CODUFRES == 21 ~ "Maranhão" , 
                                                CODUFRES == 22 ~ "Piauí" , 
                                                CODUFRES == 23 ~ "Ceará" , 
                                                CODUFRES == 24 ~ "Rio Grande do Norte" , 
                                                CODUFRES == 25 ~ "Paraíba" , 
                                                CODUFRES == 26 ~ "Pernambuco" , 
                                                CODUFRES == 27 ~ "Alagoas" , 
                                                CODUFRES == 28 ~ "Sergipe" , 
                                                CODUFRES == 29 ~ "Bahia" , 
                                                CODUFRES == 31 ~ "Minas Gerais" , 
                                                CODUFRES == 32 ~ "Espírito Santo" , 
                                                CODUFRES == 33 ~ "Rio de Janeiro" , 
                                                CODUFRES == 35 ~ "São Paulo" , 
                                                CODUFRES == 41 ~ "Paraná" , 
                                                CODUFRES == 42 ~ "Santa Catarina" , 
                                                CODUFRES == 43 ~ "Rio Grande do Sul" , 
                                                CODUFRES == 50 ~ "Mato Grosso do Sul" , 
                                                CODUFRES == 51 ~ "Mato Grosso" , 
                                                CODUFRES == 52 ~ "Goiás" , 
                                                CODUFRES == 53 ~ "Distrito Federal"),
                            Regiao = case_when(CODUFRES == 11 ~ "Norte",
                                               CODUFRES == 12 ~ "Norte" , 
                                               CODUFRES == 13 ~ "Norte" , 
                                               CODUFRES == 14 ~ "Norte" , 
                                               CODUFRES == 15 ~ "Norte" , 
                                               CODUFRES == 16 ~ "Norte" , 
                                               CODUFRES == 17 ~ "Norte" , 
                                               CODUFRES == 21 ~ "Nordeste" , 
                                               CODUFRES == 22 ~ "Nordeste" , 
                                               CODUFRES == 23 ~ "Nordeste" , 
                                               CODUFRES == 24 ~ "Nordeste" , 
                                               CODUFRES == 25 ~ "Nordeste" , 
                                               CODUFRES == 26 ~ "Nordeste" , 
                                               CODUFRES == 27 ~ "Nordeste" , 
                                               CODUFRES == 28 ~ "Nordeste" , 
                                               CODUFRES == 29 ~ "Nordeste" ,
                                               CODUFRES == 31 ~ "Sudeste" , 
                                               CODUFRES == 32 ~ "Sudeste" , 
                                               CODUFRES == 33 ~ "Sudeste" , 
                                               CODUFRES == 35 ~ "Sudeste" ,
                                               CODUFRES == 41 ~ "Sul" , 
                                               CODUFRES == 42 ~ "Sul" , 
                                               CODUFRES == 43 ~ "Sul" ,
                                               CODUFRES == 50 ~ "Centro-Oeste" , 
                                               CODUFRES == 51 ~ "Centro-Oeste" , 
                                               CODUFRES == 52 ~ "Centro-Oeste" , 
                                               CODUFRES == 53 ~ "Centro-Oeste"))



############# Por UF --------------------------------


graph_mort_uf <- ggplot(mortalide_uf_resum, aes(y = MES_OBITO, x = reorder(Estado, +`X..EXCESSO_MORTALIDADE_MORT_M1`))) +   
  geom_tile(aes(fill = `X..EXCESSO_MORTALIDADE_MORT_M1`)) + 
  geom_text(aes(label = format(round(`X..EXCESSO_MORTALIDADE_MORT_M1`, 2),big.mark = ".", decimal.mark=",")),
            colour="black", size = 4) +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y = element_text(color="#000000", size=13),
        axis.text.x = element_text(color="#000000", size=13),
        axis.title.x = element_text(size = 14,  colour = "black"),
        axis.line = element_line(colour = "white", 
                                 size = 0, linetype = "solid"),
        plot.title = element_text(face="bold", colour = "black", size = 18, hjust=0.5),
        plot.caption = element_text(size = 12),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  scale_y_continuous(breaks = seq(1,12,1), labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                                      "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  labs(x="",y="", color="", title = "Média do excesso de mortes nas Unidades da Federação, por mês. 2020.",
       caption = "Elaboração: Cidacs/Fiocruz", fill="Média do excesso de mortes: ") +
  scale_fill_gradientn(colours = paleta)


ggsave(plot = graph_mort_uf, "graph_mort_uf.png",
       width = 17, height = 8,dpi = 200, units = "in",type = "cairo-png")


############# Por região --------------------------------


mortalide_uf_resum_regiao <- mortalide_uf_resum %>%
  group_by(MES_OBITO, Regiao) %>%
  summarise(media_mortalidade = mean(`X..EXCESSO_MORTALIDADE_MORT_M1`, na.rm=TRUE)) %>% 
  as.data.frame()


graph_mort_regiao <- ggplot(mortalide_uf_resum_regiao, aes(y = MES_OBITO, x = reorder(Regiao, +media_mortalidade))) +   
  geom_tile(aes(fill = media_mortalidade)) + 
  geom_text(aes(label = format(round(media_mortalidade, 2),big.mark = ".", decimal.mark=",")),
            colour="black", size = 4.5) +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y = element_text(color="#000000", size=13),
        axis.text.x = element_text(color="#000000", size=13),
        axis.title.x = element_text(size = 14,  colour = "black"),
        axis.line = element_line(colour = "white", 
                                 size = 0, linetype = "solid"),
        plot.title = element_text(face="bold", colour = "black", size = 18, hjust=0.5),
        plot.caption = element_text(size = 12),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  scale_y_continuous(breaks = seq(1,12,1), labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                                      "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  labs(x="",y="", color="", title = "Média do excesso de mortes nas Regiões, por mês. 2020.",
       caption = "Elaboração: Cidacs/Fiocruz", fill="Média do excesso de mortes: ") +
  scale_fill_gradientn(colours = paleta)


ggsave(plot = graph_mort_regiao, "graph_mort_regiao.png",
       width = 12, height = 6,dpi = 200, units = "in",type = "cairo-png")


##########################################################################################
##################### Excesso de mortes por municipio ------------------------------------
##########################################################################################

library(geobr)
library(data.table)

mun <- read_municipality(code_muni="all", year=2020)

estado <- read_state(code_state = "all", year = 2020)


class(mun)
str(mun)


mun$code_muni <- str_sub(mun$code_muni, 1, str_length(mun$code_muni)-1)
mun$code_muni <- as.numeric(mun$code_muni)


mortalide_mun <- fread("excesso_mortal_mun_bra_2015_2020.csv", sep = ",")


mortalide_mun_resum <- mortalide_mun %>%
                       dplyr::filter(FAIXA_ETARIA == "TOTAL" & NOMEMUNRES != "DESCONHECIDO") %>% 
                       mutate(mes = case_when(MES_OBITO == 1 ~ "Janeiro",
                                              MES_OBITO == 2 ~ "Fevereiro",
                                              MES_OBITO == 3 ~ "Março",
                                              MES_OBITO == 4 ~ "Abril",
                                              MES_OBITO == 5 ~ "Maio",
                                              MES_OBITO == 6 ~ "Junho",
                                              MES_OBITO == 7 ~ "Julho",
                                              MES_OBITO == 8 ~ "Agosto",
                                              MES_OBITO == 9 ~ "Setembro",
                                              MES_OBITO == 10 ~ "Outubro",
                                              MES_OBITO == 11 ~ "Novembro",
                                              MES_OBITO == 12 ~ "Dezembro"))



dataset_all <- left_join(mun, mortalide_mun_resum, by=c("code_muni"="CODMUNRES"))

dataset_all$mes <- factor(dataset_all$mes, levels = c("Janeiro",
                                                      "Fevereiro",
                                                      "Março",
                                                      "Abril",
                                                      "Maio",
                                                      "Junho",
                                                      "Julho",
                                                      "Agosto",
                                                      "Setembro",
                                                      "Outubro",
                                                      "Novembro",
                                                      "Dezembro"))


paleta_MAPA <- c("#ffcccc", "#ff3232", "#ff0800")



all_mun_graph <- ggplot() +
  geom_sf(data=dataset_all, aes(fill=`% EXCESSO_MORTALIDADE_MORT_M1`), color= NA, size=.15)+
  geom_sf(data=estado,size=.15, fill = NA, color = "black", show.legend = FALSE) +
  labs(color="", title = "Média do excesso de mortes nos municípios, por mês. 2020.",
       caption = "Elaboração: Cidacs/Fiocruz", fill="Média do\n excesso de\n mortes: ") +
  theme(plot.title = element_text(face="bold", colour = "black", size = 18, hjust=0.5),
        plot.caption = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(fill="ghostwhite",
                                         size=0.7,
                                         linetype="blank")) +
  scale_fill_gradientn(colours = paleta_MAPA) +
  theme_minimal() +
  facet_wrap(~mes, ncol = 4)


ggsave(plot = all_mun_graph, "all_mun_graph.png",
       width = 18, height = 9 ,dpi = 200, units = "in",type = "cairo-png")


ggsave(plot = all_mun_graph, "all_mun_graph2.png",
       width = 11, height = 22,dpi = 200, units = "in",type = "cairo-png")




