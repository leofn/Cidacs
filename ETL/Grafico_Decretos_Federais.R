
library(viridis)

setwd("C:\\Users\\pc\\Desktop\\Cidacs\\ETL")


Decretos_Covid_Brasil_FED <- read.csv2("Decretos_Covid_Brasil.csv")
Decretos_Covid_Brasil_FED <- Decretos_Covid_Brasil_FED %>% filter(Tipo == "Federal")

######################## Gráficos - Decretos por mês ################################
# Recortando apenas o mês do decreto
Decretos_Covid_Brasil_FED$mes_dec <- format(as.Date(Decretos_Covid_Brasil_FED$Decretos_date), "%m")
# Nome do mês
Decretos_Covid_Brasil_FED$mes_dec <- factor(Decretos_Covid_Brasil_FED$mes_dec, levels = c("01","02","03","04",
                                                                                  "05","06","07","08","09","10","11"),
                                        labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho",
                                                   "Agosto","Setembro","Outubro","Novembro"))
# Agrupando
total_mes <- Decretos_Covid_Brasil_FED %>% 
  group_by(mes_dec) %>% 
  summarise(total=sum(n())) %>% 
  drop_na()


# gráfico
graph_mes <- ggplot(total_mes, aes(fill = mes_dec, y = total, x =mes_dec)) +
  geom_bar(position = "dodge", stat = "identity", fill="#367175") +
  geom_text(aes(label=total),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(plot.title = element_text(face="bold", size = 12),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=10,  colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=8),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=9),
        legend.position = "none") +
  labs(x = "", y="", caption = "Fonte: http://www.planalto.gov.br/ccivil_03/Portaria/quadro_portaria.htm\nDados coletados em 04/11/2020",
       title = "Total de decretos federais relacionados ao COVID-19, no Brasil, por mês. 2020.") +
  scale_y_discrete(limits=factor(0:300), breaks = c(0,50,100,150,200,250,300), name = "")



# Salvando em PNG
ggsave(plot = graph_mes, "graph_mes.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")



