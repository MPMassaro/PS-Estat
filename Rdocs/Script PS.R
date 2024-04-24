require(pacman)
p_load("tidyverse")

estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )

estat_theme <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}


setwd("D:\\Documentos\\UnB\\PS Estat\\Script")
df=read_csv("Scooby.csv")
df=select(df,-c(1,2,3))
df$format=fct_infreq(df$format)
levels(df$format)=c("Série","Filme","CrossOver")


####### Análise 1 #######

resto=data.frame(
  decada=c(1960,1960,1980,2020),
  format=c("Filme","CrossOver","CrossOver","CrossOver"),
  n=c(0,0,0,0),
  stringsAsFactors=T
)

graf1 = df %>%
  mutate(decada=floor(year(date_aired)/10)*10,n=1) %>%
  group_by(decada,format) %>%
  summarise(n=sum(n)) %>%
  rbind(resto) %>%
  mutate(freq_relativa=round(n/sum(n)*100,1))

porcentagens <- str_c(graf1$freq_relativa, "%") %>%
  str_replace("\\.", ",")

legendas <- str_squish(str_c(graf1$n, " (", porcentagens, ")"))

ggplot(graf1)+
  aes(x=as.character(decada),y=n,fill=format,label=legendas)+
  geom_col(position=position_dodge2(preserve="single",padding=0))+
  geom_text(position = position_dodge(width = 0.9),
            vjust = -0.5, hjust = 0.5,size = 2.4)+
  labs(x="Décadas",y="Número de Lançamentos",fill="Formato de Lançamento:")+
  estat_theme()

ggsave("análise-1.1.pdf",path="D:\\Documentos\\UnB\\PS Estat\\Imagens",
       width=300,height=176,units="mm")

