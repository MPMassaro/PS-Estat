################################################################################

########                          Preparativos                          ########

require(tidyverse)

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


print_quadro_resumo <- function(data, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  data <- data %>%
    summarize(`Média` = round(mean(imdb),2),
              `Desvio Padrão` = round(sd(imdb),2),
              `Variância` = round(var(imdb),2),
              `Mínimo` = round(min(imdb),2),
              `1º Quartil` = round(quantile(imdb, probs = .25),2),
              `Mediana` = round(quantile(imdb, probs = .5),2),
              `3º Quartil` = round(quantile(imdb, probs = .75),2),
              `Máximo` = round(max(imdb),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}




df=read_csv("Banco\\Scooby.csv")
df=select(df,-c(1,2,3))
df$format=fct_infreq(df$format)
levels(df$format)=c("Série","Filme","CrossOver")

df$setting_terrain=fct_infreq(df$setting_terrain)
levels(df$setting_terrain)

################################################################################

#-----------------------####### Análise 1 #######-------------------------------

df = df %>%
  mutate(decada=as.character(floor(year(date_aired)/10)*10))

graf1=data.frame(
  decada=rep(unique(df$decada),each=3),
  format=rep(unique(df$format),length(unique(df$decada)))
)

n=c()
for(i in 1:21){
  dec=graf1[i,1]
  form=graf1[i,2]
  n=c(n,nrow(filter(df,decada==dec & format==form)))
}

graf1 = graf1 %>%
  mutate(n=n) %>%
  group_by(decada) %>%
  mutate(freq_relativa=round(n/sum(n)*100,1))

porcentagens <- str_c(graf1$freq_relativa, "%") %>%
  str_replace("\\.", ",")

legendas <- str_squish(str_c(graf1$n, " (", porcentagens, ")"))

ggplot(graf1)+
  aes(x=as.character(decada),y=n,fill=format,label=legendas)+
  geom_col(position=position_dodge2(preserve="single",padding=0))+
  geom_text(position = position_dodge(width = 0.9),
            vjust = -0.7, hjust = 0.1,size = 2,angle=45)+
  labs(x="Décadas",y="Número de Lançamentos",fill="Formato de Lançamento:")+
  lims(y=c(0,185))+
  estat_theme()

ggsave("análise-1.1.pdf",path="Resultados",width=158,height=93,units="mm")

ggplot(graf1)+
  aes(x=decada,y=n,group=format,colour=format) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  #geom_text(vjust = -0.5, hjust = 0.45,size = 1.575,angle=45)+
  labs(x="Décadas",y="Número de Lançamentos",color="Formato de Lançamento:")+
  estat_theme()

ggsave("análise-1.2.pdf",path="Resultados",width=158,height=93,units="mm")

graf1 %>%
  group_by(format) %>%
  summarise(n=sum(n)) %>%
  mutate(freq_relativa=n/sum(n))

tapply(graf1$n,graf1$format,sum)

################################################################################

#-----------------------####### Análise 2 #######-------------------------------

df %>%
  filter(season %in% c("1","2","3","4")) %>%
  ggplot()+
  aes(x=season,y=imdb)+
  geom_boxplot(fill=c("#A11D21"),width=0.5)+
  stat_summary(fun="mean",geom="point",shape=23,
               size=3,fill="white")+
  estat_theme()+
  labs(x="Temporada",y="Nota IMDB")

ggsave("análise-2.1.pdf",path="Resultados",width=158,height=93,units="mm")

require(car)

leveneTest(imdb~season,data=filter(df,season %in% c("1","2","3","4")))

df %>%
  count(season)

df %>%
  filter(season %in% c("1","2","3","4")) %>%
  group_by(season) %>%
  print_quadro_resumo()


################################################################################

#-----------------------####### Análise 3 #######-------------------------------

unique(df$setting_terrain)

df %>%
  group_by(setting_terrain,trap_work_first) %>%
  mutate(freq=n()) %>%
  summarise(freq=sum(freq))





