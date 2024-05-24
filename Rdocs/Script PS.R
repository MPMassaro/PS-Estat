################################################################################

########                          Preparativos                          ########


###############################    Pacotes    ##################################

require(tidyverse)
require(reshape2)
require(rcompanion)
require(nortest)

################################################################################

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


print_quadro_resumo <- function(data, var_name, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
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
df$season=factor(df$season,levels=c("1","2","3","4"),labels=c("1ª","2ª","3ª","4ª"))

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
  filter(season %in% c("1ª","2ª","3ª","4ª")) %>%
  ggplot()+
  aes(x=season,y=imdb)+
  geom_boxplot(fill=c("#A11D21"),width=0.5)+
  stat_summary(fun="mean",geom="point",shape=23,
               size=3,fill="white")+
  estat_theme()+
  labs(x="Temporada",y="Nota IMDB")+
  scale_y_continuous(breaks=c(2,3,4,5,6,7,8,9))

ggsave("análise-2.1.pdf",path="Resultados",width=158,height=93,units="mm")

require(car)

leveneTest(imdb~season,data=filter(df,season %in% c("1ª","2ª","3ª","4ª")))

df %>%
  count(season)

df %>%
  filter(season %in% c("1ª","2ª","3ª","4ª")) %>%
  group_by(season) %>%
  print_quadro_resumo(var_name="imdb")

################################################################################

#-----------------------####### Análise 3 #######-------------------------------

unique(df$setting_terrain)

df %>%
  filter(!is.na(trap_work_first))%>%
  group_by(setting_terrain) %>%
  mutate(freq=1) %>%
  summarise(freq=sum(freq)) %>%
  arrange(by=-freq) %>%
  mutate(freq_rel=format(round(freq/sum(freq)*100,2),nsmall=2))

graf3 = df %>%
  filter(!is.na(trap_work_first) & setting_terrain %in% c("Urban","Rural","Forest")) %>%
  group_by(setting_terrain,trap_work_first) %>%
  mutate(freq=1) %>%
  summarise(freq=sum(freq))
  
graf3$setting_terrain=factor(graf3$setting_terrain,levels=c("Urban","Rural","Forest"),
                             labels=c("Urbano","Rural","Floresta"))
graf3$trap_work_first=factor(graf3$trap_work_first,levels=c(TRUE,FALSE),
                             labels=c("Sim","Não"))

graf3 = graf3 %>%
  mutate(freq=freq) %>%
  group_by(setting_terrain) %>%
  mutate(freq_relativa=round(freq/sum(freq)*100,2))

porcentagens <- str_c(graf3$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(graf3$freq, " (", porcentagens, ")")
)

ggplot(graf3)+
  aes(x=setting_terrain,y=freq,fill=trap_work_first,label=legendas)+
  geom_col(position="dodge")+
  geom_text(position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3) +
  estat_theme()+
  labs(x="Terreno",y="Frequência",fill="Funcionou na primeira vez:")+
  lims(y=c(0,70))

ggsave("análise-3.1.pdf",path="Resultados",width=158,height=93,units="mm")



# Teste Qui-Quadrado

df %>%
  filter(!is.na(trap_work_first) & setting_terrain %in% c("Urban","Rural","Forest")) %>%
  group_by(setting_terrain,trap_work_first) %>%
  mutate(freq=1) %>%
  summarise(freq=sum(freq)) %>%
  dcast(setting_terrain~trap_work_first) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  select(c(`TRUE`,`FALSE`)) %>%
  as.matrix() %>%
  chisq.test()


################################################################################

#-----------------------####### Análise 4 #######-------------------------------

df %>%
  ggplot()+
  aes(x=engagement,y=imdb) %>%
  geom_point(colour="#A11D21",size=2.5,alpha=0.6)+
  estat_theme()+
  labs(y="Nota IMDb",x="Engajamento")+
  scale_y_continuous(breaks=c(2,3,4,5,6,7,8,9))

ggsave("análise-4.1.pdf",path="Resultados",width=158,height=93,units="mm")

df %>%
  ggplot()+
  aes(y=engagement)+
  geom_boxplot(fill="#A11D21")+
  estat_theme()+
  labs(y="Engajameno")+
  scale_y_continuous(breaks=c(seq(100,275,25)))

df %>%
  print_quadro_resumo("engagement")

ad.test(df$engagement)
ad.test(df$imdb)

cor(df$engagement,df$imdb)


################################################################################

#-----------------------####### Análise 5 #######-------------------------------

caught=c("caught_fred","caught_daphnie","caught_velma",
         "caught_shaggy","caught_scooby","caught_other")

graf5 = df %>%
  select(engagement,caught) %>%
  melt("engagement") %>%
  filter(value==T)

graf5 %>%
  group_by(variable) %>%
  summarise(mediana=mean(engagement))

graf5$variable=factor(graf5$variable,levels=c("caught_fred","caught_scooby",
                                              "caught_shaggy","caught_velma",
                                              "caught_daphnie","caught_other"),
                      labels=c("Fred","Scooby","Salsicha",
                               "Velma","Daphnie","Outros"))

graf5 %>%
  ggplot()+
  aes(x=variable,y=engagement)+
  geom_boxplot(fill="#A11D21",width=0.5)+
  stat_summary(fun="mean",geom="point",shape=23,size=3,fill="white")+
  estat_theme()+
  labs(x="Quem capturou",y="Engajamento")+
  scale_y_continuous(breaks=c(100,125,150,175,200,225,250))

ggsave("análise-5.1.pdf",path="Resultados",width=158,height=93,units="mm")

graf5 %>%
  group_by(variable) %>%
  print_quadro_resumo("engagement")


