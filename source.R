library(tidyverse)
library(reshape2)
options(repr.plot.width=400, repr.plot.height = 100)
data = list.files(path='data', pattern = '*.csv', full.names="TRUE", recursive="FALSE")
df_list = list()

for (i in 1:length(data)) {
  dataframe = read.csv(data[[i]], sep=";")
  df_list[[i]] = dataframe
}

df = do.call(rbind, df_list)
df = subset(df, select = -c(Kod, Cena.i.wskaźniki, Jednostka.miary, Atrybut, X))

grouped = group_by(df, Rodzaje.towarów.i.usług, Nazwa)

ordered = grouped[order(grouped$Nazwa, grouped$Rok, grouped$Rodzaje.towarów.i.usług), ]
ordered = transform(ordered, Wartosc = as.numeric(sub(',', '.', as.character(Wartosc))))
ggplot()

for (voivodeship in unique(ordered$Nazwa)) {
  subdata <- ordered[ordered$Nazwa == voivodeship, ]
    products = c()
    meanPrice = c()
    year = c();
  for (product in unique(subdata$Rodzaje.towarów.i.usług)) {
    productSubdata = subdata[subdata$Rodzaje.towarów.i.usług == product, ]
    for (yr in unique(productSubdata$Rok)) {
      data = productSubdata[productSubdata$Rok == yr, ]
      mv = mean(data$Wartosc)
      products = c(products, product)
      year = c(year, yr)
      meanPrice = c(meanPrice, mv)
    }
  }
    
  data = data.frame(products, year, meanPrice)
  ggplot(data=data, aes(x=year)) + 
    geom_line(aes(y=meanPrice, col=products))+
    labs(title="Średnie ceny wybranych produktów w latach 2006-2019",
         subtitle=paste("Województwo", voivodeship),
         x="Rok",
         y="Średnia cena produktu/usługi") +
    theme(legend.position = "bottom")
   ggsave(paste(voivodeship, "png", sep="."), scale=4)
}

voivodeships = c()
products = c()
meanPrice = c()
deviation = c()
year = c();
for (product in unique(ordered$Rodzaje.towarów.i.usług)) {
  subdata <- ordered[ordered$Rodzaje.towarów.i.usług == product, ]
  for (voivodeship in unique(subdata$Nazwa)) {
    voivSubdata = subdata[subdata$Nazwa == voivodeship, ]
    for (yr in unique(voivSubdata$Rok)) {
      data = voivSubdata[voivSubdata$Rok == yr, ]
      mv = mean(data$Wartosc)
      sd = sd(data$Wartosc)
      voivodeships = c(voivodeships, voivodeship)
      products = c(products, product)
      year = c(year, yr)
      meanPrice = c(meanPrice, mv)
      deviation = c(deviation, sd)
    }
  }
  
  data = data.frame(voivodeships, products, year, meanPrice, deviation)
  ggplot(data, aes(x=products, y=meanPrice, fill=voivodeships)) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=meanPrice-deviation, ymax=meanPrice+deviation), width=.2,
                  position=position_dodge(.9)) +
    labs(title="Średnie ceny wybranych produktów w latach 2006-2019",
         x="Produkt",
         y="Średnia cena produktu/usługi") + 
    theme(axis.text.x = element_text(angle=45, hjust=1))
}
ggsave("Średnie wartości produktów lub usług w poszczególnych województwach.png", scale=4)
