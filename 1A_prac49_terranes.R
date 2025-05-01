library(ggplot2)
library(tibble)


setwd("")
getwd()

Terrane_ages<-read.csv("U-pb_revised_data_compilation.csv")
View(Terrane_ages)

L<-subset(Terrane_ages, Terrane_ages$Terrane=="Laurentia"|Terrane_ages$Terrane=="Laurentia Hebridean")

G<-subset(Terrane_ages, Terrane_ages$Terrane=="Gondwana"|Terrane_ages$Terrane=="Gondwana N. Amorica")


PLOT<-ggplot(NULL)+
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "black", fill="NA", size=1),
        panel.grid.major = element_line(colour = "white", size=0),
        panel.grid.minor = element_line(colour = "white", size=0),
        aspect.ratio=1,
        legend.text=element_text(size=6),
        legend.title=element_text(size=6),
        #legend.key.size = unit(0.2,"line"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x =element_text(size=10,angle=0),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.title.y =element_text(size=10,angle=90),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        #plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        axis.ticks.length=unit(-0.25, "cm"),
        strip.background =element_rect(fill="white"))


PLOT+
  geom_histogram(data=L, aes(x=Age, fill=Terrane))+
  labs(title="Laurentian Terranes")


PLOT+
  geom_histogram(data=G, aes(x=Age, fill=Terrane))+
  labs(title="Gondawanan Terranes")

