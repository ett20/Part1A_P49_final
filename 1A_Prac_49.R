library(ggplot2)
library(tibble)
#library(broom)

#Scourie=tibble("Rb/Sr"=c(0.336,0.133,0.234,0.352,0.074,0.297,0.149,0.488,0.133,0.266,0.114,0.429),"87Sr/86Sr"=c(0.71395,0.70665,0.71038,0.7144,0.70458,0.71221,0.70757,0.71901,0.70683,0.71149,0.70594,0.71717))
read.csv("1A_Practical_49_Sourie_Sr.csv")

# c stands for combine or concatenate the values into a list


ggplot(data=Scourie, aes(x=Scourie$`Rb/Sr`, y=Scourie$`87Sr/86Sr`))+
  geom_point()+
  geom_smooth(method=lm)


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



Sr<-PLOT+
  geom_point(data=Scourie, aes(x=Scourie$`Rb/Sr`, y=Scourie$`87Sr/86Sr`))+
  geom_smooth(data=Scourie, aes(x=Scourie$`Rb/Sr`, y=Scourie$`87Sr/86Sr`), method=lm)+
  labs(title = "Scourie", x="Rb/Sr", y="87Sr/86Sr")

ggsave("Sr.pdf", Sr)

#lm stands for linear model

fit <- lm(`87Sr/86Sr`~`Rb/Sr`, data = Scourie)
Fit_coefficients=as_tibble(fit$coefficients)

slope=Fit_coefficients[c(2),c("value")]
intercept=Fit_coefficients[c(1),c("value")]



Age=log(slope+1)/1.42e-11
Age/1e6



Terrane_ages<-tibble("Age"=c(2770, 2330, 1690, 2626, 2630, 2550, 2850, 2260, 2390, 2460, 1749, 649, 550, 757, 776, 1002, 1030, 556, 649, 550, 757, 776, 1002, 1030, 556, 784, 590, 749, 740, 1782, 1710, 605, 1070, 1000, 1224, 1101, 1000, 626, 620, 587, 643, 667, 536, 558, 638, 690, 693, 615, 2100, 570, 2018, 582, 2100))

PLOT+
  geom_histogram(data=Terrane_ages, aes(x=Age))

