library(ggplot2)
library(tibble)

#setwd("")
Scourie<-read.csv("1A_Practical_49_Sourie_Sr.csv")

# c stands for combine or concatenate the values into a list


ggplot(data=Scourie, aes(x=Scourie$`X87Rb.86Sr`, y=Scourie$`X87Sr.86Sr`))+
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



PLOT+
  geom_point(data=Scourie, aes(x=Scourie$`X87Rb.86Sr`, y=Scourie$`X87Sr.86Sr`))+
  geom_smooth(data=Scourie, aes(x=Scourie$`X87Rb.86Sr`, y=Scourie$`X87Sr.86Sr`), method=lm)+
  labs(title = "Scourie", x="Rb/Sr", y="87Sr/86Sr")



#lm stands for linear model

fit <- lm(`X87Sr.86Sr`~`X87Rb.86Sr`, data = Scourie)
Fit_coefficients=as_tibble(fit$coefficients)

slope=Fit_coefficients[c(2),c("value")]
intercept=Fit_coefficients[c(1),c("value")]



Age=log(slope+1)/1.42e-11
Age/1e6




