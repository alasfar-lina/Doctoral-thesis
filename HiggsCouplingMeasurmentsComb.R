
library(tidyr)
library(purrr)
library(MAd) 
library(ggplot2)
library(latex2exp)
library(ggalt)   
library(tidyverse)
data = data.frame(
  'Exp'=c('ATLAS','CMS','ATLAS','CMS','ATLAS','CMS','ATLAS','CMS','ATLAS','CMS','ATLAS','CMS','ATLAS','CMS','ATLAS','CMS'),
  'id'= c('ka','ka','kg','kg','kz','kz','kw','kw','kt','kt','kb','kb','kta','kta','kmu','kmu'),
  'k'=c( 1.04,0.895, 0.92,1.165,0.99,0.96,1.06,1.225,0.92,1.01,0.87,0.95,0.92,0.94,1.07,0.21 ),
  'std'= c(0.06,0.115, 0.065,0.115, 0.06,0.07,0.06,0.115,0.10,0.11,0.11,0.23, 0.07,0.12,0.275,0.71) )
data2 <- data %>% group_by(Exp) %>% nest()



aggrivated<- agg(id=id, es=k, var=std, method = "BHHR", cor = 0.5,  mod=NULL, data=data)

pdf(file = "./higgs_couplings.pdf", width = 7.5, height = 5.5)
ggplot(aggrivated) + 
  geom_boxplot(aes(x=reorder(id,var),lower=es-var,upper=es+var,middle=es,ymin=es-2*var,ymax=es+2*var,fill = (var)*100),stat="identity")+
  labs(x = 'Coupling', y = 'Value') + 
  scale_x_discrete(labels= unname(TeX(rev(c("$\\kappa_{\\mu}$","$\\kappa_{b}$","$\\kappa_{t}$","$\\kappa_{\\tau}$","$\\kappa_{g}$","$\\kappa_{\\gamma}$","$|\\kappa_{W}|$","$\\kappa_{Z}$")))) )+
  coord_flip() + 
  theme_minimal() + 
  scale_fill_continuous(low = '#dfe9ec', high = '#3f7f93', name = "Uncertainty [%]", trans = "log10") +
 theme(axis.text = element_text(size = 16))  +
 theme(axis.title = element_text(size = 18)) +
theme(legend.title = element_text(size = 16)) +
  annotate("text", x=4.5, y=0.11, label= "ATLAS+CMS",size=6)+
annotate("text", x=4., y=0.120, label='Aggregated LHC Run-II',size=4)+
annotate("text", x=3.65, y=0.11, label=TeX('$L=139+137\\,\\mathrm{fb}^{-1}$'),size=4)
#annotate("text", x=3.15, y=0.01, label=TeX('$\\rho=0$'),size=4)
dev.off()

percent_first <- function(x) {
  x <- sprintf("%.1f%%", x)
 # x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}

hlvrun2= data.frame(
  id=aggrivated$id,
  'run2'=aggrivated$var*100,
  'hllhc'=c(0.018,0.025,0.015,0.017,0.034,0.037,0.019,0.043)*100
)
pdf(file = "./run2-hl-dumble.pdf", width = 7.6, height = 5.5)
ggplot() +
  theme_minimal() +
  geom_segment(data=hlvrun2, aes(y=reorder(id,aggrivated$var), yend=reorder(id,aggrivated$var), x=0, xend=40), color="#b2b2b2", size=0.15)+
  geom_dumbbell(data=hlvrun2, aes(y=id, x=hllhc, xend=run2),
              size=1.5, color="#b2b2b2", size_x=3.5, size_xend = 3.5, colour_x = '#7f5355', colour_xend = '#3f7f93')+
 scale_y_discrete(labels= unname(TeX(rev(c("$\\kappa_{\\mu}$","$\\kappa_{b}$","$\\kappa_{t}$","$\\kappa_{\\tau}$","$\\kappa_{g}$","$\\kappa_{\\gamma}$","$\\kappa_{W}$","$\\kappa_{Z}$"))))) +
  geom_text(data=filter(hlvrun2, id=="kmu"),
            aes(x=run2, y=id, label="Run-II"),
            color='#3f7f93', size=4.5, vjust=-1.5,fontface="bold")+#, fontface="bold", family="Lato") +
  geom_text(data=filter(hlvrun2, id=="kmu"),
            aes(x=hllhc, y=id, label="HL-LHC"),
            color='#7f5355', size=4.5, vjust=-1.5,fontface="bold")+#, fontface="bold", family="Lato")
  geom_text(data=hlvrun2, aes(x=run2, y=id, label=percent_first(run2)),
            color='#3f7f93', size=4., vjust=2.) +
  geom_text(data=hlvrun2, color='#7f5355', size=4., vjust=2.,
            aes(x=hllhc, y=id, label=percent_first(hllhc)))+
theme(axis.text = element_text(size = 16))  +
  theme(axis.title = element_text(size = 18)) +
  ylab(TeX("Coupling"))+
  xlab(TeX("Total uncertainty $\\lbrack %\\rbrack$"))+
  theme(
    panel.grid.major=element_blank(),
   # panel.grid.minor=element_blank(),
    panel.border=element_blank(),
   # axis.ticks=element_blank(),
    #axis.text.x=element_blank(),
  )
dev.off()