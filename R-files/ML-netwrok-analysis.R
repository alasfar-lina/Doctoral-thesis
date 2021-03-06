library(ggplot2)
library(plyr);
library(dplyr)
library(tidyr)
library(splitstackshape)
options(kableExtra.latex.load_packages = TRUE)
library(kableExtra)
library(lmtest)
library(jtools) # for summ()
library(lme4)
library(caret)
library(lmerTest)
library(coin)
library(mdthemes)
library(ggpubr)
library(ggrepel)
library(latex2exp)
library(igraph)

setwd("/Users/lina/Dropbox/Doctoral-thesis")

netz<- read.csv(file="./R-files/network.csv")
 head(netz)
 op <- options(warn = (-1)) # suppress warnings
 spr0=c()
 spr1=c()
 pvalstat1=c()
 pvalstat11=c()
 pval0=c()
 pval1=c()
 ts=c()
 netzX <- netz
 netzX$chn <- NULL
 str1 <-  netzX%>% filter(.,ch==0)
 srt2 <-  netzX%>% filter(.,ch==3)
 ch1vsch2<- subset(netzX, ch==0|ch==3)
 netzY <- netzX
 netzY$ch <- NULL
 featnames <-  netzY%>% colnames()
 featpairs <- c()
 for (i in seq(from=1,to=length(featnames)-1)) {
   for (j in seq(from=i+1,to=length(featnames)) ) {
     m1 <- lm((eval(parse(text = featnames[i]))) ~ (eval(parse(text = featnames[j])))+ (eval(parse(text = featnames[j]))): ch , data = ch1vsch2)
     m0 <- lm((eval(parse(text = featnames[i]))) ~ (eval(parse(text = featnames[j]))) , data = ch1vsch2)
     m11 <- lm((eval(parse(text = featnames[j]))) ~ (eval(parse(text = featnames[i])))+ (eval(parse(text = featnames[i]))): ch , data = ch1vsch2)
     m00 <- lm((eval(parse(text = featnames[j]))) ~ (eval(parse(text = featnames[i]))) , data = ch1vsch2)
     l<-lmtest::lrtest(m1, m0)
     l1<-lmtest::lrtest(m11, m00)
     print((summ(m1)),confint = TRUE, pvals = TRUE)
     tst<-cor.test( str1[,featnames[i]] , str1[,featnames[j]], method= "pearson")
     tst1<-cor.test( srt2[,featnames[i]] , srt2[,featnames[j]], method= "pearson")
     print(tst)
     spr0 <- c(spr0, tst$estimate)
     spr1 <- c(spr1, tst1$estimate)
     pval0 <- c(pval0, tst$p.value)
     pval1 <- c(pval1, tst1$p.value)
     pvalstat1<-c(pvalstat1,l$P[2])
     pvalstat11<-c(pvalstat11,l1$P[2])
     featpairs<- c(featpairs,paste(featnames[i],featnames[j],sep='&'))
   }
 } 
 q1<-p.adjust(pval0, method ='fdr', n = length(pval0)) ## should be done after the test
 q2<-p.adjust(pval1, method ='fdr', n = length(pval1)) 
 
 df = data_frame(corr1=spr0,corr2=spr1,qval1=q1,qval2=q2,anovp=pvalstat1, names=featpairs)
 dff <- subset(df,replace_na((qval1<0.001 &qval2 <0.01 & anovp< 0.01), FALSE))
  
 #pdf(file = "./WGCNAPlots/4-module_tree_blockwise.pdf", width = 8, height = 6);
 
 ggplot(df, aes(x=corr1, y=corr2, colour = replace_na((qval1<0.01 &qval2 <0.01 & anovp< 0.01), FALSE),labels=names ))  +  theme_bw()  + 
   geom_point(size= 3,aes(alpha= replace_na((qval1<0.01 &qval2 <0.01 & anovp< 0.01), FALSE)))+   # draw points
   scale_colour_manual(name = 'FDR corr. pval < 0.01', values = setNames(c('grey','#b5838d'),c(FALSE, TRUE)),guide=FALSE) +
   scale_alpha_discrete(range=c(0.6,0.9,0.9),guide=FALSE)+
   geom_vline(xintercept = 0,colour='grey')+
   geom_hline(yintercept = 0,colour='grey')+
  geom_text_repel(data=dff, aes(label=names),
                   #nudge_x = .15,
                   box.padding = 0.5,
                   #nudge_y = 1,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 20,
                   size          = 4.15,
   )+
   labs(subtitle="", 
        y=TeX("Pearson $\\rho$ bkg."), 
        x=TeX("Pearson $\\rho$ $gg\\to hh$ (SM)"), 
       # title="*_Enterobacteriaceae_* (family) ", 
        #caption="Bootsrapping & permutation tests & FDR corrected p < 0.05 "
   )+
   annotate("text", x = 0.05, y=0.75, label = "FDR corr p<0.01")+
 
   theme(
     #plot.title = element_text(hjust=0.5, face="bold"),
     #   plot.background=element_rect(fill="#f7f7f7"),
     #  panel.background=element_rect(fill="#f7f7f7"),
     panel.grid.minor=element_blank(),
     # panel.grid.major.y=element_blank(),
     #panel.grid.major.x=element_line(),s
     #  axis.ticks=element_blank(),
     # legend.position="top",
     # panel.border=element_blank()
   )

 str1$ch <- NULL
 gr <- cor(str1,str1) %>% as.matrix()
 diag(gr)<-0
 graph<-graph.adjacency(gr,weighted=TRUE,mode="lower")
 g=delete.edges(graph, which(E(graph)$weight <=.5)) # here's my condition.
 
 plot(g)
 deg <- degree(g, mode="all")
 deg.dist <- degree_distribution(g, cumulative=F, mode="all") 
 plot( x=log10(0:max(deg)), y=log10(deg.dist), pch=19, cex=1.2, col="orange",xlab="Degree", ylab="Cumulative Frequency")
 ceb <- cluster_edge_betweenness(g) 
 dendPlot(ceb, mode="hclust")
 plot(ceb, g)
 membership(ceb) # community membership for each node
 
 hs <- hub_score(g, weights=NA)$vector
 as <- authority_score(g, weights=NA)$vector
 par(mfrow=c(1,2))
 plot(g, vertex.size=hs*50, main="Hubs")
 plot(g, vertex.size=as*30, main="Authorities")
 
 deg
 deg.dist
 
 