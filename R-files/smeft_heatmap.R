library('ComplexHeatmap')
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
zstar= 1.69

df = data.frame(
  'index'=c(1:33),
  'upper_bound'=c(2.019,5.834,1.862,3.346,1.025,0.393,0.271,0.347,0.293,0.638,0.371,
                  0.107,0.029,0.094,
                  2.195,-0.458,0.229,
                  0.348,0.035,0.370,0.040,0.344,0.062,
                  0.687,0.062,0.343,
                  0.005,0.002,0.007,0.0025,0.0015,0.5150,5.3
                  ),
  'lower_bound'=c(-2.229,-6.812,-1.830,-4.213,-1.151,-0.483,-0.205,-0.911,-0.380,-1.308,-0.449,
                  0.006,-0.084,-0.044,
                  -3.028,0.375,-0.187,
                  -1.286,-0.007,-0.004,-0.027,-0.375,-0.432,
                  -0.562,-0.432,-0.281,
                  -0.002,-0.005,-0.016,-0.005,-0.0253,-0.4390,-8.0
                  ),
  'operators'= c('cQQ1','cQQ8','cQt1','cQt8','ctt1','c8qt','c1qt','c8ut','c1ut','c8dt','c1dt',
                 'ctG','ctW','ctZ',
                 'cpt','cpui','cpdi',
                 'ctp','cbp','ccp','ctap','c3pQ3','c3pq',
                 'cpei','c3pli','cpli'
                 ,'cpG','cpB','cpW','cpWB','cpD','cpBox','cp'),
  'muttH'=c(0.,0.,0.,+13290.1/10^6,+137479./10^6,152635/10^6,22531.7/10^6,34359.2/10^6,34359.2/10^6,-1123.41/10^6,+15070.6/10^6,
            +568696/10^6,0,0,
            0,0,0,
            0,0,0,0,0,0,
            0,0,0,
            +423765./10^6,0,0,0,0,0,0),
  'muWH'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
           0,0,0,
           0,0,0,
           0,0,0,0,0,0,
           0,0,0,
           0,0,1095782/10^6, -285105./10^6,-160282/10^6,121150/10^6,0),
  'muZH'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
           0,0,0,
           0,0,0,
           +484275./10^6,-197878./10^6,+484275./10^6,0,2175601/10^6,90.68/10^6,
           0,0,0,
           0,+91707.3/10^6,+741805./10^6, +215319/10^6,-14992.4/10^6,121269/10^6,0),
  'muVBF'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
            0,0,0,
            0,0,0,
            0,0, -629.4/10^6,0, -16751.6/10^6,-15060.5/10^6,
            0,0,0,
            +57783.8/10^6, +5770.95 /10^6,  -51626.2/10^6,+21988.3/10^6,-170868./10^6,+121321./10^6,0),
  'muggHH'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
             0,0,0,
             0.317493,0,0,
             0,0,0,0,0,0,
             0,0,0,
             -0.152591,0,0,0,0.208482/4,-0.208482,0.491977),
  'muggH'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
            0,0,0,
            0,0,0,
            0,0,0,0,0,0,
            0,0,0,
            0,0,0,0,0,0,0),
  'sigmattZ'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
               0,0,0,
               0,0,0,
               0,0,0,0,0,0,
               0,0,0,
               0,0,0,0,0,0,0),
  'sigmattW:'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
                0,0,0,
                0,0,0,
                0,0,0,0,0,0,
                0,0,0,
                0,0,0,0,0,0,0),
  'sigmattA'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
               0,0,0,
               0,0,0,
               0,0,0,0,0,0,
               0,0,0,
               0,0,0,0,0,0,0),
  'sigmatqZ'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
               0,0,0,
               0,0,0,
               0,0,0,0,0,0,
               0,0,0,
               0,0,0,0,0,0,0),
  'sigmatW'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,0,0,0,
              0,0,0,
              0,0,0,0,0,0,0),
  'sigmatq'=c(0.,0.,0.,0,0,0,0,0,0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,0,0,0,
              0,0,0,
              0,0,0,0,0,0,0),
  'classification'=rep(c('4F','top','Yukawa','2 quark Higgs','2 lepton Higgs','Higgs'),times=c(11,3,3,6,3,7))
)

matr <- df[, !colnames(df) %in% c("classification", "lower_bound",'upper_bound','operators','index')]
matr <- matr %>%  as.matrix() %>%t()
colnames(matr) =df$operators
df$mean <- (df$upper_bound+df$lower_bound)/2-(df$upper_bound-df$lower_bound)/2
df$std <- (df$upper_bound-df$lower_bound)/zstar/2
nrm  <-array(0, dim=c(500,33))
for( i in 1:33){
  x<-rnorm(500, mean=df$mean[i], sd=df$std[i])
  nrm[,i]<- x
}

ha<- HeatmapAnnotation(class = df$classification, 
                           bounds = anno_boxplot(nrm),
                           annotation_name_rot = 0)

ht<- Heatmap(matr,name = "effect",show_column_dend = FALSE,top_annotation = ha,
             #row_labels = expression(alpha, beta, gamma, delta, epsilon,  zeta, 
                 #                    eta, theta, iota, kappa, lambda, mu, nu, xi, omicron, pi, rho, sigma)
             )
ht <- draw(ht)

help("expression")
#ord<-column_order(ht)
#ord

#keyDF <- data.frame(key=ord,weight=1:length(ord))
#df
#merged <- merge(df,keyDF,by.x='index',by.y='key',all.x=T,all.y=F)
#res <- merged[order(merged$weight),c('operators','upper_bound','lower_bound')]

