library(tidyverse) # needed for 'glimpse'
library(dmetar)
library(meta)
library(latex2exp)

data <- data.frame("Study" = c( "CMS Run-I (h →γγ)",
                                "ATLAS Run-I (h →γγ)" 
                                ,"CMS Run-I (h →4ℓ)" ,
                                "ATLAS Run-I(h →4ℓ))",
                                "CMS Run-II (h →γγ)","ATLAS Run-II (h →γγ) "
                                ,"CMS Run-II(h →4ℓ)",
                                "ATLAS Run-II(h →4ℓ)"),
                   "TE" = c(124.70, 126.02,125.59,124.51, 125.78,124.93,125.26,124.79),
                   "seTE" = c(0.34,0.51,0.45,0.52, 0.26,0.40, 0.21,0.37 )
                   ) 

m.gen_bin <- metagen(TE = TE,
                     seTE = seTE,
                     studlab = Study,
                     data = data,
                     sm = "SMD",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     hakn = TRUE,
                     title = "Higgs Mass measurments at the LHC")

#pdf(file = "./forestplot_higgsmass.pdf", width = 8, height = 5.5)
forest.meta(m.gen_bin, 
            #sortvar = TE,
            predict = TRUE, 
            print.tau2 = FALSE,
            xlim = c(124,127),
            digits.se=2,
           # print.I2=FALSE,
            col.square= "#9cadce",
            col.diamond= "#9cadce",
            leftcols = c("studlab"),
            leftlabs = c(""),
            rightlabs=c("Mass (GeV)")
            )

#dev.off()
