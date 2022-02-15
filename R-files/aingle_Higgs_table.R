library(formattable)
library(tidyverse)
library(htmltools)
library(webshot)

df =data.frame(
  'Process'= c('1. ggF','2. VBF','3. Vh','4. tth & th'),
  'Cross_section'=digits(c(4.851E+01,3.779E+00,1.369E+00+8.819E-01,5.060E-01+2.875E-03+7.426E-02),2),
  'Theoretical_accuracy'= c('N3LO QCD & NLO EW','NNLO QCD* & NLO EW',' NNLO QCD & NLO EW','NLO QCD & NLO EW'),
  #'Theoretical uncertainty [%] '=c((4.6+6.7)/2),
  'Contribution' = c(88,7,4,1),
  'Experimental_uncertainty' =c(6.5,10,15,20)
)

df<- df %>% group_by(Process) %>% summarise(
    'Cross-section 13 TeV (pb)' = Cross_section,
    'Theo. accuracy'=Theoretical_accuracy,
    'Exp. uncertainty (%)'=Experimental_uncertainty,
    'Contribution (%)'=Contribution
  )
f<-formattable(df, align = c ("l", "c", "l","r","r"),list(
  Process = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
  'Theo. accuracy' = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
  'Cross-section 13 TeV (pb)' = formatter("span", style = x ~ style(round(x,digits = 1),color = "grey", font.weight = "bold")),
  'Exp. uncertainty (%)'=color_bar("lightpink"),
  'Contribution (%)'= color_bar("lightblue")))


f
#' Export a Formattable as PNG, PDF, or JPEG
#'
#' @param f A formattable.
#' @param file Export path with extension .png, .pdf, or .jpeg.
#' @param width Width specification of the html widget being exported.
#' @param height Height specification of the html widget being exported.
#' @param background Background color specification.
#' @param delay Time to wait before taking webshot, in seconds.
#'
#' @importFrom formattable as.htmlwidget
#' @importFrom htmltools html_print
#' @importFrom webshot webshot
#'
#' @export
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

export_formattable(f,'./table.pdf')
