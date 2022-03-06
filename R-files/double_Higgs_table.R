library(formattable)
library(tidyverse)
library(htmltools)
library(webshot)
library(ragg)
summation=40.7077/100
df =data.frame(
  'Process'= c('1. ggF hh','2. VBF hh','3. Zhh',"4. W⁺hh",'5. W⁻hh','6. tthh & tjhh'),
  'Cross_section'=digits(c(36.69,2.05,0.415,0.369,0.198,0.949+0.0367),3),
  'Theoretical_accuracy'= c('NNLO QCD' ,'N³LO QCD',' NNLO QCD','NNLO QCD','NNLO QCD','NLO QCD'),
  'Theoretical_uncertainty'=c(12.3431,2.1,3.58469,2.13776,2.97532,5.12933) %>% digits(.,1),
  'Contribution' = c(36.69/summation,2.05/summation,0.415/summation,
                     0.369/summation,0.198/summation ,0.9857/summation) %>% digits(.,1)
)

df<- df %>% group_by(Process) %>% summarise(
    'Cross-section 14 TeV (fb)' = Cross_section,
    'Theo. accuracy'=Theoretical_accuracy,
    'Theo. uncertainty (%)'=Theoretical_uncertainty,
    'Contribution (%)'=Contribution
  )
f<-formattable(df, align = c ("l", "c", "l","r","r"),list(
  Process = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
  'Theo. accuracy' = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
  'Cross-section 13 TeV (pb)' = formatter("span", style = x ~ style(round(x,digits = 1),color = "grey", font.weight = "bold")),
  'Theo. uncertainty (%)'=color_bar("lightpink"),
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
