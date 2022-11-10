#radar chart function
# Gavin Fay January 2022, modified from code originally by Mackenzie Mazur
#
#
library(tidyverse)
library(scales)
library(fmsb)


do_radar_plot <- function(metrics) {
  # metrics is a dataframe of metrics, expecting columns
  # metrics <- tibble(metric = rep(letters[1:5],each=3),
  #                   value = runif(15),
  #                   mp = as.character(rep(1:3,5)))
  # # metric 
  # # value
  # # mp (management procedure)
  summaries <- metrics %>% 
    group_by(metric) %>% 
    summarize(min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE)) %>% 
    #pivot_longer(cols = c(2:3),names_to = "mp",
    #             values_to = "value") %>% 
    #pivot_wider(names_from = metric, values_from = value) %>% 
    #arrange(desc(mp)) %>% 
    I()
  #summaries  
  nmetrics <- length(unique(metrics$metric))
  nmp <- length(unique(metrics$mp))
    
  d <- metrics %>% 
    group_by(mp) %>% 
    left_join(summaries) %>% 
    mutate(value = value/(max + 0.001)) %>% 
    select(mp, metric, value) %>% 
    pivot_wider(names_from = metric, values_from = value) %>% 
    ungroup()
  
  bounds <- tibble(mp = c("max","min"),
                   a = c(1,0),
                   b = c(1,0),
                   c = c(1,0),
                   d = c(1,0),
                   e = c(1,0))
  
  dd <- bounds %>% 
    bind_rows(d)

#NEW PLOT
#colorblind colors
colors_fill2<-c(alpha("#000000",0.1),
                alpha("#E69F00",0.1),
                alpha("#56B4E9",0.1),
                alpha("#009E73",0.1),
                alpha("#F0E442",0.1),
                alpha("#E69F00",0.1),
                alpha("#56B4E9",0.1),
                alpha("#009E73",0.1))
colors_line2<-c(alpha("#000000",0.9),
                alpha("#E69F00",0.9),
                alpha("#56B4E9",0.9),
                alpha("#009E73",0.9),
                alpha("#F0E442",0.9),
                alpha("#E69F00",0.9),
                alpha("#56B4E9",0.9),
                alpha("#009E73",0.9))

#colorblind
radarchart(dd[,-1],seg=5,pcol=colors_line2,
           pfcol=colors_fill2,plwd=2,
           vlabels=unique(metrics$metric), vlcex=1,
           plty=c(rep(1,nmetrics),rep(2,nmetrics)))
rows<<-rownames(dd[-c(1,2),])
colors_line<<-colors_line2
legend("topleft",inset=-.01,title ="HCR",title.adj = 0.2,
       legend=unique(d$mp),
       pch=16,
       col=colors_line2[1:nmp],
       lty=1, cex=1, bty= 'n', y.intersp=1)

}

# metrics <- tibble(metric = rep(letters[1:5],each=3),
#                   value = runif(15),
#                   mp = as.character(rep(1:3,5)))
# do_radar_plot(metrics)
