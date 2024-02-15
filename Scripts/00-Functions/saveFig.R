# Script to save figures easily
# PBH March 2023

f.fig.save <- function(filename,w=8.7*2,h=8.7,svg=F){
  cat("Saving figure:",filename)
  # ggsave(filename, ggplot2::last_plot(),units="mm",dpi=300,
  #        width = w/3.7795275591, # pixel to mm under dpi=300
  #        height = h/3.7795275591)
  
  ggsave(filename, ggplot2::last_plot(),units="cm",dpi=500,
         width = w,height = h)

  }
