# Script to save figures easily
# PBH March 2023

f.fig.save <- function(filename,w=1586,h=950,svg=F){
  cat("Saving figure:",filename)
  ggsave(filename, ggplot2::last_plot(),units="mm",dpi=300,
         width = w/3.7795275591, # pixel to mm under dpi=300
         height = h/3.7795275591)
  
  
  # save as svg as well
  if (svg){
    ggsave(str_replace(filename,"png","svg"), ggplot2::last_plot(),units="mm",dpi=300,
           width = w/3.7795275591, # pixel to mm under dpi=300
           height = h/3.7795275591)
  }

  }



