theme_custom <- function(base_size=12, base_family="", legend_title=16, plot_title=14, plot_caption=12){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
  ) %+replace%
    theme(axis.ticks = element_line(color = "grey92"),
          axis.title=element_text(size=plot_title, face="bold"),
          axis.ticks.length = unit(.5, "lines"),
          panel.grid.minor = element_blank(),
          legend.title = element_text(size=legend_title),
          legend.text = element_text(color = "grey30"),
          plot.title = element_text(size = plot_title, face = "bold", hjust=0),
          plot.subtitle = element_text(size = base_size, color = "grey30"),
          plot.caption = element_text(size = plot_caption, margin = margin(t = 15)),
          strip.text.x = element_text(margin = margin(0.5,0,0.5,0, "cm")),
          strip.text=element_text(size=12))
}

theme_custom2 <- function(base_size=6, base_family="", legend_title=12, plot_title=8, plot_caption=6){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
  ) %+replace%
    theme(axis.ticks = element_line(color = "black"),
          axis.text.y=element_text(hjust=1, margin = margin(r=10)), ### adjust labels after moving ticks inside margin must be applied separately
          axis.text.x=element_text(margin = margin(t=10)),
          axis.ticks.length = unit(-0.2, "lines"),
          axis.title.x=element_text(size=plot_title,face="bold"),
          axis.title.y=element_text(size=plot_title,face="bold", angle=90),
          panel.grid = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          panel.border=element_rect(colour="black", fill=NA),
          legend.title = element_text(size=legend_title),
          legend.text = element_text(size=base_size, color = "grey30"),
          plot.title = element_text(size = plot_title, face = "bold", hjust=0),
          plot.subtitle = element_text(size = base_size, color = "grey30"),
          plot.caption = element_text(size = plot_caption, margin = margin(t = 15)),
          strip.text.x = element_text(margin = margin(0.5,0,0.5,0, "cm")),
          strip.text=element_text(size=legend_title, face="bold"),
    )
}

theme_custom_no_lines <- function(base_size=6, base_family="", legend_title=12, plot_title=8, plot_caption=6){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
  ) %+replace%
    theme(
      axis.text.y=element_text(hjust=1, margin = margin(r=10)), ### adjust labels after moving ticks inside margin must be applied separately
      axis.text.x=element_text(margin = margin(t=10)),
      axis.title.x=element_text(size=plot_title,face="bold"),
      axis.title.y=element_text(size=plot_title,face="bold", angle=90),
      panel.grid = element_blank(),
      strip.background = element_rect(colour="black", fill="white"),
      panel.border=element_blank(),
      legend.title = element_text(size=legend_title),
      legend.text = element_text(size=base_size, color = "grey30"),
      plot.title = element_text(size = plot_title, face = "bold", hjust=0),
      plot.subtitle = element_text(size = base_size, color = "grey30"),
      plot.caption = element_text(size = plot_caption, margin = margin(t = 15)),
      strip.text.x = element_text(margin = margin(0.5,0,0.5,0, "cm")),
      strip.text=element_text(size=legend_title, face="bold"),
    )
}

theme_custom3 <- function(base_size=10, base_family="", legend_title=16, plot_title=14, plot_caption=12){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
  ) %+replace%
    theme(axis.ticks = element_line(color = "grey92"),
          axis.title=element_text(size=plot_title, face="bold"),
          axis.ticks.length = unit(.5, "lines"),
          panel.grid.minor = element_blank(),
          legend.title = element_text(size=legend_title),
          legend.text = element_text(color = "grey30"),
          plot.title = element_text(size = plot_title, face = "bold", hjust=0),
          plot.subtitle = element_text(size = base_size, color = "grey30"),
          plot.caption = element_text(size = plot_caption, margin = margin(t = 15)),
          strip.text.x = element_text(margin = margin(0.5,0,0.5,0, "cm")),
          strip.text=element_text(size=legend_title))
}