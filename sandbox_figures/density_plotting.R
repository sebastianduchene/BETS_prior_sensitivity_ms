library(ggplot2)
library(grid)
library(gridExtra)
library(bayestestR)
library(RColorBrewer)

pal <- brewer.pal(3, "Set1")

setwd("~/Documents/Work/Pasteur/BETS/figures")

#meanRate_df <- data.frame(exp_data$meanRate,gamma_data$meanRate,lognorm_data$meanRate)

#cur_plot <- ggplot(exp_data, aes(x=meanRate)) + 
#  ggplot_add(gamma_data, aes(x=meanRate)) +
#  geom_density()

plot_stat_density <- function(dataset,clock_model,statistic,ci_value,hide_legend){
  exp_data <- tail(read.table(paste0("../../DownloadUpload/",dataset,"_empirical_10x/",dataset,"_het_",clock_model,"_exponential.log"),header = T),10000)
  
  gamma_data <- tail(read.table(paste0("../../DownloadUpload/",dataset,"_empirical_10x/",dataset,"_het_",clock_model,"_gamma.log"),header = T),10000)
  
  lognorm_data <- tail(read.table(paste0("../../DownloadUpload/",dataset,"_empirical_10x/",dataset,"_het_",clock_model,"_lognormal.log"),header = T),10000)

  exp_data <- cbind(exp_data,"Exponential")
  names(exp_data)[length(names(exp_data))]<-"Prior" 
  gamma_data <- cbind(gamma_data,"Gamma")
  names(gamma_data)[length(names(gamma_data))]<-"Prior" 
  lognorm_data <- cbind(lognorm_data,"Lognormal")
  names(lognorm_data)[length(names(lognorm_data))]<-"Prior" 
  
  data_df <- rbind(exp_data,gamma_data,lognorm_data)
  
  cred_int <- ci(rbind(exp_data[statistic],gamma_data[statistic],lognorm_data[statistic]),ci=ci_value) ### convert to log 10
  #max_lim <- max(exp_data[statistic],gamma_data[statistic],lognorm_data[statistic]) ### convert to log 10
  
  print(cred_int)
  #print(c(min_lim,max_lim))
  
  colours <- c("Exponential" = pal[1], "Gamma" = pal[2], "Lognormal" = pal[3])
  
  density_plot <- ggplot(data_df) + xlim(cred_int$CI_low,cred_int$CI_high) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
  #+ theme(axis.title.x=element_blank(),axis.ticks.x=element_blank())
  density_plot <- density_plot + geom_density(aes(x=!!sym(statistic),colour=Prior),show_guide=FALSE) +
    stat_density(aes(x=!!sym(statistic), colour=Prior),geom="line",position="identity") +
    scale_color_manual(values = colours) +
    theme(plot.title = element_text(size = 11, face = "bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size=7)) + 
    if(!hide_legend)theme(legend.position="none")
  return(density_plot)
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

datasets <- c("cholera", "powv", "treponema")

clock_models <- c("SC", "ucld")

#dataset <- "cholera"
#dataset <- "powv"
#dataset <- "treponema"

for (x in 1:3) {
  plot_list <- vector('list')
  i <- 1
  for (clock_model in clock_models) {
    statistics <- c("meanRate","treeModel.rootHeight","treeLength")
    if (clock_model == "ucld") {
      statistics <- c(statistics,"coefficientOfVariation")
    }
    for (statistic in statistics) {
      plot_list[[i]] <- plot_stat_density(datasets[x],clock_model,statistic,0.995,F)
      i <- i + 1
    }
  }
  legend_plot <- plot_stat_density(datasets[x],clock_model,statistic,0.995,T)
  legend <- get_legend(legend_plot)
  col.titles <- c("Strict Clock", "Uncorrelated Lognormal Relaxed Clock")
  dataset_titles <- c("Vibrio cholerae","Powassan Virus","Treponema pallidum")
  #row.titles <- c("Mean Clock Rate", "Root Height", "Tree Length")
  blank <- grid.rect(gp=gpar(col="white"))
  pdf(paste0(datasets[x],"_density_plot.pdf"),width=9)
  #grid.arrange(arrangeGrob(plot_list[[1]],plot_list[[2]],plot_list[[3]],blank,top=col.titles[1],ncol=1),
  #             arrangeGrob(plot_list[[4]],plot_list[[5]],plot_list[[6]],plot_list[[7]],top=col.titles[2],ncol=1),ncol=2,top=dataset_titles[x])
  grid.arrange(arrangeGrob(plot_list[[1]],plot_list[[2]],plot_list[[3]],legend,top=col.titles[1],ncol=1),
               arrangeGrob(plot_list[[4]],plot_list[[5]],plot_list[[6]],plot_list[[7]],top=col.titles[2],ncol=1),ncol=2,top=dataset_titles[x])
  dev.off()
}


