library(RColorBrewer)
pal <- brewer.pal(10, "Set3")

setwd("~/Documents/Work/Doherty/BETS/bounds_small_sims")
#setwd("~/Downloads/small_sim_plots")

#mle_calculations <- read.table("~/Documents/Work/Pasteur/BETS/figures/BETS_empirical.csv", head = T, sep = ",")
mle_exp <- read.csv("exponential_marginal_likelihoods.csv", row.names = 1, sep = ",")
mle_exp <- mle_exp[, c("SC_het", "SC_iso", "UCLD_iso", "UCLD_het")]
mle_log <- read.csv("lognormal_marginal_likelihoods.csv", row.names = 1, sep = ",")
mle_log <- mle_log[, c("SC_het", "SC_iso", "UCLD_iso", "UCLD_het")]
mle_gamma <- read.csv("gamma_marginal_likelihoods.csv", row.names = 1, sep = ",")
mle_gamma <- mle_gamma[, c("SC_het", "SC_iso", "UCLD_iso", "UCLD_het")]

rescale_mles <- function(mle_values){
  for(i in 1:dim(mle_values)[1]){
    mle_values[i,] <- mle_values[i,] - max(mle_values[i,])
  }
  mle_values <- 10 + mle_values / 10
  mle_values[mle_values < 0] <- 0
  return(mle_values)
}

#plot_polygon <- function(mle_values, pal){
#  polygon(c(-mle_values[1], mle_values[2], mle_values[4], -mle_values[3]), 
#            c(mle_values[1], mle_values[2], -mle_values[4], -mle_values[3]), border = pal)
#  points(c(-mle_values[1], mle_values[2], mle_values[4], -mle_values[3]), 
#         c(mle_values[1], mle_values[2], -mle_values[4], -mle_values[3]), col = pal, cex=1)
#}
 
plot_polygon <- function(mle_values, pal, index){
  polygon(c(-mle_values$SC_het[index],mle_values$SC_iso[index],mle_values$UCLD_iso[index],-mle_values$UCLD_het[index]),
          c(mle_values$SC_het[index],mle_values$SC_iso[index],-mle_values$UCLD_iso[index],-mle_values$UCLD_het[index]), border = pal)
  #points(, col = pal, cex=1)
}

draw_plot <- function(mle_values, set, prior, colours = c("purple", "orange"), main = "main title"){
  par(mar = c(3, 2, 4, 2), pty="s")
  plot(0, 0, ylim = c(-13, 13), xlim = c(-13, 13), type = "n", bty = "n", 
       yaxt = "n", xaxt = "n", ylab = "", xlab = "", main = main)
  mtext("Strict clock (SC)", side = 3, las = 1, cex = 0.8)
  mtext("Relaxed clock (UCLD)", side = 1, las = 1, cex = 0.8)
  #mtext("Heterochronous", side = 2, las = 1, cex = 0.8)
  #mtext("Isochronous", side = 4, las = 1, cex = 0.8)
  
  for(i in seq(from = 10, to = 1, by = -1)){
    polygon(c(-i, -i, i, i), c(-i, i, i, -i), border = "darkgrey", lty = 3, col = "white")
  }
  points(0, 0, pch = 3, col = "darkgrey")
  text(c(-10.5, 10.5, 10.5, -10.5), 
       c(11.5, 11.5, -11.5, -11.5), labels = colnames(mle_values))
  
  for (index in 1:10) {
    plot_polygon(mle_values, pal = colours[1], index)
  }
  for (index in 11:20){
    plot_polygon(mle_values, pal = colours[2], index)    
  }
}

for (set in c(1,3)) {
  
}



sets <- c("Heterochronous with Bounds", "Heterchronous without Bounds", "Ultrametric with Bounds", "Ultrametric without Bounds")

priors <- c("Exponential", "Gamma", "Lognormal")

for (prior in priors) {
  
}

#pdf("het_bounds_sims.pdf",width=6,height=8)
#par(mfrow = (c(3,1))) 

set <- 1

exp_mles <- mle_exp[grep("hetero", rownames(mle_exp)),]
exp_mles <- rescale_mles(exp_mles)

logn_mles <- mle_log[grep("hetero", rownames(mle_log)),]
logn_mles <- rescale_mles(logn_mles)

gamma_mles <- mle_gamma[grep("hetero", rownames(mle_gamma)),]
gamma_mles <- rescale_mles(gamma_mles)

pdf("het_sims.pdf",width=9,height=4.5)
par(mfrow = (c(1,3))) 
draw_plot(exp_mles,set,priors[1], main = "Heterochronous, exponential prior")
draw_plot(logn_mles,set,priors[3], main = "Heterochronous, lognormal prior")
legend(-16.5,-17, legend=c("Bounds on rootheight","No bounds on rootheight"), col=c("purple", "orange"),
       pch=c(1,1),lty=1,xpd=TRUE,cex=1,pt.cex=0, horiz = TRUE,bty = "n",text.width=c(13,8),seg.len=1.5)
draw_plot(gamma_mles,set,priors[2], main = "Heterochronous, gamma prior")
dev.off()

set <- 3

exp_mles <- mle_exp[grep("ultra", rownames(mle_exp)),]
exp_mles <- rescale_mles(exp_mles)

logn_mles <- mle_log[grep("ultra", rownames(mle_log)),]
logn_mles <- rescale_mles(logn_mles)

gamma_mles <- mle_gamma[grep("ultra", rownames(mle_gamma)),]
gamma_mles <- rescale_mles(gamma_mles)

pdf("iso_sims.pdf",width=9,height=4.5)
par(mfrow = c(1,3))
draw_plot(exp_mles,set,priors[1], main = "Isochronous, exponential prior")
draw_plot(logn_mles,set,priors[3], main = "Isochronous, lognormal prior")
legend(-16.5,-17, legend=c("Bounds on rootheight","No bounds on rootheight"), col=c("purple", "orange"),
       pch=c(1,1),lty=1,xpd=TRUE,cex=1,pt.cex=0, horiz = TRUE,bty = "n",text.width=c(13,8),seg.len=1.5)
draw_plot(gamma_mles,set,priors[2], main = "Isochronous, gamma prior")
dev.off()

#dev.off()

#gamma_mles <- mle_calculations$Marginal.Likelihood[grep(paste0(dataset,".+gamma"), mle_calculations$Name)]
#gamma_mles <- rescale_mles(gamma_mles)

pdf("iso_bounds_sims.pdf",width=6,height=8)
par(mfrow = (c(2,1))) 

set <- 3

exp_mles <- mle_exp[(set*7):((set*7)+9),]
exp_mles <- rescale_mles(exp_mles)

logn_mles <- mle_log[(set*7):((set*7)+9),]
logn_mles <- rescale_mles(logn_mles)

draw_plot(exp_mles,set,priors[1])
draw_plot(logn_mles,set,priors[3])
dev.off()


#plot_polygon(gamma_mles, pal[2])
#plot_polygon(logn_mles, pal[3], 1)
#legend(12,-1.5, legend=c("Exponential","Gamma","Lognormal"), col=c(pal[1],pal[2],pal[3]),pch=c(1,1,1),lty=1,xpd=TRUE,cex=1,pt.cex=1.2)





