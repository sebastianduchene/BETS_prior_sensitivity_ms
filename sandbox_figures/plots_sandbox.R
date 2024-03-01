library(RColorBrewer)
pal <- brewer.pal(3, "Set1")

setwd("~/Documents/Work/Pasteur/BETS/figures")

mle_calculations <- read.table("BETS_empirical.csv", head = T, sep = ",")
head(mle_calculations)

rescale_mles <- function(mle_values){
  mle_values <- (mle_values - max(mle_values)) / 50 # Scaling
  mle_values <- mle_values + 10
  mle_values[mle_values < 1 ] <- rnorm(sum(mle_values < 1), 0.5, 0.2) 
  # Here one unit corresponds to 50 log marginal likelihood units, due to the scaling above. 
  # log Bayes factors < 50 are assigned a value of 0.5 with some noise to avoid overlap in the figure.
  return(mle_values)
}

plot_polygon <- function(mle_values, pal){
  polygon(c(-mle_values[1], mle_values[2], mle_values[4], -mle_values[3]), 
            c(mle_values[1], mle_values[2], -mle_values[4], -mle_values[3]), border = pal)
  points(c(-mle_values[1], mle_values[2], mle_values[4], -mle_values[3]), 
         c(mle_values[1], mle_values[2], -mle_values[4], -mle_values[3]), col = pal, cex=1)
}
 
make_plots <- function(dataset){
  exp_mles <- mle_calculations$Marginal.Likelihood[grep(paste0(dataset,".+exp"), mle_calculations$Name)]
  exp_mles <- rescale_mles(exp_mles)
  
  gamma_mles <- mle_calculations$Marginal.Likelihood[grep(paste0(dataset,".+gamma"), mle_calculations$Name)]
  gamma_mles <- rescale_mles(gamma_mles)
  
  logn_mles <- mle_calculations$Marginal.Likelihood[grep(paste0(dataset,".+logn"), mle_calculations$Name)]
  logn_mles <- rescale_mles(logn_mles)
  
  par(mar = c(3, 10, 5, 8))
  plot(0, 0, ylim = c(-12, 12), xlim = c(-12, 13), type = "n", bty = "n", 
       yaxt = "n", xaxt = "n", ylab = "", xlab = "", 
       main = names_dict[dataset,], cex.main = 1.5)
  #     main = expression(paste("Prior on ", Phi: Exp(1.0))))
  mtext("Strict clock (SC)", side = 3, las = 1, cex = 0.8)
  mtext("Relaxed clock (UCLD)", side = 1, las = 1, cex = 0.8)
  mtext("Heterochronous", side = 2, las = 2, cex = 0.8)
  mtext("Isochronous", side = 4, las = 2, cex = 0.8)
  
  for(i in seq(from = 10, to = 2, by = -2)){
    polygon(c(-i, -i, i, i), c(-i, i, i, -i), border = "darkgrey", lty = 3, col = "white")
  }
  points(0, 0, pch = 3, col = "darkgrey")
  text(c(-10.5, 10.5, 10.5, -10.5), 
       c(11.5, 11.5, -11.5, -11.5), labels = c("Het/SC", "Iso/SC", "Iso/UCLD", "Het/UCLD"), cex = 1.2)
  
  plot_polygon(exp_mles, pal[1])
  plot_polygon(gamma_mles, pal[2])
  plot_polygon(logn_mles, pal[3])
  legend(12,-1.5, legend=c("Exponential","Gamma","Lognormal"), col=c(pal[1],pal[2],pal[3]),pch=c(1,1,1),lty=1,xpd=TRUE,cex=1,pt.cex=1.2)
  dev.off
}
# het/sc, iso/sc, het/ucld, iso/ucld

datasets <- c("cholera", "powv", "treponema")

names_dict <- data.frame(c("Vibrio cholerae","Orthoflavivirus powassanens","Treponema pallidum"),row.names = datasets)

pdf("polygon_plot.pdf",width=6,height=8)
par(mfrow = (c(3,1))) 
for (dataset in datasets) {
  #png(paste0(dataset,"_polygon_plot.png"),width = 800, height = 450)
  make_plots(dataset)
  #dev.off()
}
dev.off()


