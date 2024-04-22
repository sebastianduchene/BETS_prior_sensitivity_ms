library(phangorn)
library(RColorBrewer)
pal <- brewer.pal(3, "Set1")

plot(1:length(pal), col = pal, pch = 20)



# Exponential = pal[1], gamma =pal[2], lognormal = pal[3]

setwd("/home/sebastiand/Dropbox/projects_WORKING/BETS_prior_sensitivity/sandbox")

exponential <- read.table("dummy_ultrametric_exponential.log", header=TRUE)
gamma <- read.table("dummy_ultrametric_gamma.log", header=TRUE)
lognormal <- read.table("dummy_ultrametric_lognormal.log", header=TRUE)
head(exponential)

# plot pop size, clock rate, tree height, and tree length

pdf("prior_densities.pdf", width = 8, height = 4)
par(mfcol = c(3, 3), mar = c(1, 2.2, 1, 1))
hist(exponential$constant.popSize, xlim = c(0, 5), breaks = 50, col = pal[1], 
     border = "white", main = "", yaxt = "n", ylab = "")
legend(3, 1000, legend = c("Exponential", "Gamma", "Lognormal"), fill = pal, 
       border = NA, bty = "n")
hist(gamma$constant.popSize, breaks = 20000, col = pal[2], border = "white", xlim = c(0, 5), 
     main = "", yaxt = "n")
mtext("Prior probability", side = 2)
par(mar = c(4, 2, 1, 1))
hist(lognormal$constant.popSize, breaks = 500, col = pal[3], border = "white", 
     main = "", yaxt = "n", xlim = c(0, 5), xlab = "")
mtext(expression(paste("Population size ", Phi)), side = 1, padj = 2.5)

# clock rate
par(mar = c(1, 2.2, 1, 1))
hist(log10(exponential$clock.rate), xlim = c(-7, 0), breaks = 50, col = pal[1], 
     border = "white", main = "", yaxt = "n", ylab = "", xaxt = "n")
hist(log10(gamma$clock.rate), breaks = 50, col = pal[2], border = "white", xlim = c(-7, 0),
     main = "", yaxt = "n", xaxt = "n")
#mtext("Prior probability", side = 2)
par(mar = c(4, 2, 1, 1))
hist(log10(lognormal$clock.rate), breaks = 50, col = pal[3], border = "white", 
     main = "", yaxt = "n", xlim = c(-7, 0), xlab = "", xaxt = "n")
axis(1, at = c(-7:0), labels = 10^(-7:0))
mtext("Clock rate (subs/site/year)", side = 1, padj = 2.5)

# tree height
par(mar = c(1, 2.2, 1, 1))
hist(log10(exponential$treeModel.rootHeight), breaks = 10, col = pal[1], 
     border = "white", main = "", yaxt = "n", ylab = "", xlim = c(1, 2), 
     xaxt = "n")
hist(log10(gamma$treeModel.rootHeight), breaks = 50, col = pal[2], border = "white",
     main = "", yaxt = "n", xlim = c(1, 2), xaxt = "n")
#mtext("Prior probability", side = 2, padj = -1)
par(mar = c(4, 2, 1, 1))
hist(log10(lognormal$treeModel.rootHeight), breaks = 20, col = pal[3], border = "white", 
     main = "", yaxt = "n", xlim = c(1, 2), xlab = "", xaxt = "n")
mtext("Tree height (years)", side = 1, padj = 2.5)
axis(1, at = log10(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)), 
     labels = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
dev.off()

