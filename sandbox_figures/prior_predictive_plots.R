setwd("/home/sebastiand/Dropbox/projects_WORKING/BETS_prior_sensitivity/BETS_prior_sensitivity_ms/sandbox_figures")

# For pairwise plots
uniform_prior <- tail(read.table("prior_predictive_simulations/cholera_aln_prior_uniformPhi.log", 
                                 head = T, sep = "\t"), 1000)

pdf("prior_predictive_plots.pdf", width = 7, height = 7, useDingbats = F)
# Here we plot pairwise of Phi, tree length, tree height, and clock rate for the tree priors
par(mfcol = c(4, 4), mar = c(4, 5, 1, 0.5))
hist(uniform_prior$constant.popSize, main = "", border = "white", col = "darkgrey", 
     xlab = "", ylab = "Prior prob.", yaxt = "n", xaxt = "n")
axis(1, at = c(0, 5000, 10000))
plot(uniform_prior$constant.popSize, uniform_prior$treeLength, pch = 20, col = rgb(0, 0, 0, 0.2), 
     xlab = "", ylab = "Tree length", main = "", bty = "n", xaxt = "n", yaxt = "n")
axis(1, at = c(0, 5000, 10000))
axis(2, at = c(0, 100000, 200000), labels = c(0, expression(10^4), ""), las = 2)
plot(uniform_prior$constant.popSize, uniform_prior$treeModel.rootHeight, 
     pch = 20, col = rgb(0, 0, 0, 0.2), xlab = "", 
     ylab = expression(paste("Root height (x", 10^{2}, ")")),  bty = "n", 
     xaxt = "n",  main = "", yaxt = "n")
axis(1, at = c(0, 5000, 10000))
axis(2, at = c(0, 25000, 50000), labels = c(0, 25, 50), las = 1)
plot(uniform_prior$constant.popSize, log10(uniform_prior$clock.rate), pch = 20, col = rgb(0, 0, 0, 0.2), 
     xlab = expression(paste("Effective pop. size (", Phi, ")")), 
     ylab = expression(paste("Clock rate (", 10^{-7}, ")" , collapse = "")),
     main = "", bty = "n", xaxt = "n", yaxt = "n")
axis(1, at = c(0, 5000, 10000))
axis(2, at = log10(c(1e-9, 1e-6, 10^-3)), labels = c(0.01, 10, 1000), las = 2)
#plot(log10(uniform_prior$treeModel.rootHeight), log10(uniform_prior$treeLength), pch = 20, col = rgb(0, 0, 0, 0.2), 
#     xlab = "", ylab = "", bty = "n")
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
hist(uniform_prior$treeLength, main = "", border = "white", 
     col = "darkgrey", xlab = "", ylab = "Prior prob.", yaxt = "n", xaxt = "n")
axis(1, at = c(0, 100000, 200000), labels = c(0, 100, 200))
plot(uniform_prior$treeLength, uniform_prior$treeModel.rootHeight, pch = 20, col = rgb(0, 0, 0, 0.2), 
     xlab = "", ylab = "", bty = "n", yaxt = "n", xaxt = "n")
axis(2, at = c(0, 25000, 50000), labels = c(0, 25, 50), las = 1)
axis(1, at = c(0, 100000, 200000), labels = c(0, 100, 200))
plot(uniform_prior$treeLength, log10(uniform_prior$clock.rate), pch = 20, col = rgb(0, 0, 0, 0.2), 
     main = "", xlab = "Tree length", ylab = "", bty = "n", xaxt = "n", yaxt = "n")
axis(1, at = c(0, 100000, 200000), labels = c(0, expression(10^4), ""))
axis(2, at = log10(c(1e-9, 1e-6, 10^-3)), labels = c(0.01, 10, 1000), las = 2)
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
#plot(log10(uniform_prior$treeModel.rootHeight), log10(uniform_prior$treeLength), pch = 20, col = rgb(0, 0, 0, 0.2), 
#     xlab = "", ylab = "", bty = "n")##
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
hist(uniform_prior$treeModel.rootHeight, main = "", border = "white", col = "darkgrey", 
     ylab = "Prior Prob.", xlab = "", yaxt = "n", xaxt = "n")
axis(1, at = c(0, 25000, 50000), labels = c(0, 25, 50), las = 1)
plot(uniform_prior$treeModel.rootHeight, log10(uniform_prior$clock.rate), 
     pch = 20, col = rgb(0, 0, 0, 0.2), ylab = "", 
     xlab = expression(paste("Root height (x", 10^{2}, ")")), 
     main = "", bty = "n", xaxt = "n", yaxt = "n")
axis(1, at = c(0, 25000, 50000), labels = c(0, 25, 50), las = 1)
axis(2, at = log10(c(1e-9, 1e-6, 10^-3)), labels = c(0.01, 10, 1000), las = 2)
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
#plot(log10(uniform_prior$clock.rate), log10(uniform_prior$treeLength), pch = 20, col = rgb(0, 0, 0, 0.2), 
#     xlab = "", ylab = "", bty = "n")
#plot(log10(uniform_prior$clock.rate), log10(uniform_prior$treeModel.rootHeight), pch = 20, col = rgb(0, 0, 0, 0.2), 
#     xlab = "", ylab = "", bty = "n")
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
hist(log10(uniform_prior$clock.rate), main = "", border = "white", col = "darkgrey", 
     ylab = "Prior prob.", 
     xlab = expression(paste("Clock rate (", 10^{-7}, ")" , collapse = "")), 
     xaxt = "n", yaxt = "n", xlim = c(-8, -2))
axis(1, at = log10(c(1e-8, 1e-6, 10^-3)), labels = c(0.1, 10, 1000), las = 1)
dev.off()

stop("w")

phi1 <- read.table("prior_predictive_simulations/cholera_aln_fixed_phi1.log", head = T, sep = "\t")
phi10 <- read.table("prior_predictive_simulations/cholera_aln_fixed_phi10.log", head = T, sep = "\t")
phi100 <- read.table("prior_predictive_simulations/cholera_aln_fixed_phi100.log", head = T, sep = "\t")
phi1000 <- read.table("prior_predictive_simulations/cholera_aln_fixed_phi1000.log", head = T, sep = "\t")

# Here plot densities of tree length, tree height and clock rate for fixed values of Phi. 
# Each column is the simulated statistic for fixed values of Phi. Note that they are in 
# log10 scale
par(mfcol = c(4, 3), mar = c(4, 4, 3, 1))
hist(log10(phi1$treeLength), xlim = c(2, 4.3))
hist(log10(phi10$treeLength), xlim = c(2, 4.3))
hist(log10(phi100$treeLength), xlim = c(2, 4.3))
hist(log10(phi1000$treeLength), xlim = c(2, 4.3))
#
hist(log10(phi1$treeModel.rootHeight), xlim = c(1.3, 3.8))
hist(log10(phi10$treeModel.rootHeight), xlim = c(1.3, 3.8))
hist(log10(phi100$treeModel.rootHeight), xlim = c(1.3, 3.8))
hist(log10(phi1000$treeModel.rootHeight), xlim = c(1.3, 3.8))
#
hist(log10(phi1$clock.rate), xlim = c(-8, -1))
hist(log10(phi10$clock.rate), xlim = c(-8, -1))
hist(log10(phi100$clock.rate), xlim = c(-8, -1))
hist(log10(phi1000$clock.rate), xlim = c(-8, -1))
