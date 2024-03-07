setwd("/home/sebastiand/Dropbox/projects_WORKING/BETS_prior_sensitivity/BETS_prior_sensitivity_ms/sandbox_figures")

# For pairwise plots
uniform_prior <- tail(read.table("prior_predictive_simulations/cholera_aln_prior_uniformPhi.log", 
                                 head = T, sep = "\t"), 1000)

# Here we plot pairwise of Phi, tree length, tree height, and clock rate for the tree priors
par(mfcol = c(4, 4), mar = c(4, 4, 0.5, 1))
hist(uniform_prior$constant.popSize, main = "")
plot(uniform_prior$constant.popSize, uniform_prior$treeLength)
plot(uniform_prior$constant.popSize, uniform_prior$treeModel.rootHeight)
plot(uniform_prior$constant.popSize, uniform_prior$clock.rate)
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
hist(uniform_prior$treeLength, main = "")
plot(uniform_prior$treeLength, uniform_prior$treeModel.rootHeight)
plot(uniform_prior$treeLength, uniform_prior$clock.rate)
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
hist(uniform_prior$treeModel.rootHeight, main = "")
plot(uniform_prior$treeModel.rootHeight, uniform_prior$clock.rate)
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
plot(1, 1, type ="n", bty = "n", xaxt = "n", yaxt= "n", ylab = "", xlab = "", main = "")
hist(log10(uniform_prior$clock.rate), main = "")

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
