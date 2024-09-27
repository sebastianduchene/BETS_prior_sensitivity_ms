setwd("~/Documents/Work/Pasteur/BETS/figures/sandbox")
library(ggplot2)
library(GGally)
library(gridExtra)

# For pairwise plots
uniform_prior <- tail(read.table("cholera_aln_prior_uniformPhi.log", 
                                 head = T, sep = "\t"), 1000)

uniform_prior <- subset(uniform_prior, select=c("constant.popSize","treeLength","treeModel.rootHeight","clock.rate"))
#uniform_prior$clock.rate <- log10(uniform_prior$clock.rate)
#colnames(uniform_prior)[4] <- "log10(clock.rate)"

plots <- ggpairs(uniform_prior, diag = list(continuous = "barDiag"), upper = list(continuous = "blank")) + 
  theme_bw() + 
  theme(axis.text=element_text(size=8),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
for (i in ncol(uniform_prior)) {
  plots[i,4] <- plots[i,4] + scale_x_continuous(limits=c(seq(0,0.0006,0.0001)))
}
pdf("prior_predictive_uniform.pdf",width=9)
plots
dev.off()

phi1 <- tail(read.table("cholera_aln_fixed_phi1.log", head = T, sep = "\t"),760)
phi1 <-subset(phi1, select=c("treeLength","treeModel.rootHeight","clock.rate"))

phi10 <- tail(read.table("cholera_aln_fixed_phi10.log", head = T, sep = "\t"),673)
phi10 <-subset(phi10, select=c("treeLength","treeModel.rootHeight","clock.rate"))

phi100 <- tail(read.table("cholera_aln_fixed_phi100.log", head = T, sep = "\t"),638)
phi100 <-subset(phi100, select=c("treeLength","treeModel.rootHeight","clock.rate"))

phi1000 <- tail(read.table("cholera_aln_fixed_phi1000.log", head = T, sep = "\t"),448)
phi1000 <-subset(phi1000, select=c("treeLength","treeModel.rootHeight","clock.rate"))

ggplot_borders_nogrid <- theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

phi1_treeLength <- ggplot(phi1, aes(x=treeLength)) + geom_histogram() + ggplot_borders_nogrid
phi1_rootHeight <- ggplot(phi1, aes(x=treeModel.rootHeight)) + geom_histogram() + ggplot_borders_nogrid
phi1_clockRate <- ggplot(phi1, aes(x=clock.rate)) + geom_histogram() + ggplot_borders_nogrid

phi10_treeLength <- ggplot(phi10, aes(x=treeLength)) + geom_histogram() + ggplot_borders_nogrid
phi10_rootHeight <- ggplot(phi10, aes(x=treeModel.rootHeight)) + geom_histogram() + ggplot_borders_nogrid
phi10_clockRate <- ggplot(phi10, aes(x=clock.rate)) + geom_histogram() + ggplot_borders_nogrid

phi100_treeLength <- ggplot(phi100, aes(x=treeLength)) + geom_histogram() + ggplot_borders_nogrid
phi100_rootHeight <- ggplot(phi100, aes(x=treeModel.rootHeight)) + geom_histogram() + ggplot_borders_nogrid
phi100_clockRate <- ggplot(phi100, aes(x=clock.rate)) + geom_histogram() + ggplot_borders_nogrid

phi1000_treeLength <- ggplot(phi1000, aes(x=treeLength)) + geom_histogram() + ggplot_borders_nogrid
phi1000_rootHeight <- ggplot(phi1000, aes(x=treeModel.rootHeight)) + geom_histogram() + ggplot_borders_nogrid
phi1000_clockRate <- ggplot(phi1000, aes(x=clock.rate)) + geom_histogram() + ggplot_borders_nogrid

pdf("tree_prior_rates_times.pdf")
grid.arrange(arrangeGrob(phi1_treeLength,phi1_rootHeight,phi1_clockRate,left="Phi = 1",ncol=3),
             arrangeGrob(phi10_treeLength,phi10_rootHeight,phi10_clockRate,left="Phi = 10",ncol=3),
             arrangeGrob(phi100_treeLength,phi100_rootHeight,phi100_clockRate,left="Phi = 100",ncol=3),
             arrangeGrob(phi1000_treeLength,phi1000_rootHeight,phi1000_clockRate,left="Phi = 1000",ncol=3),nrow=4) 
dev.off()
