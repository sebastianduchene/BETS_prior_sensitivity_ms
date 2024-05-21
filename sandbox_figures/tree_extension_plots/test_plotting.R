library(NELSI)
library(RColorBrewer)
pal <- brewer.pal(8, "Set1")

tr <- rtree(10, br = runif)

ordinates <- data.frame(get.ordinates(tr))

class(ordinates)

plot(1,1, type = "n", xlim = c(-1, 1), ylim = c(0, 1), bty = "n")
for(i in seq(0, 5, by = 0.25)) lines(c(i, i), c(-1, 22), lty = 3)

for(i in 1:nrow(tr$edge)){
    ordinate_start <- ordinates$node.index == tr$edge[i, 1]
    ordinate_end <- ordinates$node.index == tr$edge[i, 2]
    print(log10(1+c(ordinates$x.coord[ordinate_start], ordinates$x.coord[ordinate_end])))
    lines(log10(1+c(ordinates$x.coord[ordinate_start], ordinates$x.coord[ordinate_end])),
          log10(1+c(ordinates$y.coord[ordinate_start], ordinates$y.coord[ordinate_end])),
          type = "S", col = pal[2], lwd = 4)
}

tr2 <- rtree(10)
ordinates <- data.frame(get.ordinates(tr2))

for(i in 1:nrow(tr2$edge)){
    ordinate_start <- ordinates$node.index == tr2$edge[i, 1]
    ordinate_end <- ordinates$node.index == tr2$edge[i, 2]
    lines(c(ordinates$x.coord[ordinate_start], ordinates$x.coord[ordinate_end]),
          c(ordinates$y.coord[ordinate_start], ordinates$y.coord[ordinate_end]) + 10,
          type = "S", col = pal[3], lwd = 4)
}


rotate <- function(v, angle){
    tmatrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2, byrow=T)
    return(tmatrix %*% v)
}

rotate(c(3, 1), pi/2)

ordinates
ordinates_rotated <- get.ordinates(tr2)
for(i in 1:nrow(ordinates_rotated)){
    ordinates_rotated[i, c("x.coord", "y.coord")] <- rotate(c(ordinates_rotated[i, "x.coord"], ordinates_rotated[i, "y.coord"]), 3*pi/2)
}

ordinates_rotated <- data.frame(ordinates_rotated)
ordinates_rotated
ordinates_rotated$y.coord <- ordinates_rotated$y.coord - min(ordinates_rotated$y.coord)

plot(1,1, type = "n", xlim = c(0, 20), ylim = c(0, 10), bty = "n")
for(i in seq(0, 10, by = 1)) lines(c(-1, 20), c(i, i), lty = 3)

for(i in 1:nrow(tr2$edge)){
    ordinate_start <- ordinates_rotated$node.index == tr2$edge[i, 1]
    ordinate_end <- ordinates_rotated$node.index == tr2$edge[i, 2]
    lines(c(ordinates_rotated$x.coord[ordinate_start], ordinates_rotated$x.coord[ordinate_end]),
          c(ordinates_rotated$y.coord[ordinate_start], ordinates_rotated$y.coord[ordinate_end]),
          col = pal[2], lwd = 4, type = "s")
}

###################

rotate <- function(v, angle){
  tmatrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2, byrow=T)
  return(tmatrix %*% v)
}


plot.tree.lines <- function(tree, rotation.angle, x.offset = 0, y.offset = 0, log.scale = F){
  ordinates <- get.ordinates(tree)
  ordinates_rotated <- ordinates
  for(i in 1:nrow(ordinates_rotated)){
    ordinates_rotated[i, c("x.coord", "y.coord")] <- rotate(c(ordinates_rotated[i, "x.coord"], ordinates_rotated[i, "y.coord"]), rotation.angle)
  }
  ordinates_rotated <- data.frame(ordinates_rotated)
  if(y.offset == 0){
    ordinates_rotated$y.coord <- ordinates_rotated$y.coord - min(ordinates_rotated$y.coord)
  } else {
    ordinates_rotated$y.coord <- ordinates_rotated$y.coord + y.offset
  }
  if(x.offset != 0){
    ordinates_rotated$x.coord <- ordinates_rotated$x.coord + x.offset
  }
  if(log.scale){
    ordinates_rotated$y.coord <- log10(1 + ordinates_rotated$y.coord)
  }
  for(i in 1:nrow(tree$edge)){
    ordinate_start <- ordinates_rotated$node.index == tree$edge[i, 1]
    ordinate_end <- ordinates_rotated$node.index == tree$edge[i, 2]
#    points(ordinates_rotated$x.coord[ordinate_start], ordinates_rotated$y.coord[ordinate_start], col = pal[2], pch = 19)
#    segments(ordinates_rotated$x.coord[ordinate_start], ordinates_rotated$y.coord[ordinate_start],
#             ordinates_rotated$x.coord[ordinate_end], ordinates_rotated$y.coord[ordinate_end],
#             col = pal[2], lwd = 1)
    lines(c(ordinates_rotated$x.coord[ordinate_start], ordinates_rotated$x.coord[ordinate_end]),
          c(ordinates_rotated$y.coord[ordinate_start], ordinates_rotated$y.coord[ordinate_end]),
          col = "darkgrey", lwd = 2, type = "l")
  }
  tips <- ordinates_rotated$node.index %in% 1:length(tree$tip.label)
  points(ordinates_rotated$x.coord[tips], ordinates_rotated$y.coord[tips], col = pal[2], pch = 20)
}

setwd("/home/sebastiand/Downloads/test_small_popsize1_bounds")

pdf("tree_distortion_ultrametric.pdf", useDingbats = F, width = 8, height = 7)
par(mar = c(5, 4, 1, 1))
plot(1,1, type = "n", xlim = c(0, 230), ylim = c(0, 3.2), bty = "n", yaxt = "n", ylab = "Time", 
     xaxt = "n", xlab = "")
axis(2, at = log10(1+c(0, 0.5, 2, 5, 10, 50, 100, 500, 1000)), 
     labels = c(0, 0.5, 2, 5, 10, 50, 100, 500, 1000), las = 2)
axis(1, at = c(25, 75, 135, 200), labels = c("SC\nisochronous\n(True model)", "SC\nheterochronous", 
                                             "UCLD clock\nisochronous", "UCLD clock\nheterochronous"), 
     padj = 0.7)
for(i in log10(1+c(0, 0.5, 2, 5, 10, 50, 100, 500, 1000))) lines(c(-10, 220), c(i, i), lty = 2, lwd = 0.5)

ultrametric_sc <- reorder.phylo(x = read.nexus("ultrametric_5__SC_iso_gamma.tre"), 
                                  order = "postorder")
plot.tree.lines(ultrametric_sc, 3*pi/2, 0, 0, log.scale = T)

heterochronous_sc <- reorder.phylo(x = read.nexus("ultrametric_5__SC_het_gamma.tre"), 
                                  order = "postorder")
plot.tree.lines(heterochronous_sc, 3*pi/2, x.offset = 55, 0, log.scale =T)

ultrametric_ucld <- reorder.phylo(x = read.nexus("ultrametric_5__UCLD_iso_gamma.tre"), 
                                  order = "postorder")
plot.tree.lines(ultrametric_ucld, 3*pi/2, x.offset = 110, 0, log.scale = T)

heterochronous_ucld <- reorder.phylo(x = read.nexus("ultrametric_5__UCLD_het_gamma.tre"), 
                                  order = "postorder")
plot.tree.lines(heterochronous_ucld, 3*pi/2, x.offset = 170, 0, log.scale = T)
dev.off()


pdf("tree_distortion_heterochronous.pdf", useDingbats = F, width = 8, height = 7)
par(mar = c(5, 4, 1, 1))
plot(1,1, type = "n", xlim = c(0, 230), ylim = c(0, 0.8), bty = "n", yaxt = "n", ylab = "Time", 
     xaxt = "n", xlab = "")
axis(2, at = log10(1+c(0, 0.1, 0.2, 0.4, 0.5, 1, 2, 3, 4)), 
     labels = c(0, 0.1, 0.2, 0.4, 0.5, 1, 2, 3, 4), las = 2)
axis(1, at = c(25, 75, 135, 200), labels = c("SC\nisochronous", "SC\nheterochronous\n(True model)", 
                                             "UCLD clock\nisochronous", "UCLD clock\nheterochronous"), 
     padj = 0.7)
for(i in log10(1+c(0, 0.1, 0.2, 0.4, 0.5, 1, 2, 3, 4))) lines(c(-10, 220), c(i, i), lty = 2, lwd = 0.5)

ultrametric_sc <- reorder.phylo(x = read.nexus("heterochronous_5__SC_iso_gamma.tre"), 
                                order = "postorder")
plot.tree.lines(ultrametric_sc, 3*pi/2, 0, 0, log.scale = T)

heterochronous_sc <- reorder.phylo(x = read.nexus("heterochronous_5__SC_het_gamma.tre"), 
                                   order = "postorder")
plot.tree.lines(heterochronous_sc, 3*pi/2, x.offset = 55, 0, log.scale =T)

ultrametric_ucld <- reorder.phylo(x = read.nexus("heterochronous_5__UCLD_iso_gamma.tre"), 
                                  order = "postorder")
plot.tree.lines(ultrametric_ucld, 3*pi/2, x.offset = 115
                , 0, log.scale = T)

heterochronous_ucld <- reorder.phylo(x = read.nexus("heterochronous_5__UCLD_het_gamma.tre"), 
                                     order = "postorder")
plot.tree.lines(heterochronous_ucld, 3*pi/2, x.offset = 170, 0, log.scale = T)
dev.off()
