## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
  fig.width=5, 
  fig.height=5,
  fig.align = "center"
)

## ----setup, warning=FALSE, message=FALSE---------------------------------
library(mapster)

## ------------------------------------------------------------------------
x <- data.frame(x=c(1, 0, 0, -1, 0, 0),
                y=c(0, 1, 0, 0, -1, 0),
                z=c(0, 0, 1, 0, 0, -1))
rownames(x) <- c("X+", "Y+", "Z+", "X-", "Y-", "Z-")
x

## ------------------------------------------------------------------------
map_proj(preset="ortho")  # use presets and set before projecting

## ------------------------------------------------------------------------
s <- map_data(x, preset="ortho")
s

## ------------------------------------------------------------------------
s$label <- rownames(s)
s$col <- rep(c("darkgreen", "red"), each=3)
s$cex <- 1
s

## ----fig.height=3.5------------------------------------------------------
par(mar=rep(1,4))
map_setup(s)
map_grid()
map_points(s)
map_labels(s)

## ----fig.height=3.5------------------------------------------------------
par(mar=rep(1,4))
map_proj("mollweide")  # set projection type
s <- map_data(s)       # recalculate data, can usually be omitted
map_setup(s)
map_grid()
map_points(s, cex=2:3, col=scales::alpha(s$col, .3))
map_labels(s, label=1:6)

## ----fig.width=7---------------------------------------------------------
set.seed(0)
x <- random_data(20)
x$cex <- .7
par(mar=rep(1,4))
map_proj("mollweide")  # use presets and set before projecting
x <- map_data(x)
map_setup(x)
map_grid()
map_points(x, col="black", cex=1)
map_labels(x)
map_lines(x[4, ], x[2, ], col="red")
map_lines(x[c(1,7,8,9,16), ], x[14, ], col="blue")

## ----fig.width=7---------------------------------------------------------
library(convexhull)
map_setup(x)
map_grid()
map_points(x)
map_labels(x)
pp <- c_proj(x)
g1 <- c(1,14,16,18)
g2 <- c(1,5,7,9,14,19) 
d <- convex_hull(pp[g1, ], col="#FF000030", smooth=2, shape = 2)
convex_hull(pp[g2, ], col="#0000FF30", smooth=2, shape = 2)
map_text(x[c(7,16), ], label=c("men", "women"), col="blue", cex=1.5, pos=c(1,3))

## ------------------------------------------------------------------------
map_setup(x)
map_grid()
map_points(x, "phi >= 0", col="darkgreen")
map_labels(x, "phi >= 0")

## ------------------------------------------------------------------------
map_setup(x)
map_grid()
ss <- .(phi >= 0)
map_points(x, ss)
map_centroid_point(x, ss, col="red")
map_centroid_star(x, ss, col="red")
map_centroid_label(x, "centroid", ss, pos=4)

## ------------------------------------------------------------------------
map_setup(x)
map_grid()
c <- centroid(x, ss)                # calc centroid coordinates
map_points(c, cex=2, pch=17, col="blue")
s <- subset2(x, ss)    # select a subset of points
map_lines(s, c, col="blue")

## ------------------------------------------------------------------------
x <- data.frame(x = runif(4),
                y = runif(4),
                z = runif(4))
x <- round(x, 1)
s <- add_sphere_coords(x)
s

## ----results='hide'------------------------------------------------------
s <- add_proj_data(s)
s

## ----results='hide'------------------------------------------------------
s <- map_data(x)
s

## ----results='hide'------------------------------------------------------
c_cart(s)
c_sphere(s)
c_proj(s)

