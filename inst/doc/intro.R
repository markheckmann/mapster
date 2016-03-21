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
s <- map_data(x)
map_setup(s)
map_grid()
map_points(s, col="black", cex=1)
map_labels(s)
z <- s[ , c("x", "y", "z")]
map_lines(z[4, ], z[2, ], col="red")
map_lines(z[c(1,7,8,9,16), ], z[14, ], col="blue")

## ----fig.width=7---------------------------------------------------------
library(convexhull)
map_setup(s)
map_grid()
map_points(s)
map_labels(s)
pp <- s[ , c("xp", "yp")]
g1 <- c(1,14,16,18)
g2 <- c(1,5,7,9,14,19) 
convex_hull(pp[g1, ], col="#FF000030", smooth=2, shape = 2)
convex_hull(pp[g2, ], col="#0000FF30", smooth=2, shape = 2)
map_text(z[c(7,16), ], c("men", "women"), col="blue", cex=1.5, pos=1)

## ------------------------------------------------------------------------
x <- data.frame(x = runif(4),
                y = runif(4),
                z = runif(4))
x <- round(x, 1)
s <- add_sphere_coords(x)
s

## ------------------------------------------------------------------------
s <- add_proj_data(s)
s

## ------------------------------------------------------------------------
s <- map_data(x)
s

