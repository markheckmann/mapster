


#' Generate random data for map plotting
#' @export
random_data <- function(n, prefix="")
{
  x <- replicate(3, runif(n, -1, 1))
  colnames(x) <- c("x", "y", "z")
  x <- as.data.frame(x)
  #x$label <- words::sentences(nwords, s = n, lang = "en", maxchar = Inf)
  ii <- 1L:nrow(x)
  x$label <- paste0(prefix, ii)
#   x$col <- 1
#   x$cex <- 1
  x
}


#' 3d cartesians to sperical with fixed radius
#' 
#' @keywords internal
#' 
cart_to_surface_of_sphere <- function(x, r=1)
{
  s <- t(apply(x, 1, cartesian_to_spherical_coords))
  colnames(s) <- c("phi", "theta", "r")
  as.data.frame(s)
}


#' Add colummns with spherical coordinates
#' 
#' @keywords internal
#' @export
add_sphere_coords <- function(x, r=1)
{
  xyz <- x[c("x", "y", "z")]
  s <- cart_to_surface_of_sphere(xyz, r=r)
  
  cbind_replace(x, s)
}


#' Add projections as user coords based on spherical coords
#' 
#' Uses last projection \code{.Last.projection()} to calc projected
#' coordinates. Adds the variables \code{xp} and \code{yp} to dataframe
#' based on spherical coordinates \code{phi} and \code{theta}.
#' 
#' @keywords internal
#' @export
add_proj_data <- function(s, proj="", parameters=NULL, orientation=NULL) 
{
  p <- mapproj::mapproject(s$phi, s$theta, proj=proj, 
                           parameters=NULL, orientation=orientation)
  xy <- cbind(xp = p$x, yp = p$y)
  cbind_replace(s, xy)  
}


#' Set projection and orientation
#' 
#' Wrapper for \code{mapproj::mapproject}
#' @export
#' 
map_proj <- function(preset=NULL, proj="", parameters=NULL, orientation=NULL)
{
  if (!is.null(preset)) {
    preset <- match.arg(preset, c("mollweide", "orthographic"))
    if (preset == "mollweide") {
      proj <- preset
      orientation = c(90,0,0) 
    } 
    if (preset == "orthographic") {
      proj <- preset
      orientation = c(0,0,0) 
    }   
  }
  
  mapproj::mapproject(0, 0, proj=proj, parameters=parameters, 
                       orientation=orientation)  
  invisible(NULL)
}


#' Convert a vector of length three into dataframe with columns x, y, z.
#' @keywords internal
#' 
vec_to_xyz_df <- function(x)
{
  if (is.vector(x) & length(x) == 3) {
    x <- as.data.frame(rbind(x))
    colnames(x) <- c("x", "y", "z")
  }
  x
}


#' Prepares the data for plotting
#' 
#' The function needs a dataframe as input. The arguments that are accepted
#' cartesian coordinates \code{x}, \code{y}, \code{z},) spherical coordinates
#' (\code{phi}, \code{theta}, \code{r}), or projected user coordinates 
#' (\code{xp}, \code{yp}). If all are supplied, they are respected in reverse order, 
#' i.e. projected user coordinates are used if given.
#' 
#' @export
#' 
map_data <- function(x, preset=NULL, proj="", parameters=NULL, orientation=NULL)
{
  map_proj(preset=preset, proj=proj, parameters=parameters, 
               orientation=orientation)
  
  nms <- colnames(x)
  
  has.xyz <- all(c("x", "y", "z") %in% nms)
  has.sphere <- all(c("phi", "theta") %in% nms)  # r optinal as not used
  has.proj <- all(c("xp", "yp") %in% nms)
     
  if (!(has.xyz | has.sphere | has.proj)) {
    stop("Either cartesian, spherical or projected user ",  
         "coordinates must be supplied.", call. = FALSE)
  }
        
  if (has.xyz) {
    x <- add_sphere_coords(x)
    x <- add_proj_data(x)
  } 
  if (has.sphere) {
    x <- add_proj_data(x)
  }
#   
#   if (has.proj) {
#     # do nothing and return x
#   } else if (has.sphere) {
#     x <- add_proj_data(x)
#   } else if (has.xyz) {
#     x <- add_sphere_coords(x)
#     x <- add_proj_data(x)
#   } else {
#     stop("Either cartesian, spherical or projected user ",  
#          "coordinates must be supplied.", call. = FALSE)
#   }
  x
}



#' Find centroid of a set of points
#' 
#' @export
centroid <- function(x, subset=.())
{
  x <- map_data(x)
  s <- subset2(x, subset)
  s <- coordinate_set(s)
  m <- col_means(s)
  map_data(m)   # implicitly xyz are used for caclulation as map_data builds all on xyz
}




#### +----------- CREATING THE PLOT -----------  ####


#' Set up plotting region
#' 
#' Convenient wrapper around \code{plot}
#' @export
#' 
map_setup <- function(s, scale=1)
{
  s <- map_data(s)
  xmax <- max(abs(s$xp), na.rm=T)
  ymax <- max(abs(s$yp), na.rm=T)
  x.lim <- c(-xmax, xmax) * scale               
  y.lim <- c(-ymax, ymax) * scale  
  plot(NULL, xlim=x.lim, ylim=y.lim, type="n", pch=4, 
       xaxt="n", yaxt="n", xlab="", ylab="", asp=1, bty="n")  
}


#' Add grid lines to map
#' 
#' Convenient wrapper around \code{mapproj::map.grid}
#'
#' @export 
map_grid <- function(lim = c(-180, 180, -90, 90), lty=1, col=grey(.6))
{
  mapproj::map.grid(lim, nx=20, ny=20, font=1, col=col, cex=.6, lty=lty, labels=F) 
}


# order of evaluation: is argument supplied?
# is argument in dataframe?
# else use default value


#' Adds arguments supplied explicity to dataframe.
#' 
#' Most functions have default values whoch can be overwritten.
#' 
#' @param x Info dataframe.
#' @param ... Arguments that are added as columns to dataframe.
#' @param replace Replace existing columns? If set to \code{FALSE} 
#'  default arguments can be supplied.
#' @keywords internal
#
add_args_to_dataframe <- function(x, ..., replace=TRUE)
{
  dots <- list(...)  
  args <- names(dots)
  if (!replace)
    args <- setdiff(args, colnames(x))
  for (arg in args) {
    x[[arg]] <- dots[[arg]]
  }
  x
}


#' Draw points on map
#' 
#' @export
map_points <- function(x, subset=.(), ...) 
{
  x <- add_args_to_dataframe(x, ...)
  x <- add_args_to_dataframe(x, pch=16, col="black", cex=1, replace=FALSE)
  
  warn_if_var_is_factor(x, c("col", "pch"))
  
  x <- map_data(x)
  x <- subset2(x, subset)
  
  points(x$xp, x$yp, pch=x$pch, col=x$col, cex=x$cex)             
}


# #' Draw points on map
# #' 
# map_points <- function(s) 
# {
#   points(s$xp, s$yp, pch=16, col=s$color, cex=s$cex)             
# }


#' Add point labels to map
#' 
#' \code{map_labels} positions labels using an pabels positioning algorithm.
#' \code{map_text} also plots text but passes all arguments directly to \code{text} while
#'  in \code{map_labels} they are mapped from the dataframe.
#'  
#' @param x A dataframe.
#' @param subset Logical condition to select subset. Either as a string or using
#'   the \code{\link{plyr::.}} function.
#' @param ... Arguments that are added as columns to dataframe.
#' @export
#' @rdname map-text
#' 
map_labels <- function(x, subset=.(), ...)
{
  x <- check_matrix(x)
  
  x <- add_args_to_dataframe(x, ...)
  x <- add_args_to_dataframe(x, label="", pch=16, cex=1, col="black", replace=FALSE)

  x <- map_data(x)
  x <- subset2(x, subset)
  
  o <- na.omit(x)   # pointLabel does not work on missing position values
    
  l.xy <- maptools::pointLabel(o$xp, o$yp, labels=o$label, cex=o$cex, do=FALSE)
  text(l.xy, labels = o$label, cex=o$cex, col=o$col)       
}


#' @rdname map-text
#' @export
map_text <- function(x, ..., subset=.())
{
  x <- check_matrix(x)
  
  x <- add_args_to_dataframe(x, ...)
  x <- add_args_to_dataframe(x, label="", pch=16, cex=1, col="black", offset=.5, replace=FALSE)
  
  x <- map_data(x)
  x <- subset2(x, subset)
  xy <- c_proj(x)
  text(xy, labels = x$label, cex=x$cex, col=x$col, pos=x$pos, offset=x$offset)    
}


# map_text <- function(x, labels="", ..., subset=.())
# {
#   x <- check_matrix(x)
#   x <- map_data(x)
#   x <- subset2(x, subset)
#   pp <- c_proj(x)
#   text(pp, labels = labels, ...)
# }

# x,y two points. x and y can have any number of dimensions.
#
interpolate_between_points <- function(x, y, n =100)
{
  inter <- function(i) x + i/n *(y - x)
  t(sapply(0L:n, inter))
}


#' Draw line between two points
#' 
#' @keywords internal
#' @export
map_line <- function(x, y, n=200, ...)
{
 
  # extract only x,y,z coords (might need generalization in te future)
  x <- c_cart(x)
  y <- c_cart(y)
  
  # find points on line
  s <- interpolate_between_points(x, y, n = n)    # interpolated points in n-dim
  s <- cart_to_sphere(s)                          # get spherical coords 
  s <- as.data.frame(s)
  
  #colnames(s) <- c("phi", "theta", "r") 
  s <- add_proj_data(s)                           # uses Last.projection() settings if proj=""
  h <- s[, c("xp", "yp")]        
  h <- na.omit(h)
            
  # check if point changes "around the world" (i.e. big differences in xp or yp)
  # and omit this segment
  n0 <- 1L:(length(h$xp) - 1) 
  n1 <- n0 + 1     
  delta.x <- abs(h$xp[n0] - h$xp[n1])
  delta.y <- abs(h$yp[n0] - h$yp[n1]) 
  draw <- !delta.x > .1 | delta.y > .1
  n0.draw <- n0[draw]
  n1.draw <- n1[draw]
  segments(h$xp[n0.draw], h$yp[n0.draw], h$xp[n1.draw], h$yp[n1.draw], ...)
}


#' Draw lines between all points in x and y
#' 
#' @param x,y Either a matrix ot vector with three columns (x,y,z).
#'  Spherical and projected coords are currently not supported).
#' @param n Number of segments used to draw the lines.
#' @export
map_lines <- function(x, y, n=200, ...)
{
  x <- check_matrix(x)
  y <- check_matrix(y)
  ii <- nrow(x)
  jj <- nrow(y)
  for (i in 1L:ii) {
    for (j in 1L:jj) {
      map_line(x[i, ], y[j, ], n=n, ...)
    }
  }
}




#' Draw centroid of set of points
#' 
#' @export
map_centroid_point <- function(x, subset=.(), ...)
{
  s <- subset2(x, subset)
  c <- centroid(s)
  map_points(c, ...)
}


#' Draw a star (lines from centroid to points)
#'  
#' @export
map_centroid_star <- function(x, subset=.(), ...)
{
  s <- subset2(x, subset)
  c <- centroid(s)
  map_lines(s, c, ...)
}



#' Draw label for centroid.
#' 
#' Label must currently be set explicitly.
#'  
#' @export
map_centroid_label <- function(x, label="", subset=.(), ...)
{
  s <- subset2(x, subset)
  c <- centroid(s)
  pp <- c_proj(c)
  text(pp, labels = label, ...)
}





#### +----------- OLD STUFF -----------  ####

# example projection

# p <- mapproject(s$phi, s$theta, proj="orthographic", 
#                 parameters=NULL, orientation=c(0,0,0))
# xy <- cbind(xp = p$x, yp = p$y)
# s <- cbind(s, xy)
# s  
# par(mar=rep(2, 4))
# lim <- c(-180, 180, -90, 90)
# xmax <- max(abs(s$xp), na.rm=T) 
# ymax <- max(abs(s$yp), na.rm=T) 
# x.lim <- c(-xmax, xmax)                
# y.lim <- c(-ymax, ymax)   
# plot(NULL, xlim=x.lim, ylim=y.lim, type="n", pch=4, 
#      xaxt="n", yaxt="n", xlab="", ylab="", asp=1, bty="n")
# map.grid(lim, nx=20, ny=20, font=1, col=grey(.6), cex=.6, lty=2, labels=T)
# points(s$xp, s$yp, pch=16, col=s$color, cex=s$cex)             
# 
# sn <- na.omit(s)
# l.xy <- maptools::pointLabel(sn$xp, sn$yp, labels=sn$label, cex=sn$cex, do=FALSE)
# text(l.xy, sn$label, cex=sn$cex, col=sn$color)     
