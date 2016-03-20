
# #' Set and query circplot settings
# #' 
# #' @param ... Key values pairs seperated by comma or a list of key value pairs as 
# #'  e.g. returned by the function itself.
# #' @examples
# #' circ_par()
# #' circ_par(x.from = c(0,1))   # change input range from 0 to 1
# #' 
# map_par <- function(...)
# {
#   cur.settings <- options("circ")[[1]]
#   parnames <- names(cur.settings)
#   
#   args <- list(...)
#   
#   if (length(args) == 0) {
#     return(cur.settings)
#   }
#   
#   # flatten if a list key values is supplied (e.g. the old par object)
#   # Note that list(...) wraps it into another list which is not wanted
#   if (is.list(args[[1]])) {
#     args <- args[[1]]
#   }
#       
#   if (is.null(names(args)) & all(unlist(lapply(args, is.character)))) {
#     pm <- pmatch(unlist(args), parnames)
#     return(cur.settings[na.omit(pm)])
#   } else {
#     names(args) <- parnames[pmatch(names(args), parnames)]
#     new.settings <- modifyList(cur.settings, args)
#     options(circ=new.settings)
#     invisible(new.settings)
#   }
# }
# 
# 
# #' Initialize circplot parameters
# #' 
# #' The values are saved in the field \code{circ} using the \code{options} function.
# #' The following parameters can be set:
# #' \enumerate{
# #'   \item x.from Range of original data (default \code{[0,1]}).
# #'   \item x.to Range data is mapped to, usually 0 to 2pi.
# #'   \item y.from Range of original data (default \code{[0,1]}).
# #'   \item y.to Range data is mapped to (default \code{[1,1.1]})
# #'   \item n.seg Number of segments used when drawing lines.
# #'   \item rings Border radi of rings (defaults is three rings \code{c(1.0, 1.1, 1.2, 1.3)})
# #' }
# #' 
# map_par_init <- function()
# {
#   l <- list()
#   l$x.from <- c(0, 1)
#   l$x.to <- c(0, 2*pi)
#   l$y.from <- c(0, 1)
#   l$y.to <- c(1, 1.1)  
#   l$n.seg <- 100          # number of segments used to draw forms
#   l$rings <- 10:14/10
#   # circ$width <- .1
#   # circ$line <- 1
#   options(circ=l)
# }



################################################################################

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



#' Generate random data for plotting
#' 
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
#' 
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
#' 
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


#' Prepares the data for plotting
#' 
#' The function needs a dataframe as input. The arguments that are accepted
#' cartesian coordinates \code{x}, \code{y}, \code{z},) spherical coordinates
#' (\code{phi}, \code{theta}, \code{r}), or projected user coordinates 
#' (\code{xp}, \code{yp}). If all are supplied, they are respected in reverse order, 
#' i.e. projected user coordinates are used if given.
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


#' Set up plotting region
#' 
#' Convenient wrapper around \code{plot}
#' 
map_setup <- function(s, scale=1)
{
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
#' 
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
map_points <- function(x, ...) 
{
  x <- add_args_to_dataframe(x, ...)
  x <- add_args_to_dataframe(x, pch=16, col="black", cex=1, replace=FALSE)
  
  x <- map_data(x)
  
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
map_labels <- function(x, ...)
{
  x <- add_args_to_dataframe(x, ...)
  x <- add_args_to_dataframe(x, label="", pch=16, cex=1, col="black", replace=FALSE)

  x <- map_data(x)
  
  o <- na.omit(x)   # pointLabel does not work on missing position values
    
  l.xy <- maptools::pointLabel(o$xp, o$yp, labels=o$label, cex=o$cex, do=FALSE)
  text(l.xy, labels = o$label, cex=o$cex, col=o$col)       
}


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
#' 
map_line <- function(x, y, n=200, ...)
{
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


#' Draw text from cartesian coords
#' 
map_text <- function(x, labels="", ...)
{
  x <- check_matrix(x)
  s <- cart_to_surface_of_sphere(x)
  p <- add_proj_data(s)
  pp <- p[c("xp", "yp")]
  text(pp, labels = labels, ...)
}





















