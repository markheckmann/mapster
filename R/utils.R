
#### UTILS ####

as.radian <- function (degree) 
{
  degree/180 * pi
}


as.degree <- function (rad) 
{
  rad * 180 / pi
}

# # as.degree(as.radian(1:18*20))
# 
# 
# #' Draw a circle
# #' 
# #' Wrapper for \code{\link{plotrix::draw.circle}}
# #' 
# draw_circle <- function(...)
# {
#   plotrix::draw.circle(...)
# }



#' Column binding of new columns, replacement of existing ones
#' 
#' @examples
#' a <- data.frame(a="a1")
#' ab <- data.frame(a="a2", b="b1")
#' c <- data.frame(c="c1")
#' bc <- data.frame(b="b2", c="c2")
#' 
#' cbind_replace(a, c)
#' cbind_replace(a, ab)
#' cbind_replace(ab, bc)
#' 
#' @keywords internal
#' 
cbind_replace <- function(x, y)
{
  xy <- cbind(x, y)
  nms <- colnames(xy)
  rem <- duplicated(nms, fromLast = TRUE)
  xy[, !rem]
}


# other: r = (x2 + y2 + z2)1/2,     q =tan-1(z/(x2+y2)1/2),     f = tan-1(y/x). 
# 
cartesian_to_spherical_coords <- function(v)
{
  v <- unlist(v)
  phi <- atan2(v[2], v[1]) * 180/pi
  theta <- 90 - acos(v[3]/sum(v^2)^.5) * 180/pi 
  r <- (v[1]^2 + v[2]^2 + v[3]^2)^.5 
  # if (r == 0 & is.nan(theta))        # in case r=0 no angle can be calculated NaN is set to zero
  #     theta <- 0
  #   if (phi < 0)
  #     phi <- phi + 360
  #   if (theta < 0)
  #     theta <- theta + 360      
  c(phi=phi, theta=theta, r=r)
}


# Works for vectors and matrices
# Always returns a matrix
#
cart_to_sphere <- function(xyz)
{
  if (is.vector(xyz)) {
    res <- cartesian_to_spherical_coords(xyz)
  } else {
    res <- t(apply(xyz, 1, cartesian_to_spherical_coords))
    colnames(res) <- c("phi", "theta", "r")
  }
  res
}


#' Check if x is a matrix and convert vectors
#' 
#' @keywords internal
#' 
check_matrix <- function(x)
{
  x <- vec_to_xyz_df(x)
  if (is.vector(x)) 
    x <- matrix(x, nrow=1)
  if (! is.matrix(x) & ! is.data.frame(x))
    stop("'x' must be a vector, matrix or dataframe")
  x
}  

  
#' Get coordinate columns from dataframe
#' 
#' Comvencience wrapper to get cartesian (\code{}, \code{}, \code{}), 
#' spherical (\code{phi}, \code{theta}, \code{r}), or
#' projected (\code{xp}, \code{yp}) coordinate columns.
#' 
#' @param x A dataframe.
#' @export
#' 
#' @rdname coords
c_cart <- function(x) 
{
  x[ , c("x", "y", "z"), drop=FALSE]
}

#' @rdname coords
#' @export
#' 
c_sphere <- function(x) 
{
  x[ , c("phi", "theta", "r"), drop=FALSE]
}

#' @rdname coords
#' @export
#' 
c_proj <- function(x)
{
  x[ , c("xp", "yp"), drop=FALSE]
}



# extract coordinate set

coordinate_set <- function(x)
{
  xyz <- c("x", "y", "z")
  sphere <- c("phi", "theta", "r")
  proj <- c("xp", "yp")
  v <- c(xyz, sphere, proj)
  x[ , v, drop=FALSE]
}



# to avoid colMeans which converts to matrix or vector
col_means <- numcolwise(mean, na.rm=T)






