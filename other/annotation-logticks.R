annotation_logticks <- function (base = 10, sides = "bl",
                                 short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"), ...) {
  require(grid)
  GeomLogticks$new(base = base, sides = sides,
                   short = short, mid = mid, long = long, ...)
}

require(proto)
GeomLogticks <- proto(ggplot2:::Geom, {
  objname <- "logticks"
  
  draw <- function(., data, scales, coordinates, base = 10, sides = "bl",
                   short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"), ...) {
    
    ticks <- list()
    
    # TODO:
    # Allow linear or log space
    # Make tick calculation conditional
    
    # Convert these units to numbers so that they can be put in data frames
    short <- convertUnit(short, "cm", valueOnly = TRUE)
    mid   <- convertUnit(mid,   "cm", valueOnly = TRUE)
    long  <- convertUnit(long,  "cm", valueOnly = TRUE)
    
    # Get positions of x and y tick marks
    xticks <- calc_logticks(base = base,
                            minpow = floor(scales$x.range[1]), maxpow = ceiling(scales$x.range[2]),
                            start = 0, shortend = short, midend = mid, longend = long)
    names(xticks)[names(xticks)=="value"] <- "x"
    
    yticks <- calc_logticks(base = base,
                            minpow = floor(scales$y.range[1]), maxpow = ceiling(scales$y.range[2]),
                            start = 0, shortend = short, midend = mid, longend = long)
    names(yticks)[names(yticks)=="value"] <- "y"
    
    xticks <- coord_transform(coordinates, xticks, scales)    
    yticks <- coord_transform(coordinates, yticks, scales)    
    
    
    if(grepl("b", sides)) {
      ticks$x_b <- with(data, segmentsGrob(
        x0 = unit(xticks$x, "native"), x1 = unit(xticks$x, "native"), 
        y0 = unit(xticks$start, "cm"), y1 = unit(xticks$end, "cm"),
        gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
      ))
    }
    if(grepl("t", sides)) {
      ticks$x_t <- with(data, segmentsGrob(
        x0 = unit(xticks$x, "native"), x1 = unit(xticks$x, "native"), 
        y0 = unit(1, "npc") - unit(xticks$start, "cm"), y1 = unit(1, "npc") - unit(xticks$end, "cm"),
        gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
      ))
    }
    if(grepl("l", sides)) {
      ticks$y_l <- with(data, segmentsGrob(
        y0 = unit(yticks$y, "native"), y1 = unit(yticks$y, "native"), 
        x0 = unit(yticks$start, "cm"), x1 = unit(yticks$end, "cm"),
        gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
      ))
    }
    if(grepl("r", sides)) {
      ticks$y_r <- with(data, segmentsGrob(
        y0 = unit(yticks$y, "native"), y1 = unit(yticks$y, "native"), 
        x0 = unit(1, "npc") - unit(yticks$start, "cm"), x1 = unit(1, "npc") - unit(yticks$end, "cm"),
        gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
      ))
    }
    
    gTree(children = do.call("gList", ticks))
  }
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = 1)
  guide_geom <- function(.) "path"
})



calc_logticks <- function(base = 10, ticks.per.base = base - 1,
                          minpow = 0, maxpow = 4, start = 0, shortend = .1, midend = .2, longend = .3) {
  
  # Number of blocks of tick marks
  reps <- maxpow - minpow
  
  # For base 10: 1, 2, 3, ..., 7, 8, 9, 1, 2, ...
  ticknums  <- rep(seq(1, base-1, length.out = ticks.per.base), reps)
  
  # For base 10: 1, 1, 1, ..., 1, 1, 1, 2, 2, ... (for example)
  powers <- rep(seq(minpow, maxpow-1), each = ticks.per.base)
  
  ticks  <- ticknums * base^powers
  ticks  <- c(ticks, base^maxpow)  # Add the last tick mark
  ticks  <- log(ticks, base)
  
  # Set all of the ticks short
  tickend <- rep(shortend, length(ticks))
  
  # Set the "major" ticks long
  # Get the position within each cycle, 0, 1, 2, ..., 8, 0, 1, 2. ...
  cycleIdx <- ticknums - 1
  tickend[cycleIdx == 0] <- longend # Set beginning of each cycle long
  
  # Where to place the longer tick marks that are between each base
  # For base 10, this will be at each 5
  longtick.after.base <- floor(ticks.per.base/2)
  tickend[ cycleIdx == longtick.after.base ] <- midend
  
  tickdf <- data.frame(value = ticks, start = start, end = tickend)
  
  return(tickdf)
}
