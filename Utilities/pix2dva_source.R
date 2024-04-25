

pix2DVA <- function(x, scr_sz_px = 1920, scr_sz_cm = 135.89, par_dist_cm = 86.36,
                    return_cm = FALSE){
  
  # Input Args:
  
  # x              = numeric, the value in pixels to be converted
  # scr_sz_px      = numeric, one dimension (length or width) of the monitor/screen in pixels 
  # scr_sz_cm      = numeric, the SAME dimension of the monitor/screen in centimeters
  # par_dist       = numeric, viewing distance of observer in centimeters
  # return_cm      = logical, should function also return size in centimeters?
  
  # Note: f designed with Dassonville Lab eye-tracking projector as defaults (2020)
  # Change default parameters of scr_sz_px, scr_sz_cm, par_dist_cm for different
  # experimental configurations 
  
  
  # Convert to pix to cm
  cm <- (x * scr_sz_cm) / scr_sz_px 
  
  # arctan(Opp / Adj) to get angle in radians
  theta <- atan(cm / par_dist_cm)
  
  # Convert radians to degrees
  degree <- (180 * theta) / pi
  
  # Round to something nice
  dva <- round(degree, 4)
  
  # Should return CM?
  if(return_cm == TRUE){
    
    output <- (c(cm , dva))
    names(output) <- c("cm", "dva")
    return(output)
    
  } else {
    
    return(dva) 
    
  }
  
}

#pix2DVA(150)
