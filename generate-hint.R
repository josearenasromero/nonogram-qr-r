generate_hint <- function(sm, rnd=0.8) {
  temp_sm <- sm
  for (r in 1:nrow(temp_sm)) {
    for (c in 1:ncol(temp_sm)) {
      cval <- temp_sm[r,c]
      if(cval > 0) {
        rval <- runif(1)
        if(rval > rnd) {
          temp_sm[r,c] <- 0
        }
      }
    }
  }
  
  base_string_horizontal <- ""
  base_string_vertical <- ""
  
  for(r in 1:ncol(temp_sm)) {
    base_number <- 0
    #print("start col")
    first_time = FALSE
    for(c in 1:nrow(temp_sm)) {
      #print(temp_sm[r,c])
      #print("start row")
      #print(temp_sm[r,c])
      if(temp_sm[r,c] > 0) {
        base_number <- base_number + temp_sm[r,c]
        first_time = TRUE
      }
      else {
        if(first_time) {
          base_string_horizontal <- paste(base_string_horizontal, base_number, ",", sep="")
          first_time = FALSE
        }
        base_number <- 0
        #if(ncol(temp_sm) == c && nrow(temp_sm) == r) {
        #base_string <- paste(base_string, base_number, "-", sep="")
        #} else {
        #base_string <- paste(base_string, base_number, ",", sep="")
        #}
        
      }
    }
    #if(base_number > 0 && ncol(temp_sm) == c && nrow(temp_sm) == r) { 
    #  base_string <- paste(base_string, base_number, "-", sep="")
    #  print("HOLA")
    #} else if(base_number > 0) { 
    if(base_number > 0) { 
      base_string_horizontal <- paste(base_string_horizontal, base_number, ":", sep="")
      base_number <- 0
    }
    
    if(substr(base_string_horizontal,nchar(base_string_horizontal),nchar(base_string_horizontal)) == ",") { 
      
      base_string_horizontal <- substr(base_string_horizontal,1,nchar(base_string_horizontal)-1)
      base_string_horizontal <- paste(base_string_horizontal, ":", sep="")
    }
  }
  
  #START FOR VERTICAL
  for(r in 1:nrow(temp_sm)) {
    base_number <- 0
    #print("start col")
    first_time = FALSE
    for(c in 1:ncol(temp_sm)) {
      #print(temp_sm[r,c])
      #print("start row")
      #print(temp_sm[c,r])
      if(temp_sm[c,r] > 0) {
        base_number <- base_number + temp_sm[c,r]
        first_time = TRUE
      }
      else {
        if(first_time) {
          #el problema es que aca cuando la fila solo tiene 1 valor, se raya
          base_string_vertical <- paste(base_string_vertical, base_number, ",", sep="")
          first_time = FALSE
        }
        base_number <- 0
        #if(ncol(temp_sm) == c && nrow(temp_sm) == r) {
        #base_string <- paste(base_string, base_number, "-", sep="")
        #} else {
        #base_string <- paste(base_string, base_number, ",", sep="")
        #}
        
      }
    }
    #if(base_number > 0 && ncol(temp_sm) == c && nrow(temp_sm) == r) { 
    #  base_string <- paste(base_string, base_number, "-", sep="")
    #  print("HOLA")
    #} else if(base_number > 0) { 
    if(base_number > 0) { 
      base_string_vertical <- paste(base_string_vertical, base_number, ":", sep="")
      base_number <- 0
    }
    
    if(substr(base_string_vertical,nchar(base_string_vertical),nchar(base_string_vertical)) == ",") { 
      
      base_string_vertical <- substr(base_string_vertical,1,nchar(base_string_vertical)-1)
      base_string_vertical <- paste(base_string_vertical, ":", sep="")
    }
  }
  
  base_string_horizontal <- substr(base_string_horizontal,1,nchar(base_string_horizontal)-1)
  base_string_vertical <- substr(base_string_vertical,1,nchar(base_string_vertical)-1)
  result <- list("hint"=paste(base_string_horizontal, "-", base_string_vertical, sep=""), "sm"=temp_sm)
  
  return (result)
}