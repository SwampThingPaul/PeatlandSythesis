

## Convert coordinates 

dms_to_dd <- function(coord) {
  # Remove spaces and extract direction
  coord <- gsub(" ", "", coord)
  dir <- substring(coord, nchar(coord))
  
  # Extract degrees, minutes, seconds
  matches <- regmatches(coord, regexec("([0-9]+)°([0-9]+)'([0-9.]+)\"", coord))[[1]]
  deg <- as.numeric(matches[2])
  min <- as.numeric(matches[3])
  sec <- as.numeric(matches[4])
  
  # Convert to decimal degrees
  dd <- deg + min / 60 + sec / 3600
  
  # Apply negative sign if direction is S or W
  if (dir %in% c("S", "W")) {
    dd <- -dd
  }
  
  return(dd)
}

# Example
dms_to_dd("54°50'2.44\"S")
# [1] -54.83401
