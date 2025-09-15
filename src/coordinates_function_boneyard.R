## Geo bone yard


# DMS -> Decimal Degrees
dms_to_dd <- function(x) {
  # Extract numbers (deg, min, sec) and hemisphere
  m <- regexec("(-?\\d+)°\\s*(\\d+)'\\s*(\\d+(?:\\.\\d+)?)\"?\\s*([NSEW]?)", x)
  parts <- regmatches(x, m)[[1]]
  
  if (length(parts) == 0) return(as.numeric(x))  # if not matching, assume decimal
  
  deg <- as.numeric(parts[2])
  min <- as.numeric(parts[3])
  sec <- as.numeric(parts[4])
  hemi <- parts[5]
  
  dd <- abs(deg) + min/60 + sec/3600
  if (toupper(hemi) %in% c("S", "W")) dd <- -dd
  return(dd)
}

# DM -> Decimal Degrees
dm_to_dd <- function(x) {
  m <- regexec("(-?\\d+)°\\s*(\\d+(?:\\.\\d+)?)'?\\s*([NSEW]?)", x)
  parts <- regmatches(x, m)[[1]]
  
  if (length(parts) == 0) return(as.numeric(x))  # if not matching, assume decimal
  
  deg <- as.numeric(parts[2])
  min <- as.numeric(parts[3])
  hemi <- parts[4]
  
  dd <- abs(deg) + min/60
  if (toupper(hemi) %in% c("S", "W")) dd <- -dd
  return(dd)
}

# General wrapper that auto-detects format
coord_to_dd <- function(x) {
  x <- trimws(x)
  if (grepl("°.*'.*\"", x)) {
    dms_to_dd(x)
  } else if (grepl("°.*'", x)) {
    dm_to_dd(x)
  } else {
    as.numeric(x) # assume already decimal
  }
}

coord_to_dd <- function(x) {
  x <- trimws(x)
  
  # Normalize symbols
  x <- gsub("˚", "°", x)      # weird degree symbol
  x <- gsub("′", "'", x)      # prime → apostrophe
  x <- gsub("’", "'", x)      # sometimes curly apostrophe
  x <- gsub("″", "\"", x)     # double-prime → quote
  x <- gsub("“|”", "\"", x)   # curly quotes → standard quote
  
  # DMS: degrees, minutes, seconds
  m <- regexec(
    "(-?\\d+)°\\s*(\\d+)['’]?\\s*(\\d+(?:\\.\\d+)?)[\"“”]?\\s*([NSEW]?)",
    x
  )
  parts <- regmatches(x, m)[[1]]
  if (length(parts) > 0) {
    deg  <- as.numeric(parts[2])
    min  <- as.numeric(parts[3])
    sec  <- as.numeric(parts[4])
    hemi <- parts[5]
    dd <- abs(deg) + min/60 + sec/3600
    if (toupper(hemi) %in% c("S","W")) dd <- -dd
    return(dd)
  }
  
  # DM: degrees + decimal minutes
  m <- regexec("(-?\\d+)°\\s*(\\d+(?:\\.\\d+)?)['’]?\\s*([NSEW]?)", x)
  parts <- regmatches(x, m)[[1]]
  if (length(parts) > 0) {
    deg  <- as.numeric(parts[2])
    min  <- as.numeric(parts[3])
    hemi <- parts[4]
    dd <- abs(deg) + min/60
    if (toupper(hemi) %in% c("S","W")) dd <- -dd
    return(dd)
  }
  
  # Decimal degrees with hemisphere
  m <- regexec("(-?\\d+(?:\\.\\d+)?)°?\\s*([NSEW])", x)
  parts <- regmatches(x, m)[[1]]
  if (length(parts) > 0) {
    dd <- as.numeric(parts[2])
    hemi <- parts[3]
    if (toupper(hemi) %in% c("S","W")) dd <- -dd
    return(dd)
  }
  
  # Plain decimal
  as.numeric(x)
}


x <-'6° 32′ 50.41″ N'
library(measurements)

# Example conversion
dms_to_dd <- function(coord) {
  # Remove symbols and split
  parts <- unlist(strsplit(gsub("[°'\"]", " ", coord), "\\s+"))
  deg <- as.numeric(parts[1])
  min <- as.numeric(parts[2])
  sec <- ifelse(length(parts) > 2, as.numeric(parts[3]), 0)
  direction <- tail(parts, 1)
  
  dd <- deg + min / 60 + sec / 3600
  if (direction %in% c("S", "W")) dd <- -dd
  return(dd)
}

convert_to_dd <- function(coord) {
  # Try direct numeric conversion
  if (suppressWarnings(!is.na(as.numeric(coord)))) {
    return(as.numeric(coord))
  }
  
  # scrub from parzer 
  coord <- trimws(gsub("[^A-Za-z0-9\\.\\ ,'-]|d|g", "'", coord))
  
  
  
  # Extract components
  pattern <- "([0-9]+)\\s+([0-9]+)?\\s*([0-9\\.]+)?\\s*([NSEW])"
  match <- regexec(pattern, coord)
  parts <- regmatches(coord, match)[[1]]
  
  if (length(parts) >= 5) {
    deg <- as.numeric(parts[2])
    min <- ifelse(parts[3] != "", as.numeric(parts[3]), 0)
    sec <- ifelse(parts[4] != "", as.numeric(parts[4]), 0)
    dir <- parts[5]
    
    dd <- deg + min / 60 + sec / 3600
    if (dir %in% c("S", "W")) dd <- -dd
    return(dd)
  }
  
  return(NA)
}

# scrub <- function(x) trimws(gsub("[^A-Za-z0-9\\.\\ ,'-]|d|g", "'", x))