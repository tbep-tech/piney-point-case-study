# try simple load, download if fail
rdataload <- function(x){
  
  fl <- basename(x)
  obj <- gsub('\\.RData$', '', fl)
  flurl <- x
  
  # try simple load
  ld <- try(load(url(flurl)), silent = T)
  
  # return x if load worked
  if(!inherits(ld, 'try-error')){
    out <- get(obj)
  }
  
  # download x if load failed
  if(inherits(ld, 'try-error')){
    
    fl <- paste(tempdir(), fl, sep = '/')
    download.file(flurl, destfile = fl, quiet = T)
    load(file = fl)
    out <- get(obj)
    suppressMessages(file.remove(fl))
    
  }
  
  return(out)
  
}