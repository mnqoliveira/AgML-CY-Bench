# Monique Oliveira - moniquepgoliveira@gmail.com

# Libraries ---------------------------------------------------------------
libraries <- c("here", "tidyverse", "data.table")

lapply(libraries, require, character.only = TRUE)

# Functions ---------------------------------------------------------------
read_filter <- function(ids, filename, it, max_rows, colnames_){
  
  skip_ = max_rows * it
  x <- fread(filename, skip=skip_, nrows = max_rows)
  
  if(!is.null(colnames_)){
    colnames(x) <- colnames_
  }
  
  test_ <- !"year" %in% colnames(x)
  if(test_){
    x <- x[, year := as.numeric(substring(date, 1, 4))]
  }
  
  cols <- setdiff(names(x), c("adm_id", "crop_name", "date"))
  x_ <- x[ids, on = c("adm_id", "year")][, (cols) := lapply(.SD, round, 4), .SDcols = cols]
  
  if(test_){
    x_ <- copy(x_[, year := NULL])
  }
  
  return(x_)
  
}

read_predictors <- function(ids, filename, num_rows, max_rows){
  
  file_l <- list()
  
  if(num_rows > max_rows){
    n_its <- (num_rows %/% max_rows)-1
    
    colnames_ <- NULL
    
    for (it in 0:n_its){
      
      print(it)
      temp <- read_filter(ids, filename, it, max_rows, colnames_)
      file_l[[it+1]] <- temp
      
      if (it == 0){
        colnames_ <- colnames(temp)
      }
      
    }
    
    if ((num_rows %% max_rows) != 0){
      temp <- read_filter(ids, filename, it+1, 
                          (num_rows-((it+1)*max_rows)), colnames_)
      file_l[[it+2]] <- temp
    }
    
  } else {
    temp <- read_filter(ids, filename, 0, num_rows, colnames_=NULL)
    file_l[[1]] <- temp
    
  }
    
  return(file_l)
  
}

