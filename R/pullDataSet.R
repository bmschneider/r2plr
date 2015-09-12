#' Pulls data from database
#'
#' \code{pullDataSet} returns data.frame with results of 
#' the query in a given .sql file while replacing arguments with given 
#' values. 
#'
#' NOTE: all the vars should be in a named list
#'
#' @param f A .sql file for the query
pullDataSet <- function(f, varList, conn){
  
  #require(RPostgreSQL) # shouldn't want a require statement here
  temp <- readLines(con = f)
  query <- paste(temp, collapse= '\n')

  if (length(varList) > 0){
    for (i in 1:length(varList)){
      query <- gsub(x= query
                    #, pattern= paste('&', names(varList[[i]]), '.', sep= '')
                    , pattern= names(varList)[i]
                    , replacement= varList[[i]]
      )
    }
  }
  
  d <- dbSendQuery(conn, statement= query)
  data <- dbFetch(res = d)
  
  return(data)
}

# this will get all the R data.frames desired within the R environment
# one can now write R code on top of it, preferably into R markdown document
