#' Deploys R script as PL/R function
#'
#' \code{pullDataFromGreenplum} Once churned through the algorithm 
#' and settled on it for sample dataset, 
#' want to deploy as PL/R function on Greenplum. This generates the 
#' PL/R function syntax
#'
#' compileRToPLR(script= 'working-document.Rmd'
#'               , inputs= list(dat1= dat1), problemKey = 'id'
#'               , output= data.frame(x= 1, y= 2)
#'               , name= 'schema.function'
#'               , outputFile= '')
#'
#'
#'
#' @param script a file path
#' script: file path for the script
compileRToPLR <- function(script, inputs, problemKey, output, name, outputFile){
  # script is a R markdown file which needs the code parsed out of it
  # this TBD
  # Question: could the R script also be parameterized to be a normal R script?
  
  # Question: should I check the R script body for mention of the 
  #   compileRToPLR funtion in case user did not follow chunk name 
  #   guideline? 
  
  rmdFile <- readLines(con = script)
  
  fileLines <- 1:(length(rmdFile))
  
  if (substring(script, first = nchar(script) - 3, last= nchar(script)) == '.Rmd'){
    tildeLines <- grep(rmdFile, pattern= '```')
    index <- rep(1:2, times= length(tildeLines)/2)
    
    rBody <- paste(rmdFile[tildeLines[3] < fileLines & fileLines < tildeLines[4]]
                   , collapse= '\n'
    )
  } else {
    if (substring(script, first = nchar(script) - 1, last= nchar(script)) == '.R'){
      rBody <- paste(rmdFile, collapse= '\n')
    } else {
      print('invalid file')
    }
  }
  
  # inputs are data.frames which define the arguments 
  types <- sapply(inputs[[1]], class)
  types <- gsub(x= types, pattern= 'numeric', replacement= 'DOUBLE PRECISION[]')
  funcHeading <- paste('CREATE OR REPLACE FUNCTION '
                       , name, '(', problemKey, '\n\t'
                       , paste(paste(names(inputs[[1]]), types, sep= ' '), collapse= '\n\t, ')
                       , '\n)\n'
                       , 'RETURNS SETOF '
                       , name, '_tp AS \n'
                       , '$BODY$\n'
                       , sep= ''
                       )
  
  # the R script is also going to need to create the data.frames
  # from vectors, since that is how they come in from Greenplum
  
  rHeading <- paste(names(inputs)[1], ' <- data.frame(\n  '
                    , paste(
                      paste(names(inputs[[1]]), '= ', names(inputs[[1]]), sep= '')
                      , collapse= '\n  , '
                      )
                    , '\n  )'
                    , collapse= ''
  )
  
  # output is a data.frame which defines the return type, 
  funcReturn <- paste('RETURN(returnDf)'
                      , '$BODY$'
                      , 'LANGUAGE plr VOLATILE;'
                      , sep= ' \n'
                      )
  
  #  must match the output table
  write(x= paste(funcHeading, rHeading, rBody, funcReturn, sep= '\n\n\n')
        , file = paste(name, '.sql', sep= '')
  )
  
  # inputs are data.frames which define the arguments 
  types <- sapply(output, class)
  types <- gsub(x= types, pattern= 'numeric', replacement= 'DOUBLE PRECISION[]')
  funcType <- paste('CREATE TYPE AS '
                       , name, '(' 
                       , paste(paste(names(output), types, sep= ' '), collapse= '\n\t, ')
                       , '\n)\n;\n'
                       , sep= ''
  )

  write(x= funcType
        , file = paste(name, '_tp.sql', sep= '')
  )
  
  # write out the execution query as well
  query <- paste('SELECT (plr_output).*'
                 , 'FROM (' 
                 , paste('SELECT ', name, '(', sep= '')
                 , paste(names(inputs[[1]]), collapse= ', ')
                 , ') AS plr_output'
                 , 'FROM input_table'
                 , ') AS q;'
                 , sep= '\n'
  )
  
  #  execute the query
  write(x= query
        , file = paste('execute', name, '.sql', sep= '')
  )
  
  return(0)
}



