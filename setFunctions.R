setCrosstabs <- function(htmlTree)
{
  OutputLine <<- "here is some example text"
}

setFrequencies <- function(htmlTree)
{
  #tables <- getRelevantTables(getTableNames(htmlTree), TablesToRemove())
  OutputLine <<- "here is some example text"
}


#Function to set global variables (<<-) needed for output
#
#Args:
#   htmlTree: Tree Object
#
#Returns:
#   NULL
#
#Note
#    - global variables set using <<-

setTTest <- function(htmlTree)
{
  StatsTable <- treeObjs.search4Table(htmlTree, "One-Sample Statistics")[[1]]
  TestTable <- treeObjs.search4Table(htmlTree, "Test Value")[[1]]

  StatsDF <<- tableObjs.tableToDataFrame(StatsTable)
  TestDF <<- tableObjs.tableToDataFrame(TestTable)

  output <- c()
  output <- setIntro(output)
  template_sig <- "
###### Summary of %s :
This variable has `%0.2f` observations with a mean of `%0.2f`. The t-test value is `%0.2f`. Since p-value < 0.05 (it is p=`%0.2f`) 
we can say that the mean value is statistically significantly different from `%0.2f`.
" # dont't forget the newline
  
  template_nosig <- "
### Summary of %s :
This variable has `%0.2f` observations with a mean of `%0.2f`. The t-test value is `%0.2f`. Since p-value > 0.05 (it is p=`%0.2f`)  
we can say that the mean value is not statistically significantly different from `%0.2f`.
" # dont't forget the newline
  
  for (i in seq(length(row.names(StatsDF)))) {
    
    if (as.numeric(TestDF$`Sig. (2-tailed)`)[i] <= 0.05) {
      output <- c(output, paste(sprintf(template_sig , 
                                        row.names(StatsDF)[i], 
                                        as.numeric(StatsDF$N)[i], 
                                        as.numeric(StatsDF$Mean)[i], 
                                        as.numeric(TestDF$t)[i], 
                                        as.numeric(TestDF$`Sig. (2-tailed)`)[i], 
                                        as.numeric(strsplit(TestTable$get('name'), "=")[[1]][2])[i])))
    } else {
      output <- c(output, paste(sprintf(template_nosig, 
                                        row.names(StatsDF)[i], 
                                        as.numeric(StatsDF$N)[i], 
                                        as.numeric(StatsDF$Mean)[i], 
                                        as.numeric(TestDF$t)[i], 
                                        as.numeric(TestDF$`Sig. (2-tailed)`)[i], 
                                        as.numeric(strsplit(TestTable$get('name'), "=")[[1]][2])[i])))
    }
    
  }
  output <<- output
}

#Function to create rmarkdown output.
#
#Args:
#   NULL
#
#Returns:
#   NULL
#
#Note
#    - error (tryCatch) checking done to ensure output document is closed before rendering word document

##TO DO: Add warning suppression toward end of development

createOutput <- function()
{
  
  run <- TRUE
  
  while(run)
  {
    run <- tryCatch(
      {
        #Leave warning suppression till the end 
        #suppressWarnings(render('Mich_Output.Rmd', quiet = TRUE))
        render('Mich_Output.Rmd', quiet = TRUE)
        FALSE
      }, 
      error = function(err)
      {
        print(err)
        readline(prompt="Please close word document then press [enter] to continue")
        TRUE
      })
  }
}

setIntro <- function(output)
{
  output <- c(output, "# Statistical Interpreter Output\n\n")
  output <- c(output, "Welcome to your Statistical Interpreter brought to you by [OLSPS Analytics](http://www.olsps.com/ 'Title'). We're here to hold your hand.\n\n")
  output <- c(output, "## Summary of your analysis\n\n")
  
  #replace here with generic
  output <- c(output, paste("In Statistics SPSS you ran a T-Test on the following variables: ", paste(row.names(StatsDF), collapse = ", "), "\n\n"))
  
  output <- c(output, "## Interpretation of your analysis\n\n")
  
  output
}

##options(warn = -1) Switch off warnings globally