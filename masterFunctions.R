#Function to set Statistics SPSS functions as we provide output for them
#
#Args:
#   NULL
#
#Returns:
#   String vector containing Statistics functions
#
#Note:
#   - The name entered here MUST match that of the function call in Statistics as shown in the log output

MasterList <- function()
{
  masterList <- c("CROSSTABS", "FREQUENCIES VARIABLES", "T-TEST PAIRS", "T-TEST")
  masterList
}

#Function to set which Statistics tables are not of interest
#
#Args:
#   NULL
#
#Returns:
#   String vector containing Statistics table names

TablesToRemove <- function()
{
  tablesToRemove <- c("Notes", "Statistics")
  tablesToRemove
}

#Function to call relevant setfunction for rmarkdown globals
#
#Args:
#   Tree:  Tree object
#
#Returns:
#   NULL
#
#NOTE:
#     - switch statement argument order MUST match MasterList order

setGlobals <- function(Tree)
{
  if (is.na(match(Tree$get("name"), MasterList())))
  {
    print("Output functionality not provided")
  }
  else
  {
    switch(match(Tree$get("name"), MasterList()), 
           setCrosstabs(Tree),
           setFrequencies(Tree),
           setPairsTTest(Tree),
           setTTest(Tree))
  }
}

#Possibly use to distinguish between "row tables" and "column tables"
TableType <- function()
{
  tableType <- c('CROSSTABS' = 1, 'FREQUENCIES VARIABLES' = 1)
}