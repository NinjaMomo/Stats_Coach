main <- function(htmlAddress)
{
  source("setup.R")
  sourceAll()
  importLibraries()

  ## Uncomment below to view tables and column/row names
  #tree <<- Tree(htmlTreeParse(htmlAddress,
  #                            useInternal = TRUE))
  #treeObjs.unravelTablesToDataFrame(tree)
  
  setGlobals(Tree(htmlTreeParse(htmlAddress,
                                useInternal = TRUE)))
  
  createOutput()
  #deleteFile(htmlAddress)
}