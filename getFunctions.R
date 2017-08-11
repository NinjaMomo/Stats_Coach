getStatFunction <- function(htmlTree, masterList)
{
  desc <- sapply(getNodeSet(htmlTree, "//body"), xmlValue)
  desc <- gsub("\\s", " ", desc)
  
  for (i in masterList)
  {
    if (grepl(i, desc))
    {
      StatisticsFunction <- i
    }
  }
  
  StatisticsFunction
}

getTableNames <- function(htmlTree)
{
  tableNames <- sapply(getNodeSet(htmlTree, "//table/caption"), xmlValue, recursive = F)
  
  tableNames
}

getRelevantTables <- function(tables, tablesToRemove)
{
  for (i in tablesToRemove)
  {
    tables <- tables[i != tables]
  }
  
  tables
}



