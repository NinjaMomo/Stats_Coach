getColLabels <- function(html)
{
  tableName <- 'Pre-campaign spend category'
  cNames <- sapply(getNodeSet(html, paste("//table[@aria-label = '", tableName, "']//tr/td[contains(@class, 'columnLabels') and contains(@class, 'role3')]", sep = "")), xmlValue)
  cNames
}

getRowLabels <- function(html)
{
  tableName <- 'Pre-campaign spend category'
  rNames <- sapply(getNodeSet(html, paste("//table[@aria-label = '", tableName, "']//tr/td[contains(@class, 'rowLabels') and contains(@class, 'role3')]", sep = "")), xmlValue)
  rNames
}


getValues <- function(x)
{
  a <- 'Pre-campaign spend category'
  
  for(i in 1:length(getNodeSet(x, "//table[@aria-label = 'Pre-campaign spend category']//tr")))
  {
    j = 1
    tdValues <- c()
    rowFound <- FALSE
    
    while(length(getNodeSet(x, paste("//table[@aria-label = '", a, "']//tr[",i,"]/td[", j,"]", sep = ""))) > 0)
    {
      tdValues <- c(tdValues, xmlValue(getNodeSet(x, paste("//table[@aria-label = '", a, "']//tr[",i,"]/td[", j,"]", sep = ""))[[1]]))
      j = j + 1
    }
    print(tdValues)
    
  }
}
