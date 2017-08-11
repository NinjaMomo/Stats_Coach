#Function to emulate class/object functionality
#
#Args:
#   treeHTML: HTMLInternalDocument (Use htmlTreeParse with 'useInternal' = TRUE)
#   name: Tree name (Function call in masterList), DEFAULT = ""
#
#Returns:
#     treeObjs: list


Tree <- function(treeHTML, name = "") {
  
  #List of class "fields" and generic get/set methods
  treeObjs = list(treeHTML = treeHTML,
                  name = name, 
                  tableNames = c(),
                  relevantTableNames = c(),
                  tables = c(),
                  #Function to get specified value
                  #
                  #Args:
                  #     x: treeObjs object
                  #
                  #Returns:
                  #     treeObjs[[x]]
                  get = function(x) treeObjs[[x]],
                  
                  #Function to set specified value
                  #
                  #Args:
                  #     x: treeObjs object
                  #     value: value to set
                  #
                  #Returns:
                  #     NULL
                  set = function(x, value) treeObjs[[x]] <<- value)
  
  #Add methods to the list
  
  #Function to set the name of the Tree
  #
  #Args:
  #   htmlTree: HTMLInternalDocument (Use htmlTreeParse with 'useInternal' = TRUE), DEFAULT = treeHTML
  #   masterList: List of relevant function names, DEFAULT = MasterList()
  #
  #Returns:
  #     NULL
  
  treeObjs$setName <- function(htmlTree = treeHTML, masterList = MasterList())
  {
    n <- treeObjs$name
    desc <- sapply(getNodeSet(htmlTree, "//body"), xmlValue)
    desc <- gsub("\\s", " ", desc)                                            #replace '\s' with " "
    
    for (i in masterList)
    {
      if (grepl(i, desc))
      {
        treeName <- i
      }
    }
    
    n <- c(treeName)
    
    assign('name', n, envir=treeObjs)                                         #assign values to treeObjs environment
  }
  
  
  #Function to set the name of Table objects in the Tree
  #
  #Args:
  #   htmlTree: HTMLInternalDocument (Use htmlTreeParse with 'useInternal' = TRUE), DEFAULT = treeHTML
  #
  #Returns:
  #     NULL
  
  treeObjs$setTableNames <- function(htmlTree = treeHTML)
  {
    t <- treeObjs$tableNames
    
    tableNames <- sapply(getNodeSet(htmlTree, "//table/caption"), xmlValue, recursive = FALSE)
    
    t <- c(t,tableNames)
    
    assign("tableNames", t, envir = treeObjs)
  }
  
  
  #Function to set the name of relevant Table objects in the Tree. Some tables may be undesirable as specified in TablesToRemove()
  #
  #Args:
  #   tables: list of all table names, DEFAULT = treeObjs$get("tableNames")
  #   tablesToRemove: list of tables to remove, DEFAULT = TablesToRemove()
  #
  #Returns:
  #     NULL
  
  treeObjs$setRelevantTables <- function(tables = treeObjs$get("tableNames"), tablesToRemove = TablesToRemove())
  {
    tr <- treeObjs$relevantTableNames
    
    for (i in tablesToRemove)
    {
      tables <- tables[i != tables]
    }
    
    tr <- c(tr, tables)
    
    assign("relevantTableNames", tr, envir = treeObjs)
  }
  
  #Function to create list of Table objects
  #
  #Args:
  #   tableNames: List of desired tables, DEFAULT = treeObjs$get("relevantTableNames")
  #
  #Returns:
  #     NULL
  
  treeObjs$createTables <- function(tableNames = treeObjs$get("relevantTableNames"))
  {
    tr <- treeObjs$tables
    
    for(i in 1:length(tableNames))
    {
      name <- tableNames[i]
      numCols <- as.numeric(xmlAttrs(getNodeSet(treeObjs$get("treeHTML"), 
                                                paste("//table[@aria-label = '",                   #unique tables found by aria-label
                                                      name, "']", 
                                                      sep = ""))[[1]])[['data-numcols']])
      newTable <- Table(name = name, dataCols = c(1:numCols))
      tr[[name]] <- newTable
    }
    
    assign("tables", tr, envir = treeObjs)
  }
  
  
  #Function to set all Tables. Calls treeObjs.setTable()
  #
  #Args:
  #   tableNames: List of Table objects, DEFAULT = treeObjs$get("tables")
  #   HTML: HTMLInternalDocument, DEFAULT = treeObjs$get("treeHTML")
  #
  #Returns:
  #     NULL
  
  treeObjs$setTables <- function(tables = treeObjs$get("tables"), HTML = treeObjs$get("treeHTML"))
  {
    tb <- tables
    
    for (t in tb)
    { 
      treeObjs.setTable(t, HTML)
    }
    
    assign("tables", tb, envir = treeObjs)
    
  }
  
  #The idea is to create a new environment for each class object, done through list2env, which converts a list
  #(such as treeObjs) to an environmnet where the list items are objects within that environment
  
  treeObjs <- list2env(treeObjs)
  
  #Assign class 
  class(treeObjs) <- "Tree"
  
  #Call all constructor methods with default values. environment methods/objects accessed though $ operator
  treeObjs$setName()
  treeObjs$setTableNames()
  treeObjs$setRelevantTables()
  treeObjs$createTables()
  treeObjs$setTables()
  
  return(treeObjs)
}


#Function to set all values within a Table, including subTables
#
#Args:
#   masterTable: upper most parent Table
#   HTML: HTMLInternalDocument
#   rowIndex: which "row" to start on (specified by '\tr' in HTML), DEFAULT = 1
#   table: current Table object, DEFAULT = NULL
#   colIndex: Start of column values in HTML, DEFAULT = 0
#   colRange: vector of column indices relevant to table, DEFAULT = c()
#
#Returns:
#     NULL
#
#NOTE: A lot of the way that this function works is due to the form of the HTML. It may need to be modified as more cases are introduced.
#      If a better way of navigating and extracting the HTML values can be found then a lot of the complication here can be improved. For now though
#      this works, although in a very complicated way that will not be fully explained ;P. Trust Damien
treeObjs.setTable <- function(masterTable, HTML, rowIndex = 1, table = NULL, colIndex = 0, colRange = c())
{
  #if table =  DEFAULT, set to masterTable
  if (is.null(table))
  {
    table <- masterTable
  }
  
  #if colRange = DEFAULT, set to 'dataCols' of current table
  if (length(colRange) == 0)
  {
    colRange <- table$get('dataCols')
  }
  
  #get name of the masterTable as this is how the HTML is navigated to a specific table
  masterTableName <- masterTable$get('name')
  currentValuesIndex <- colIndex
  subcolIndex <- 0
  
  cNames <- c()
  valuesIndex <- c()
  startOfValues <- c()
  subTableIndices <- list()
  
  #HTML table unique in 'aria-label'
  for(i in rowIndex:length(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr", sep = ""))))
  {
    for (j in colRange)
    {
      #tryCatch to break loop if error occurs
      tryCatch(
        {
          #if a HTML row has a 'colspan' attribute it has been found that that signifies a subtable in the parent table. If found, various attributes need to be calculated such that
          #subsequent tables reference the correct values
          if(sum(names(xmlAttrs(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr[",i,"]/td[contains(@class, 'columnLabels')]", sep = ""))[[j]])) == 'colspan') > 0)
          {
            colSpan <- as.numeric(xmlAttrs(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr[",i,"]/td[contains(@class, 'columnLabels')]", sep = ""))[[j]])[['colspan']])
            startOfValues <- c(startOfValues, currentValuesIndex)
            currentValuesIndex <- currentValuesIndex + colSpan
            
            #Set hasSubTable bool to TRUE
            table$set('hasSubTable', TRUE)
            
            subcolIndex <- subcolIndex + 1
            
            #create subTable Table object
            newSubTable <- Table(xmlValue(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr[",i,"]/td[contains(@class, 'columnLabels')]", sep = ""))[[j]]))
            
            #Things happen here
            tableObjs.setSubTable(table, newSubTable, c(subcolIndex:(subcolIndex + colSpan - 1)))
            
          }
          else
          {
            currentValuesIndex <- currentValuesIndex + 1
            valuesIndex <- c(valuesIndex, currentValuesIndex)
            
            #get column name
            cNames <- c(cNames, xmlValue(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr[",i,"]/td[contains(@class, 'columnLabels')]", sep = ""))[[j]]))
          }
        }, error = function(err){return()})
      
    }
    
    #If a subtable has been found, recursively called treeObjs.setTable
    if(table$get('hasSubTable'))
    {
      s <- 1
      for(st in table$get('subTables'))
      {
        treeObjs.setTable(masterTable, HTML, rowIndex = i+1, table = st, colIndex = startOfValues[s], colRange = st$get('dataCols'))
        s <- s + 1
      }
      break
    }
  }
  
  #get row names
  ##MAY need to turn into loop to account for html being lame and not doing rows consistently
  rNames <- sapply(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr/td[contains(@class, 'rowLabels') and (contains(@class, 'vCR role3') or contains(@class, 'rowLabels dataAreaTop role3') or contains(@class, 'hCR role3'))]", sep = "")), xmlValue)
  
  #set colNames and rowNames
  table$set('colNames', cNames)
  table$set('rowNames', rNames)
  
  #Retrieve relevant data values
  ##Add stop condition if row name is found
  for(i in rowIndex:length(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr", sep = ""))))
  {
    j = 1
    tdValues <- c()
    rowFound <- FALSE
    
    while(length(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr[",i,"]/td[", j,"]", sep = ""))) > 0)
    {
      if(rowFound)
      {
        tdValues <- c(tdValues, xmlValue(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr[",i,"]/td[", j,"]", sep = ""))[[1]]))
      }
      if(sum(xmlValue(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr[",i,"]/td[", j,"]", sep = ""))[[1]]) == rNames) > 0)
      {
        rn <- xmlValue(getNodeSet(HTML, paste("//table[@aria-label = '", masterTableName, "']//tr[",i,"]/td[", j,"]", sep = ""))[[1]])
        rowFound <- TRUE
      }
      
      j = j + 1
    }
    if(!is.null(tdValues))
    {
      tableObjs.setValues(table, tdValues[valuesIndex], rn)
    }
    
  }
}


#Function to produce list of Tables from the parent Tree
#
#Args:
#   tree: Tree object
#   allTables: list of Tables, DEFAULT = c()
#
#Returns:
#     allTables: list of Tables
treeObjs.unravelTables <- function(tree, allTables = c())
{
  
  allTables <- allTables
  
  for(t in tree$get('tables'))
  {
    allTables <- tableObjs.unravelTables(t, allTables)
  }
  
  allTables
}


#Function to find specific Tree by name
#
#Args:
#   tree: Tree to search
#   phrase: keyword/phrase to search for
#
#Returns:
#     IF FOUND: Table
#     ELSE: "No match"
treeObjs.search4Table <- function(tree, phrase)
{
  tables <- treeObjs.unravelTables(tree)
  
  matchedTable <- c()
  
  for(t in tables)
  {
    if(grepl(phrase, t$get('name')))
    {
      matchedTable <- c(matchedTable, t)
    }
  }
  
  if(length(matchedTable) == 0)
  {
    print('No Match')
  }
  else
  {
    matchedTable
  }
}


#Function to produce list of dataframes from the parent Tree
#
#Args:
#   tree: Tree object
#
#Returns:
#     dfList: list of dataframes
treeObjs.unravelTablesToDataFrame <- function(tree)
{
  tableList <- treeObjs.unravelTables(tree)
  
  dfList <- list()
  
  for(i in tableList)
  {
    dfList[[i$get('name')]] <- tableObjs.tableToDataFrame(i)
  }
  
  dfList
}


###########################################################################################
###########################################################################################


#Function to emulate class/object functionality
#
#Args:
#   name: Table name, DEFAULT = ""
#   dataCols: Columns in master table (table listed in Tree object) that are relevant to this table
#
#Returns:
#     tableObjs: list
Table <- function(name = "", dataCols = c()) {
  tableObjs = list(name = name, 
                   rowNames = c(),
                   colNames = c(),
                   values = list(),
                   hasSubTable = FALSE,
                   subTables = c(),
                   dataCols = dataCols,
                   #Function to get specified value
                   #
                   #Args:
                   #     x: tableObjs object
                   #
                   #Returns:
                   #     tableObjs[[x]]
                   get = function(x) tableObjs[[x]],
                   
                   #Function to set specified value
                   #
                   #Args:
                   #     x: tableObjs object
                   #     value: value to set
                   #
                   #Returns:
                   #     NULL
                   set = function(x, value) tableObjs[[x]] <<- value)
  
  tableObjs <- list2env(tableObjs)
  class(tableObjs) <- "Table"
  return(tableObjs)
}

#Function to set Tableobjs list values by a key
#
#Args:
#   table: Table object
#   values: values to set
#   key: list key to label value
#
#Returns:
#     NULL
tableObjs.setValues <- function(table, values, key)
{
  table$values[[key]] <- values
}

#Function to append Tableobjs rowNames
#
#Args:
#   table: Table object
#   rowName:  row name to add to vector of rowNames
#
#Returns:
#     NULL
tableObjs.setRowNames <- function(table, rowName)
{
  table$rowNames <- c(table$rowNames, rowName)
}

#Function to append Tableobjs colNames
#
#Args:
#   table: Table object
#   colName:  column name to add to vector of colNames
#
#Returns:
#     NULL
tableObjs.setColNames <- function(table, colName)
{
  table$colNames <- c(table$colNames, colName)
}

#Function to convert Table to dataframe
#
#Args:
#   table: Table object
#
#Returns:
#     dFrame: dataframe
tableObjs.tableToDataFrame <- function(table)
{
  dFrame <- data.frame(matrix(unlist(table$get('values')), length(table$get('rowNames')), length(table$get('colNames')), byrow = TRUE), row.names = table$get('rowNames'), stringsAsFactors = F)
  
  colnames(dFrame) = table$get('colNames')
  
  dFrame
}

#Function to set a table as a subtable of another (the parent)
#
#Args:
#   table: Table object (parent)
#   subTable: Table object
#   subTableIndices: list of dataCols indices of subtable
#
#Returns:
#     NULL
tableObjs.setSubTable <- function(table, subTable, subTableIndices)
{
  
  subTable$set('dataCols',subTableIndices)
  
  table$subTables[[subTable$get('name')]] <- subTable
  
}

#Function to recursively unravel table and subtables into a list
#
#Args:
#   table: Table object (parent)
#   allTables: vector of Tables, DEFAULT = c()
#
#Returns:
#     allTables: vector of Tables
tableObjs.unravelTables <- function(table, allTables = c())
{
  allTables <- allTables
  
  if(table$get('hasSubTable'))
  {
    for(i in table$get('subTables'))
    {
      allTables <- tableObjs.unravelTables(i, allTables)
    }
  }
  
  allTables <- c(allTables, table)
  
  allTables
}

#Function to search for a table by a keyword/phrase
#
#Args:
#   table: Table object (parent)
#   phrase: keyword to search for
#
#Returns:
#     IF FOUND: matchedTable (List of Tables)
#     ELSE: "No match"
tableObjs.search4Table <- function(table, phrase)
{
  tables <- tableObjs.unravelTables(table)
  
  matchedTable <- c()
  
  for(t in tables)
  {
    if(grepl(phrase, t$get('name')))
    {
      matchedTable <- c(matchedTable, t)
    }
  }
  
  if(length(matchedTable) == 0)
  {
    print('No Match')
  }
  else
  {
    matchedTable
  }
}

#Function to print Table details
#
#Args:
#   table: Table object (parent)
#
#Returns:
#     NULL
tableObjs.print <- function(table)
{
  print(paste("Table name: ",table$get('name'), sep =""))  
  cat("Table column names: ")
  cat(table$get('colNames'), sep = ", ")
  cat("\nTable row names: ")
  cat(table$get('rowNames'), sep = ", ")
}

