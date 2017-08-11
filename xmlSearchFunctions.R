library(XML)
x = htmlTreeParse('C:\\Users\\Damien\\Documents\\Work\\StatsOutput\\HTMLAttempt\\outputt.htm',useInternal = TRUE)
xx <- xmlRoot(x)
for (i in 1:xmlSize(xx[['body']]))
{
  if (length(as.character(xmlAttrs(xx[["body"]][[i]])[[1]]) == "itemBody") != 0)
  {
    if (as.character(xmlAttrs(xx[['body']][[i]])[[1]]) == 'itemBody')
    {
      print(i)
    }
  }
}

##get number of "tr" in this table
length(getNodeSet(x, "//table[@aria-label = 'Pre-campaign spend category']//tr"))

##get attribute list
xmlAttrs(getNodeSet(x, "//table[@aria-label = 'Pre-campaign spend category']//tr")[[1]][[1]])

##get attribute names
names(xmlAttrs(getNodeSet(x, "//table[@aria-label = 'Pre-campaign spend category']//tr")[[1]][[1]]))

##navigate to a specifific td in a specific tr
getNodeSet(x, paste("//table[@aria-label = '", a, "']//tr[1]/td", sep = ""))[[1]]

sum(xmlSApply(getNodeSet(x, "//table[@aria-label = 'Pre-campaign spend category']//tr")[[2]], xmlName) == "td")
getNodeSet(x, "//table[@aria-label = 'Pre-campaign spend category']//tr")