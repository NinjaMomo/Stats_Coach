#Function to delete a file 
#
#Args:
#   address: String filepath
#
#Returns:
#   NULL
deleteFile <- function(address)
{
  file.remove(address)
}