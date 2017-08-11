#' Constructor
EmailClass <- function(name, email) {
  nc = list(
    name = name,
    email = email,
    get = function(x) nc[[x]],
    set = function(x, value) nc[[x]] <<- value,
    props = list(),
    history = list(),
    getHistory = function() return(nc$history),
    getNumMessagesSent = function() return(length(nc$history))
  )
  #Add a few more methods
  nc$sendMail = function(to) {
    cat(paste("Sending mail to", to, 'from', nc$email))
    h <- nc$history
    print("hello")
    #h[[(length(h)+1)]] <- list(to=to, timestamp=Sys.time())
    #assign('history', h, envir=nc)
  }
  nc$addProp = function(name, value) {
    p <- nc$props
    p[[name]] <- value
    assign('props', p, envir=nc)
  }
  nc <- list2env(nc)
  class(nc) <- "EmailClass"
  return(nc)
}

#' Define S3 generic method for the print function.
print.EmailClass <- function(x) {
  if(class(x) != "EmailClass") stop();
  cat(paste(x$get("name"), "'s email address is ", x$get("email"), sep=''))
}

###########################################################################################
###########################################################################################

