
options("nfilter.tab" = 1)
options("nfilter.subset" = 1)
options("nfilter.glm" = 1)
options("nfilter.string" = 1000)
options("nfilter.stringShort" = 1000)
options("nfilter.kNN" = 1)
options("nfilter.levels" = 1)
options("nfilter.noise" = 0)
options("datashield.privacyLevel" = 1)

datashield.login <- function (logins = NULL, ... , locals = list(how_many = 0, opal_name = '.connection_object', tie_first_to_GlobalEnv = FALSE)){

  first <- list()
  second <- list()
  default_locals <- list(how_many = 0, opal_name = '.connection_object', tie_first_to_GlobalEnv = FALSE)
  defs <- setdiff(names(default_locals), names(locals))
  locals[defs] <- default_locals[defs]
  if (locals$how_many > 0){

    .set.new.datashield.methods(locals$opal_name)
    # mimic a real opal object, for each required pseudo-connection,
    # give it the class 'local' too for later dispatch of assign and aggregate methods
     # I also  need to create a separate environment for each connection to avoid name collisions
    for (i in 1:locals$how_many){
        l <- paste0('local',i)
        first[[l]] <- new.env()
        class(first[[l]]) <- c('local')
        first[[l]]$name <- l
        if(locals$tie_first_to_GlobalEnv && l == 1){ # optionally the first envir is globalenv
          first[[l]]$envir <- .GlobalEnv
        } else {
          first[[l]]$envir <- new.env(parent = parent.frame())
        }
        
        #first[[l]]$version <- '2.7.6-b20170311061231'
      }

  }
  #once here we can fall back on the default functionality:
  if(!is.null(logins)){
      arglist<- as.list(match.call())[-1]
      arglist$locals <- NULL
      second <- do.call(opal::datashield.login, arglist)
  }
  conns <- list(locals = first, remotes = second)
  out<-c(names(first), names(second))
  names(out) <- out

  attr(out, 'connection_object') <- locals$opal_name


  # give them some self awareness
 # Map(function(x){
#    expr <- paste0('c("',x$name,'")')
#    opal::datashield.assign(x, '.whoami', as.symbol(expr))
#    return(x)
#  }, out)
  # add a few goodies in the server session:
  # - store the name of the server as seen by the client in .whoami
  # - a bit violent: redefine the data.frame() function to make factors and to replace dates with factors as well
  sapply(second, function(x){
    expr <- paste0('c("',x$name,'")')
    opal::datashield.assign(x, '.whoami', as.symbol(expr), async = TRUE, wait = FALSE) # poll later
    opal::datashield.aggregate(x, quote(set.stringsAsFactors(TRUE)), async = TRUE, wait = FALSE) # poll later
  })
  # now poll:
  dsCDISCclient::ds2.wait.for.asyncs(second, 1)
  assign(locals$opal_name, conns, envir = parent.frame())
  out
}


.set.new.datashield.methods <- function(conn_obj){

# define 2 replacement methods for datashield.assign and datashield.aggregate, then export them in the global env

 assn <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE){
   # the parameters after value are not used
   
   #my.env <- .GlobalEnv
  # if('envir' %in% names(opal)){
     my.env <- opal$envir
   #}

   if(!is.call(value)){
      value <- parse(text = as.character(value))
    }

    assign(symbol, eval(value, envir = my.env), envir = my.env)
    
}

 agg <- function(opal, expr, async=TRUE, wait=TRUE){
  # async and wait are there just for show
   #my.env <- .GlobalEnv
   #if('envir' %in% names(opal)){
     my.env <- opal$envir
   #}
   if(!is.call(expr)){
       expr <- parse(text = as.character(expr))
    }


    ret <- eval(expr, envir = my.env)
    #out <- list()
    #out[[opal$name]] <- ret
    #out
    ret
 }

 sym <- function(opal){
  #   my.env <- .GlobalEnv
   #if('envir' %in% names(opal)){
     my.env <- opal$envir
   #}
   ret <- unlist(lapply(ls(envir = my.env), function(x) if(class(eval(parse(text=x), envir = my.env)) != 'function') x))
   if(is.null(ret)){
     ret <-character(0)
   }
   ret
 }

 sym.char <- function(opals_vector){
   real_opals <- get(conn_obj)
   # deal with local connections
   ret <- Map(function(x){
     datashield.symbols(x)
   },
   Reduce(c,real_opals)[opals_vector]
   )
   unlist(ret, recursive = FALSE)
   ret
 }

 # we also have to redefine the datashield..list methods.

 assn.list=function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE) {
   # deal with the local connections first:

   Map(function(x){
     if(grepl('local', x )){

       datashield.assign(opal[[x]], symbol, value)
       opal[[x]] <<- NULL
     }
   }, names(opal))
 #  if(!is.null(opal$local)){
#     datashield.assign(opal$local, symbol, value)
#     opal$local <- NULL
#   }

   #if anything left, fall back to the default functionality:
   if(length(opal) >0 ){
    opal:::datashield.assign.list(opal, symbol, value, variables=variables, missings=missings, identifiers=identifiers, async=async, wait=wait)
   }
 }

 assn.char=function(opals_vector, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE) {

   real_opals <- get(conn_obj)
   Map(function(x){
       
       datashield.assign(x, symbol, value, variables, missings, identifiers , async, wait)


   }, Reduce(c,real_opals)[opals_vector])
   invisible()

 }

 agg.list=function(opal, expr, async=TRUE, wait=TRUE) {
   ret <- list()

   # deal with local connections
   Map(function(x){

     if(grepl('local', x )){
       ret <<- c(ret,datashield.aggregate(opal[[x]], expr))
       opal[[x]] <<- NULL
     }
   }, names(opal))
#   if(!is.null(opal$local)){
#     ret <- datashield.aggregate(opal$local, expr)
#     opal$local <- NULL
#   }

   #fall back to the default functionality:

   if(length(opal) >0 ){
     ret <- c(ret, opal:::datashield.aggregate.list(opal, expr, async=async, wait=wait))
   }
   ret
 }
 agg.char=function(opals_vector, expr, async=TRUE, wait=TRUE) {

   real_opals <- get(conn_obj)
   # deal with local connections
  ret <- Map(function(x){
              datashield.aggregate(x, expr)
            },
            Reduce(c,real_opals)[opals_vector]
        )
  #unlist(ret, recursive = FALSE)
  ret

 }
 
 logout <- function(opals_vector){
   real_opals <- get(conn_obj)
   opal::datashield.logout(real_opals$remotes)
 }


 assign('datashield.assign.local', assn, envir = .GlobalEnv)
 assign('datashield.aggregate.local', agg, envir = .GlobalEnv)
 assign('datashield.assign.character', assn.char, envir = .GlobalEnv)
 assign('datashield.aggregate.character', agg.char, envir = .GlobalEnv)
 assign('datashield.assign.opal', opal:::datashield.assign.opal, envir = .GlobalEnv)
 assign('datashield.aggregate.opal', opal:::datashield.aggregate.opal, envir = .GlobalEnv)
 #assign('datashield.aggregate.list', agg.list, envir = .GlobalEnv)
 assign('datashield.symbols.local', sym, envir = .GlobalEnv)
 assign('datashield.symbols.character', sym.char, envir = .GlobalEnv)
 assign('datashield.symbols.opal', opal:::datashield.symbols.opal, envir = .GlobalEnv)
 assign('datashield.logout', logout, envir = .GlobalEnv)
 assign('print.local', function (x, ...) 
 {
   cat("url: local","\n")
   cat("name:", x$name, "\n")
   cat("username:", x$username, "\n")
   if (!is.null(x$restore)) {
     cat("restore:", x$restore, "\n")
   }
 }, envir = .GlobalEnv)

 #assignInNamespace('datashield.assign.opal', assn, ns = asNamespace('opal'))
 #assignInNamespace('datashield.aggregate.opal', agg, ns = 'opal')
 #assignInNamespace('datashield.assign.list', assn.list, ns = 'opal')
 #assignInNamespace('datashield.aggregate.list', agg.list, ns = 'opal')
 #assignInNamespace('datashield.symbols.opal', sym, ns = 'opal')
 #assignInNamespace('datashield.symbols.list', sym.list, ns = 'opal')


}

