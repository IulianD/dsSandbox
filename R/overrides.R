
options("nfilter.tab" = 1)
options("nfilter.subset" = 1)
options("nfilter.glm" = 1)
options("nfilter.string" = 1000)
options("nfilter.stringShort" = 1000)
options("nfilter.kNN" = 1)
options("nfilter.levels" = 1)
options("nfilter.noise" = 0)
options("datashield.privacyLevel" = 1)


dssCreateFakeServers <- function(servers = 1, opal_name = '.connection_object', tie_first_to_GlobalEnv = FALSE){
  first <- list()
  if(is.numeric(servers) && length(servers) ==1){
    for (i in 1:servers){
      l <- paste0('local',i)
      first[[l]] <- new.env()
      class(first[[l]]) <- c('local')
      first[[l]]$name <- l
      if(tie_first_to_GlobalEnv && i == 1){ # optionally the first envir is globalenv
        first[[l]]$envir <- .GlobalEnv
      } else {
        first[[l]]$envir <- new.env()
      }

    }
  } else {
    first <- Map( function(x){
      ret<- new.env()
      class(ret) <- c('local')
      ret$name <- x
      ret$envir <- new.env()
      ret
    }, servers)
    if(tie_first_to_GlobalEnv){
      first[[1]]$envir <- .GlobalEnv
    }
    
  }
  .set.new.login.function(first, opal_name)
  names(first)
}

.set.new.login.function <- function(local_conns, opal_name){
  
  mylogin <- function(which_connections = names(local_conns), cross_connect = FALSE,...){
    reals <- NULL
    if (length(list(...)) > 0){
      reals <- opal::datashield.login(...)
    }
    if(!is.null(which_connections)){
      wh <- intersect(which_connections, names(local_conns))
     if(length(wh) == 0 && is.null(reals)){
        stop('No connections provided.')
      }
      local_conns <- local_conns[wh]
    }
    if(!cross_connect){
      final_conn_obj <- list(locals = local_conns, remotes = reals)
      assign(opal_name, final_conn_obj, envir = .GlobalEnv)
      .set.new.datashield.methods(opal_name)
      out <- Reduce(c, lapply(final_conn_obj,names))
    }  else {
      out <- names(local_conns)
    }
    names(out) <- out
    attr(out, 'connection_object') <- opal_name
    out
  }
  assign('datashield.login', mylogin, envir = .GlobalEnv)
  
}


.set.new.datashield.methods <- function(opal_name){
  


# define methods for 
# datashield.assign.character and datashield.aggregate.character and datsahield.symbols.character, then export them in the global env
# same for datashield.<...>.local  

 assn <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE){
   # the parameters after value are not used
  my.env <- opal$envir

   if(!is.call(value)){
      value <- parse(text = as.character(value))
    }

    assign(symbol, eval(value, envir = my.env), envir = my.env)
    
}

 agg <- function(opal, expr, async=TRUE, wait=TRUE){
  # async and wait are there just for show
  my.env <- opal$envir

   if(!is.call(expr)){
       expr <- parse(text = as.character(expr))
    }

    eval(expr, envir = my.env)

 }

 sym <- function(opal){
   my.env <- opal$envir
   ret <- unlist(lapply(ls(envir = my.env), function(x) if(class(eval(parse(text=x), envir = my.env)) != 'function') x))
   if(is.null(ret)){
     ret <-character(0)
   }
   ret
 }

 sym.char <- function(opals_vector){
   conn_obj <- get(opal_name, envir = .GlobalEnv)
    Map(function(x){
       datashield.symbols(x)
      },
        Reduce(c,conn_obj)[opals_vector]
   )
}

 assn.char=function(opals_vector, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE) {

   conn_obj <- get(opal_name, envir = .GlobalEnv)
   Map(function(x){
     datashield.assign(x, symbol, value, variables, missings, identifiers , async, wait)
    }, Reduce(c,conn_obj)[opals_vector])
   invisible()
}

  agg.char=function(opals_vector, expr, async=TRUE, wait=TRUE) {
    conn_obj <- get(opal_name, envir = .GlobalEnv)

    Map(function(x){
              datashield.aggregate(x, expr)
            },
            Reduce(c,conn_obj)[opals_vector]
        )
 }
 
 logout <- function(opals_vector){
   conn_obj <- get(opal_name, envir = .GlobalEnv)
   
   rem <- real_opals$remotes
   if(length(rem[opals_vector]) > 0){
    opal::datashield.logout(rem[opals_vector])
   }
   rm(ls(pattern ='datashield.*.local|datashield.*.opal|datashield.*.character', envir = .GlobalEnv), envir = .GlobalEnv)
   
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


}

  


