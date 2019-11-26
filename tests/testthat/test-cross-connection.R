datashield.aggregate.character <- function(opals_vector, expr, async=TRUE, wait=TRUE) {
  real_opals <- attr(opals_vector, 'connection_object')
  Map(function(x){
    datashield.aggregate(x, expr)
  },
  Reduce(c,real_opals)[opals_vector]
  )
}