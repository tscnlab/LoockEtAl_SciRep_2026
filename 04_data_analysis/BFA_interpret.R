BFA_interpret <- function(BFA = BFA){
  
  A <- interpret_bf(
    as.vector(BFA),
    rules = "jeffreys1961",
    log = FALSE,
    include_value = FALSE,
    protect_ratio = TRUE,
    exact = TRUE
  )
  return (A)
}