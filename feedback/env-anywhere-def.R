#' which parent environments of <env> contain <name>?
#' inputs:
#'    name: a variable name
#'    env: valid inputs for pryr:::to_env, defaults to the calling frame
#' output: a list of environments containing <name>
anywhere <- function(name, env = parent.frame()) {
  checkmate::assert_string(name, min.chars = 1)
  #to_env takes care of checks for <env> and conversions to environments
  # (input checking & homogenization)
  env <- pryr:::to_env(env)

  result <- list()
  # base case: terminate recursion in emptyenv and return empty list
  if (identical(env, emptyenv())) {
    return(list())
  }
  # success: if <name> is in current <env>, <env> is added to results list ...
  if (exists(name, env, inherits = FALSE)) {
    result <- append(result, env)
  }
  # .... and then we go up one level to the next environment and add what we
  #      find there to the list
  # (recursion)
  append(result, anywhere(name, parent.env(env)))
}
