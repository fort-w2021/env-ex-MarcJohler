# a)
# lists differ from environments by the following properties:
# - objects in an environment MUST have a name
# - environments have parents (except the Empty Environment)
# - The order of elements is only relevant for lists
# - Environments have reference semantics
# e.g. an environment could contain an object pointing at the environment itself

# b)
# Global Environment

# c)
# in the current environment,
# meaning e.g. in the Global Environment for executing in the console (at least by default)
# or e.g. in the function call environment if a function with a "<-" is called (also by default)

# d)

# recursive version of where to find all the environments which contain an object with a certain name
where_recursion <- function(object_name, start_env) {
  # check if the name can be found in any environment, starting from start_env
  found_in <- try(pryr::where(object_name, start_env), silent = TRUE)
  # if so go to the enclosure environment of that environment to continue recursively
  if (class(found_in) == "environment") {
    return(append(
      list(found_in),
      where_recursion(
        object_name,
        rlang::env_parent(found_in)
      )
    ))
  }
  # if not, return an empty list
  return(list())
}

# user interface for generating a list of environments which contain a certain object
anywhere <- function(object_name, env = NULL) {
  # check if object_name is a single character
  checkmate::assert_character(object_name,
    min.chars = 1,
    len = 1,
    any.missing = FALSE
  )

  # check if env is an environment
  checkmate::assert_environment(env, null.ok = TRUE)

  # if env is not defined start at the call environment
  if (is.null(env)) {
    env <- rlang::caller_env()
  }

  # call the where_recursion function in order to
  # find all environments where the object is contained
  where_recursion(object_name, env)
}
