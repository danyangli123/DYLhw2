#' Sqrt.2
#'
#' @param x numeric value which you want to take square root of
#'
#' @return sqrt(x) or error with negative input
#' @export
#'
#' @examples sqrt.2(2) = sqrt(2)
sqrt.2 = function(x){

  if(x<0){
    rlang::abort(message = "negative input, NA introduced!",
                 .subclass ="invalid_input",
                 invalid_input = x)
  }else{
    return(sqrt(x))
  }
}

#' log.2
#'
#' @param x numeric value which you want to take log of
#'
#' @return log(x) or error with negative input
#' @export
#'
#' @examples log.2(2) = log(2)
log.2 = function(x){
  if(x<0){
    rlang::abort(message = "negative input, NA introduced!",
                 .subclass ="invalid_input",
                 invalid_input = x)
  }else{
    return(log(x))
  }
}

#' Function operator for sqrt.2 and log.2
#'
#' @param f sqrt.2/log.2
#'
#' @return sqrt(x)/log(x) or error condition object,
#' with "invalid_input" subclass and invalid value attached
#' @export
#'
#' @examples f_operator(sqrt.2)(2) = sqrt(2)
f_operator = function(f){
  force(f)
  function(x){
    if(x > 0){
      return(f(x))
    }else{
      cnd <- catch_cnd(f(x))
      return(cnd)
    }
  }
}
