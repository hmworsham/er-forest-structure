# Functions to assist with inferential model building for GAM and GBM

#' make.modframe
#' @description Make a standard model frame for inferential modeling
#' @param y character. Column name of the target response variable
#' @param df dataframe. Dataframe of variable values
#' @param modtype character. Target model specification that model frame will be fed into. 'gam' for Generalized Additive Model (mcgv::gam) or 'gbm' for Generalized Boosted Model (caret::gbm)
#' @param target.vars list. Character list of column names of target explanatory variables
#' @param itx concatenated list. Character list of paired variables to be specified as pairwise interactions in GAM. List elements must be of the form 'X,Y'. Defaults to c('none)
#' @returns list. Two elements are returned in a list. The first element is a text string containing the model formula to be used in the model. The second element is a dataframe of n observations on m variables, where the names of the m variables correspond to those in the formula, the first variable is the response, and the remaining m-1 variables are explanatory variables.
#' @export make.modframe
#'

make.modframe <- function(y, df, modtype=c('gam', 'gbm'), target.vars, itx=c('none')) {

  # Build up the initial dataframe
  re <- df[y] # Isolate the response
  x <- df[names(vars) %in% target.vars] # Isolate features
  fv <- names(df[sapply(df, is.factor)]) # Isolate factor features
  tv <- target.vars[!target.vars %in% fv &
                      !target.vars %in% c('x', 'y')]  # Remove factors and coords
  mm <- cbind(re,x)

  # Handle GAM model frames
  if(modtype=='gam') {
    if(all(itx=='all')) {
      itx = apply(apply(combn(names(vars),2), 1, paste), 1, \(i) paste(i, collapse=', '))
    } else if(all(itx=='none')) {
      itx = NULL
    } else {
      itx = itx
    }

    ff <- paste(y, '~',
                paste(c(
                  paste0('s(', tv,
                         ', bs="tp")'),
                  fv),
                  collapse=' + ')
    )

    if(!is.null(itx)) {
      ff <- paste(c(ff,
                    paste0('ti(', itx,
                           ', bs="tp")')),
                  collapse=' + ')
    }

    # Handle GBM model frames
  } else {

    ff <- paste0(c(paste0(y, ' ~ ')),
                paste(c(tv, fv),
                collapse=' + '))

  }

  ff <- as.formula(ff)

  return(list('formula'=ff, 'data'=mm))

}

