# Functions to assist with inferential model building for GAM and GBM

#' make.modframe
#' @description Make a standard model frame for inferential modeling
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

