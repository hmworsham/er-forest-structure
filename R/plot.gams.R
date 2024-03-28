## Functions for generating GAM partial-effects plots

#' Slice model frame
#' @description Slice a model frame for prediction of a
#' variable's partial effects over its observed interval
#' @export pe.slice
#'

pe.slice <- function(v, mod) {
  if(!v=='geology') {
    data <- model.frame(mod)
    data <- data[-1]
    meds <- apply(data, 2, \(x) median(as.numeric(x), na.rm=T))
    r.max <- max(data[v], na.rm=T)
    r.min <- min(data[v], na.rm=T)
    geol.mode <- modal(data['geology'], na.rm=T)
    df <- data.frame(pseq=seq(-4.99,5,0.01))
    df <- cbind(df, t(data.frame(meds)))
    df$geology <- geol.mode
    df[(df$pseq<r.min | df$pseq>r.max),] <- NA
    df <- df[!names(df)==v]
    names(df)[1] <- v
    pred.v <- predict(mod, df, type='response', se.fit=T)
    slice.v <- data.frame(sup=seq(-4.99,5,0.01),
                          fit=pred.v$fit,
                          u95=pred.v$fit+pred.v$se.fit,
                          l95=pred.v$fit-pred.v$se.fit)
    names(slice.v) <- c('v', paste0(v,'.fit'), paste0(v, '.u95'), paste0(v, '.l95'))
    slice.v
  }
}

#' Pivot sliced model frame
#' @description Pivot a partial-effects dataframe longer for comparison of
#' multiple partial effects
#' @export pe.pivot
#'

pe.pivot <- function(sdf, plot.vars) {

  fl <- sdf %>%
    pivot_longer(cols=contains('fit'),
                 names_to='var',
                 values_to='fit') %>%
    mutate(var=str_replace_all(var, '.fit', '')) %>%
    dplyr::select(c(v,var,fit)) %>%
    left_join(varnames, by=c('var'='varnames'))

  ul <- sdf %>%
    pivot_longer(cols=contains('u95'),
                 names_to='var',
                 values_to='u95') %>%
    mutate(var=str_replace_all(var, '.u95', '')) %>%
    dplyr::select(c(v,var, u95))

  ll <- sdf %>%
    pivot_longer(cols=contains('l95'),
                 names_to='var',
                 values_to='l95') %>%
    mutate(var=str_replace_all(var, '.l95', '')) %>%
    dplyr::select(c(v,var, l95))

  slices.df.l <- reduce(list(fl,ul,ll), left_join, by=c('v', 'var'))
  slices.df.l <- slices.df.l %>% filter(var %in% plot.vars)

  slices.df.l
}


#' Slice model frame for interactions
#' @description Slice a model frame for prediction of an
#' interaction's partial effects over the interacting variables'
#' observed intervals
#' @export pe.slice.itx
#'
pe.slice.itx <- function(v, mod) {
  if(!'geology' %in% v) {
    data <- model.frame(mod)
    data <- data[-1]
    meds <- apply(data, 2, \(x) median(as.numeric(x), na.rm=T))
    r.max <- max(data[v], na.rm=T)
    r.min <- min(data[v], na.rm=T)
    geol.mode <- modal(data['geology'], na.rm=T)
    df <- expand.grid(pseq1=seq(-4.9,5,0.1),
                      pseq2=seq(-4.9,5,0.1))
    df <- cbind(df, t(data.frame(meds)))
    df$geology <- geol.mode
    df[(df$pseq1<r.min | df$pseq1>r.max),] <- NA
    df[!is.na(df$pseq2) & (df$pseq2<r.min | df$pseq2>r.max),] <- NA
    df <- df[!names(df) %in% v]
    names(df)[1:2] <- v
    pred.v <- predict(mod, df, type='response', se.fit=T)
    slice.v <- data.frame(df[1:2],
                          fit=pred.v$fit,
                          u95=pred.v$fit+pred.v$se.fit,
                          l95=pred.v$fit-pred.v$se.fit)
    names(slice.v) <- c('v1', 'v2',
                        paste0(paste0(v, collapse='-'), '.fit'),
                        paste0(paste0(v, collapse='-'), '.u95'),
                        paste0(paste0(v, collapse='-'), '.l95'))
    slice.v
  }
}

#' Pivot sliced interaction model frame
#' @description Pivot an interaction partial-effects dataframe longer for comparison of
#' multiple partial effects
#' @export pe.pivot.itx
#'

pe.pivot.itx <- function(sdf, plot.vars) {

  fl <- sdf %>%
    pivot_longer(cols=contains('fit'),
                 names_to='var',
                 values_to='fit') %>%
    mutate(var=str_replace_all(var, '.fit', '')) %>%
    dplyr::select(c(v1,v2,var,fit))

  ul <- sdf %>%
    pivot_longer(cols=contains('u95'),
                 names_to='var',
                 values_to='u95') %>%
    mutate(var=str_replace_all(var, '.u95', '')) %>%
    dplyr::select(c(v1,v2,var, u95))

  ll <- sdf %>%
    pivot_longer(cols=contains('l95'),
                 names_to='var',
                 values_to='l95') %>%
    mutate(var=str_replace_all(var, '.l95', '')) %>%
    dplyr::select(c(v1,v2,var,l95))

  slices.df.l <- reduce(list(fl,ul,ll), left_join, by=c('v1', 'v2', 'var'))
  #slices.df.l <- slices.df.l %>% filter(var %in% plot.vars)

  slices.df.l
}
