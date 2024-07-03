# Bipartite matching

#' Bipartite matching
#' @description Run optimal bipartite matching on field and modeled trees by minimizing Euclidean distances. Return performance statistics and data for plotting.
#' @param runid list. Names of sampling areas cross-referenced in lasset and obset
#' @param lasset list. List of point cloud datasets in LAS format to apply bipartite matching to. Typically the output of an optimized ITC segementation or delineation algorithm, such as Li2012.opt.
#' @param obset dataframe. Dataset containing at minimum X,Y,Z coordinates for field-identified trees with an additional column labeling trees by an ID matching one of those in runid.
#' @return dataframe. Performance and accuracy statistics for each sample area, parameter permutation, and algorithm.
#' @export bipart.match
#'

# Apply over quadrants, and their corresponding obs set, then the lists of candidate lassets
# Consider where argument 'quad' can come from, and if this can/should be integrated into one function ...
bipart.match <- function(runid, lasset, obset) {

  # Pull quadrant ID from runid
  plt <- unlist(str_split(runid, '_'))[1]

  # Subset the observed trees to the quadrant of focus
  obs <- obset[obset$PLOT_ID==plt,]

  mod <- lasset[names(lasset)==runid][[1]]

  # Define 'control' dataframe from observed trees in quad
  ctrl <- data.frame(treeID = obs$Tag_Number,
                     Z=obs$Z,
                     st_coordinates(obs))

  # Define 'treatment' dataframe from modeled trees in quad
  treat <- data.frame(treeID=mod$treeID,
                      Z=mod$Z,
                      st_coordinates(mod))

  # Bind control and treatment dfs into paired and clean
  df.paired <- bind_rows(ctrl, treat, .id='src')
  rownames(df.paired) <- NULL
  df.paired['src'][df.paired['src']=='1'] <- 0
  df.paired['src'][df.paired['src']=='2'] <- 1
  df.paired$src <- as.integer(df.paired$src)

  # Standardize distances
  scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

  df.paired <- df.paired %>%
    mutate(Z.std = scale2(Z, na.rm=T),
           X.std = scale2(X, na.rm=T),
           Y.std = scale2(Y, na.rm=T))

  # Compute distances
  # euc.caliper <- match_on(src~X.std+Y.std+Z.std,
  #                         data=df.paired,
  #                         method='euclidean',
  #                         caliper=2)

  dist.euc.std <- match_on(src~X.std+Y.std+Z.std,
                           data=df.paired,
                           method='euclidean')
  dist.euc.raw <- match_on(src~X+Y+Z,
                           data=df.paired,
                           method='euclidean')

  # Bind distances to treatment dataframe
  dmat <- dist.euc.raw@.Data
  y.dmat <- data.frame(cbind(treat, dmat))
  y.dmat2 <- dmat

  # Pass 1: find each predicted tree's best match
  nobs <- ncol(y.dmat)
  for(i in seq(nrow(y.dmat))) {
    z <- y.dmat[i,'Z']
    dmin <- case_when(z<=10 ~ 3,
                      10 < z & z <= 15 ~ 4,
                      15 < z & z <=25 ~ 5,
                      z > 25 ~ 5)
    y.dmat[i, 4+which(y.dmat[i, 5:nobs] > dmin)] <- NA
    match <- ifelse(sum(y.dmat[i,5:nobs], na.rm=T)>0, which.min(y.dmat[i,5:nobs]), NA)
    d <- ifelse(!is.na(match), y.dmat[i, 4+match], NA)
    if(!is.na(match)) y.dmat[,4+match] <- NA
    y.dmat[i, 'pred'] <- i
    y.dmat[i, 'obs'] <- match
    y.dmat[i, 'pair_id'] <- i
    y.dmat[i, 'dist'] <- d
  }

  # Pass 2: assert that the match found in pass 1 is the optimal match for each observed tree
  for(i in y.dmat$obs) {
    print(paste('Obs', i))
    match2 <- which.min(y.dmat2[,i])
    print(paste('Pred', match2))
  }

  return(y.dmat)

  # # Execute bipartite pair matching on Euclidean distances
  # match.euc <- pairmatch(dist.euc.std, data=df.paired)

  # Bind match ID to original dataframe
  #df.matched <- cbind(df.paired, matches=match.euc)
  df.matched <- left_join(df.paired, by=c('treeID', 'obs'))
  return(df.matched)
  # Calculate match distances
  df.matchdist <- matched.distances(match.euc, dist.euc.raw, preserve.unit.names=F)
  #df.matchdist[df.matchdist > 9] <- NA
  df.matchdist <- data.frame(df.matchdist)
  df.matchdist$matches <- row.names(df.matchdist)

  # Append match distances to original dataframe
  df.matched$matches <- as.character(df.matched$matches)
  df.matched <- left_join(df.matched, df.matchdist, by='matches')

  # Calculate performance statistics
  nobs <- length(df.matched$src[df.matched$src==0]) # number of observed tree crowns in quad
  npred <- length(df.matched$src[df.matched$src==1]) # number of delineated tree crowns in quad
  tp <- length(unique(df.matched$df.matchdist[!is.na(df.matched$df.matchdist)])) # true positive = n matches
  obs.unmatched <- df.matched[df.matched$src==0 & is.na(df.matched$df.matchdist),] # unmatched observed
  mod.unmatched <- df.matched[df.matched$src==1 & is.na(df.matched$df.matchdist),] # unmatched modeled
  fn <- nrow(obs.unmatched) # false negatives = n unmatched observed
  fp <- nrow(mod.unmatched) # false positives = n unmatched modeled
  ft <- fn + fp # false total = false negatives + false positives
  obs.fn.rt <- fn / nrow(ctrl) # obs fail rate
  mod.fn.rt <- fp / nrow(treat) # mod fail rate
  dists <- df.matched$df.matchdist
  rmse <- sqrt(sum(dists^2, na.rm=T)/tp)
  loss = rmse/(tp/(fn+fp))
  precision = tp/(tp+fp)
  recall = tp/(tp+fn)
  f = 2 * (recall*precision) / (recall+precision)

  # Performance results
  performance <- data.frame('nobstrees'=nobs,
                            'npredtrees'=npred,
                            'tp'=tp,
                            'fn'=fn,
                            'fp'=fp,
                            'ft'=ft,
                            'obs.fn.rt'=obs.fn.rt,
                            'mod.fn.rt'=mod.fn.rt,
                            'rmse'=rmse,
                            'loss'=loss,
                            'precision'=precision,
                            'recall'=recall,
                            'f'=f
  )

  # For best performing model in set, generate X,Y,Z density plots and export
  # df.matched.l <- df.matched %>%
  #   pivot_longer(cols=c(X,Y,Z),
  #                names_to='dim') %>%
  #   mutate(src = factor(case_when(src == 0 ~ 'Observed',
  #                                 src == 1 ~ 'Modeled')))
  #
  # ggplot(df.matched.l, aes(x=value, group=src, color=factor(src))) +
  #   geom_density() +
  #   facet_wrap(~dim, nrow=3, scales='free')
  #
  # # TODO implement plot export
  #
  # For best performing model in set, generate pair maps and export
  # ggplot(df.matched,
  #        aes(x=X,
  #            y=Y,
  #            size=Z,
  #            shape=factor(src),
  #            color=factor(matches),
  #            label=factor(matches))) +
  # geom_point() +
  # geom_text() +
  # scale_color_manual(values=rainbow(53, s=.75)[sample(1:53, 53)]) +
  # scale_shape_manual(values=c(2,3))

  # TODO implement plot export

  return(performance)
}
