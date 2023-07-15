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
bipart.match2 <- function(runid, lasset, obset) {

  # Pull quadrant ID from runid
  plt <- unlist(str_split(runid, '_'))[1]

  # Subset the observed trees to the quadrant of focus
  obs <- obset[obset$PLOT_ID==plt,]

  mod <- lasset[names(lasset)==runid][[1]]

  # Define 'control' dataframe from observed trees in quad
  ctrl <- data.frame(treeID = obs$Tag_Number,
                     Z=obs$Z,
                     st_coordinates(obs))
  ctrl <- ctrl[order(ctrl$Z, decreasing = T),]

  # Define 'treatment' dataframe from modeled trees in quad
  treat <- data.frame(treeID=mod$treeID,
                      Z=mod$Z,
                      st_coordinates(mod))
  treat <- treat[order(treat$Z, decreasing = T),]

  # Bind control and treatment dfs into paired and clean
  df.paired <- bind_rows(ctrl, treat, .id='src')
  rownames(df.paired) <- NULL
  df.paired['src'][df.paired['src']=='1'] <- 0
  df.paired['src'][df.paired['src']=='2'] <- 1
  df.paired$src <- as.integer(df.paired$src)

  # Standardize distances
  # scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  #
  # df.paired <- df.paired %>%
  #   mutate(Z.std = scale2(Z, na.rm=T),
  #          X.std = scale2(X, na.rm=T),
  #          Y.std = scale2(Y, na.rm=T))

  # Compute distances
  # dist.euc.std <- match_on(src~X.std+Y.std+Z.std,
  #                          data=df.paired,
  #                          method='euclidean')
  dist.euc.z <- match_on(src~Z,
                           data=df.paired,
                           method='euclidean')
  dist.euc.xy <- match_on(src~X+Y,
                          data=df.paired,
                          method='euclidean')
  dist.euc.xyz <- match_on(src~X+Y+Z,
                           data=df.paired,
                           method='euclidean')

  # Bind distances to treatment dataframe
  dmat <- dist.euc.xy@.Data
  treat.d.xy <- data.frame(cbind(treat, dmat))
  treat.d.xy <- treat.d.xy[order(treat.d.xy$Z, decreasing=T),]

  dmatz <- dist.euc.z@.Data
  treat.d.z <- data.frame(cbind(treat, dmatz))
  treat.d.z <- treat.d.z[order(treat.d.z$Z, decreasing=T),]

  dmatxyz <- dist.euc.xyz@.Data
  treat.d.xyz <- data.frame(cbind(treat, dmatxyz))
  treat.d.xyz <- treat.d.xyz[order(treat.d.xyz$Z, decreasing=T),]

  dxy.tmp <- treat.d.xy
  dz.tmp <- treat.d.z

  # Find matches
  nobs <- ncol(treat.d.xy)
  for(i in seq(nrow(treat.d.z))) {
    z <- treat.d.z[i,'Z']
    dzmin <- case_when(z<=10 ~ 3,
                      10 < z & z <= 15 ~ 3,
                      15 < z & z <=25 ~ 4,
                      z > 25 ~ 4)
    treat.d.z[i, 4+which(treat.d.z[i, 5:nobs] > dzmin)] <- NA
    dz.tmp[i,] <- treat.d.z[i,] #NEW
    dz.tmp[i, 4+which(!is.na(treat.d.z[i, 5:nobs]))] <- 1
    #treat.d.z[i, 4+which(!is.na(treat.d.z[i, 5:nobs]))] <- 1
    treat.d.xy[i,5:nobs] <- dz.tmp[i,5:nobs] * treat.d.xy[i,5:nobs]
    dxymin <- case_when(z<=10 ~ 3,
                       10 < z & z <= 15 ~ 4,
                       15 < z & z <=25 ~ 5,
                       z > 25 ~ 5)
    treat.d.xy[i, 4+which(treat.d.xy[i, 5:nobs] > dxymin)] <- NA

    # Match
    match <- which.min(treat.d.xy[i, 5:nobs])

    # Vote

    # Generate match outputs
    dxy <- treat.d.xy[i, 4+match]
    dz <- treat.d.z[i, 4+match]
    dxyz <- treat.d.xyz[i, 4+match]
    treat.d.xy[i, 'pred'] <- i
    treat.d.xy[i, 'obs'] <- ifelse(identical(match, integer(0)), NA, match)
    treat.d.xy[i, 'pair_id'] <- ifelse(identical(match, integer(0)), NA, i)
    treat.d.xy[i, 'dxy'] <- ifelse(identical(match, integer(0)), NA, dxy)
    treat.d.xy[i, 'dz'] <- ifelse(identical(match, integer(0)), NA, dz)
    treat.d.xy[i, 'dxyz'] <- ifelse(identical(match, integer(0)), NA, dxyz)
    treat.d.xy[ , 4 + match] <- NA
  }

  # # Execute bipartite pair matching on Euclidean distances
  # match.euc <- pairmatch(dist.euc.std, data=df.paired)

  # Bind match ID to original dataframe
  # df.matched <- cbind(df.paired, matches=match.euc)
  df.paired <- rownames_to_column(df.paired, 'obs')
  df.paired$obs <- as.integer(df.paired$obs)
  df.matched <- left_join(df.paired, treat.d.xy, by='obs')
  df.matched <- df.matched %>%
    select(pair_id,
           src,
           obs,
           pred,
           treeIDobs=treeID.x,
           Zobs=Z.x,
           Xobs=X.x,
           Yobs=Y.x,
           treeIDpred=treeID.y,
           Zpred=Z.y,
           Xpred=X.y,
           Ypred=Y.y,
           dxy,
           dz,
           dxyz
           )

  # Calculate performance statistics
  nobs <- length(df.matched$src[df.matched$src==0]) # number of observed tree crowns in quad
  npred <- length(df.matched$src[df.matched$src==1]) # number of delineated tree crowns in quad
  tp <- length(unique(df.matched$pair_id[!is.na(df.matched$pair_id)])) # true positive = n matches
  obs.unmatched <- df.matched[df.matched$src==0 & is.na(df.matched$pair_id),] # unmatched observed
  mod.unmatched <- df.matched[df.matched$src==1 & is.na(df.matched$pair_id),] # unmatched modeled
  fn <- nrow(obs.unmatched) # false negatives = n unmatched observed
  fp <- nrow(mod.unmatched) # false positives = n unmatched modeled
  ft <- fn + fp # false total = false negatives + false positives
  obs.fn.rt <- fn / nrow(ctrl) # obs fail rate
  mod.fn.rt <- fp / nrow(treat) # mod fail rate
  dists <- df.matched$dxyz
  rmse <- sqrt(sum(dists^2, na.rm=T)/tp)
  #loss = rmse/(tp/(fn+fp))
  precision = tp/(tp+fp)
  recall = tp/(tp+fn)
  f = 2 * (recall*precision) / (recall+precision)
  loss = rmse/f

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
                            'precision'=precision,
                            'recall'=recall,
                            'f'=f,
                            'loss'=loss
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
