# Bipartite matching

#' Bipartite matching
#' @description Run optimal bipartite matching on field and modeled trees by minimizing Euclidean distances. Return performance statistics and data for plotting.
#' @param runid list. Names of sampling areas cross-referenced in lasset and obset
#' @param lasset list. List of point cloud datasets in LAS format to apply bipartite matching to. Typically the output of an optimized ITC segementation or delineation algorithm, such as Li2012.opt.
#' @param obset dataframe. Dataset containing at minimum X,Y,Z coordinates for field-identified trees with an additional column labeling trees by an ID matching one of those in runid.
#' @return dataframe. Performance and accuracy statistics for each sample area, parameter permutation, and algorithm.
#' @export bipart.match2
#'

# Apply over quadrants, and their corresponding obs set, then the lists of candidate lassets
# Consider where argument 'quad' can come from, and if this can/should be integrated into one function ...
bipart.match3 <- function(runid, lasset, obset, plotdir=F) {

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
  df.paired <- bind_rows(treat, ctrl, .id='src')
  rownames(df.paired) <- NULL
  df.paired['src'][df.paired['src']=='1'] <- 1
  df.paired['src'][df.paired['src']=='2'] <- 0
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
  names(treat.d.xy)[5:length(treat.d.xy)] <- ctrl$treeID

  dmatz <- dist.euc.z@.Data
  treat.d.z <- data.frame(cbind(treat, dmatz))
  treat.d.z <- treat.d.z[order(treat.d.z$Z, decreasing=T),]
  names(treat.d.z)[5:length(treat.d.z)] <- ctrl$treeID

  dmatxyz <- dist.euc.xyz@.Data
  treat.d.xyz <- data.frame(cbind(treat, dmatxyz))
  treat.d.xyz <- treat.d.xyz[order(treat.d.xyz$Z, decreasing=T),]
  names(treat.d.xyz)[5:length(treat.d.xyz)] <- ctrl$treeID

  match.run <- function(xy.df, z.df, xyz.df) {

  # Duplicate treatment dataframes to tmp for processing
  dxy.tmp <- xy.df
  dz.tmp <- z.df
  dxyz.tmp <- xyz.df

  # Transpose dataframe for testing against surrounding predicted trees
  tdxy.tmp <- data.frame(t(dxy.tmp)[5:ncol(dxy.tmp),])
  tdz.tmp <- data.frame(t(dz.tmp)[5:ncol(dz.tmp),])
  tdxyz.tmp <- data.frame(t(dxyz.tmp)[5:ncol(dxyz.tmp),])

  # return(list(treat.d.xy, treat.d.z, treat.d.xyz))

  # Set counters
  testmatch <- integer(0)
  pairid <- 0
  nobs <- ncol(dxy.tmp)

  # Find matches
  for(i in seq(nrow(xy.df))) {
    # print('----------')
    # print(paste('i:', i))
    # Pull height for modeled tree i
    z <- dz.tmp[i,'Z']
    #print(paste('z:', z))
    # Given Z, define maximum ∆Z threshold
    dz.min <- case_when(z<=10 ~ 3,
                        10 < z & z <= 15 ~ 4,
                        15 < z & z <=25 ~ 4,
                        z > 25 ~ 4)
    # print(paste('dz.min:', dz.min))
    # Given Z, define maximum XY search radius threshold
    dxy.min <- case_when(z<=10 ~ 4,
                         10 < z & z <= 15 ~ 4,
                         15 < z & z <=25 ~ 5,
                         z > 25 ~ 5)
    # print(paste('dxy.min:', dxy.min))

    # Find candidates
    xy.cands <- 4+which(dxy.tmp[i, 5:nobs] <= dxy.min)
    z.cands <- 4+which(dz.tmp[i,5:nobs] <= dz.min)
    cands <- intersect(xy.cands, z.cands)
    # print(paste('xy.cands:', 'xy.cands'))
    # print(paste('z.cands:', 'z.cands'))
    # print(paste('cands:', cands))
    # print(paste('n_cands:', length(cands)))
    # If there are candidates, proceed with voting
    # Else, no match (assign NA to match)

    if(length(cands)) {

      # Set anything > XYSRT or > ∆ZT to NA
      dxy.tmp[i, -c(1:4, cands)] <- NA
      dz.tmp[i, -c(1:4, cands)] <- NA

      # Vote 1: dxy and dz values for nearest tree in {x,y}
      i1 <- which.min(dxy.tmp[i, 5:nobs]) # index of nearest candidate in {x,y}
      u1 <- min(dxy.tmp[i, 5:nobs], na.rm=T) # dxy to nearest tree in {x,y}
      v1 <- dz.tmp[i, 4+i1] # dz to nearest tree in {x,y}
      z1 <- which.min(dz.tmp[i, 5:nobs]) # index of nearest candidate in {z}
      # print(c(i1, u1, v1, z1))

      # Get dz and dxy values for all other candidates in ascending dz order
      dxy.cands <- suppressWarnings(dxy.tmp[i, cands][order(dz.tmp[i, cands])])
      dz.cands <- suppressWarnings(dz.tmp[i, cands][order(dz.tmp[i, cands])])
      # print(dxy.cands)
      # print(dz.cands)

      # If dz for the nearest {x,y} is the minimum dz, then assign match
      # Else proceed to vote 2
      if (z1==i1) {
        match <- names(dz.tmp)[4+i1]
        # print(c('vote1match:', match))
      } else {
        # Vote 2:
        # If there is a smaller dz for a more distant tree AND the dxy for that pair
        # is < 2.5 + dxy for the nearest tree in {x,y} then assign that match
        # Else assign the match from vote 1
        for(j in seq_along(dz.cands)) {

          if(dz.cands[j] <= v1 & dxy.tmp[i, names(dz.cands[j])] <= u1+2.5) {
            n2 <- names(dz.cands)[j]
            i2 <- which(n2==names(dxy.tmp)[5:nobs])
            v2 <- dz.cands[j]
            u2 <- dxy.tmp[i, i2]
            match <- names(dxy.tmp)[4+i2]
            break
          } else {
            v2 <- v1
            i2 <- i1
            u2 <- u1
            match <- names(dxy.tmp)[4+i2]
          }

          print(paste('vote2match:', as.character(match)))
        }
      }

      # Test observed match against surrounding predicted trees
      # tdxyz.min <- which.min(tdxyz.tmp[match, ])
      # print(paste('checkpredmatch:', as.character(tdxyz.min)))
      # testmatch <- integer(0)
      # if(tdxyz.min==i) {
      #   tdxyz.tmp[, i] <- NA
      # } else {
      #   #testmatch <- tdxyz.min
      #   match <- integer(0)
      # }

    } else {
      match <- integer(0)
      dxy.tmp[i, -c(1:4)] <- NA
      dz.tmp[i, -c(1:4)] <- NA
    }

    print(c('finalmatch:', match))

    # Generate match outputs
    dxy <- dxy.tmp[i, which(names(dxy.tmp)==match)]
    dz <- dz.tmp[i, which(names(dz.tmp)==match)]
    dxyz <- dxyz.tmp[i, which(names(dxyz.tmp)==match)]
    pairid <- ifelse(identical(match, integer(0)), pairid, pairid+1)
    dxy.tmp[i, 'pred'] <- dxy.tmp[i, 'treeID']
    dxy.tmp[i, 'obs'] <- as.integer(ifelse(identical(match, integer(0)), NA, match))
    dxy.tmp[i, 'pair_id'] <- ifelse(identical(match, integer(0)), NA, pairid)
    dxy.tmp[i, 'dxy'] <- ifelse(identical(match, integer(0)), NA, dxy)
    dxy.tmp[i, 'dz'] <- ifelse(identical(match, integer(0)), NA, dz)
    dxy.tmp[i, 'dxyz'] <- ifelse(identical(match, integer(0)), NA, dxyz)
    #dxy.tmp[i, 'testmatch'] <- ifelse(identical(testmatch, integer(0)), NA, testmatch)
    dxy.tmp[ , which(names(dxy.tmp)==match)] <- NA
    dz.tmp[ , which(names(dxy.tmp)==match)] <- NA
    dxyz.tmp[, which(names(dxy.tmp)==match)] <- NA
  }

  return(dxy.tmp)

  }

  # Pass 1
  match.1 <- match.run(treat.d.xy, treat.d.z, treat.d.xyz)

  # Update inputs to filter out modeled trees matched in Pass 1
  treat.d.xy.2 <- treat.d.xy
  treat.d.xy.2[,names(treat.d.xy.2) %in% match.1$obs] <- NA
  treat.d.xy.2 <- treat.d.xy.2[which(is.na(match.1$pair_id)),]
  treat.d.z.2 <- treat.d.z
  treat.d.z.2[,names(treat.d.z.2) %in% match.1$obs] <- NA
  treat.d.z.2 <- treat.d.z.2[which(is.na(match.1$pair_id)),]
  treat.d.xyz.2 <- treat.d.xyz
  treat.d.xyz.2[,names(treat.d.xyz.2) %in% match.1$obs] <- NA
  treat.d.xyz.2 <- treat.d.xyz.2[which(is.na(match.1$pair_id)),]

  # Pass 2
  match.2 <- match.run(treat.d.xy.2, treat.d.z.2, treat.d.xyz.2)
  match.2$pair_id <- match.2$pair_id + max(match.1$pair_id, na.rm=T)

  # Join matches from pass 1 and pass 2
  match.1[match(match.2$treeID, match.1$treeID),] <- match.2

  # Update inputs to filter out modeled trees matched in Pass 2
  treat.d.xy.3 <- treat.d.xy
  treat.d.xy.3[,names(treat.d.xy.3) %in% match.1$obs] <- NA
  treat.d.xy.3 <- treat.d.xy.3[which(is.na(match.1$pair_id)),]
  treat.d.z.3 <- treat.d.z
  treat.d.z.3[,names(treat.d.z.3) %in% match.1$obs] <- NA
  treat.d.z.3 <- treat.d.z.3[which(is.na(match.1$pair_id)),]
  treat.d.xyz.3 <- treat.d.xyz
  treat.d.xyz.3[,names(treat.d.xyz.3) %in% match.1$obs] <- NA
  treat.d.xyz.3 <- treat.d.xyz.3[which(is.na(match.1$pair_id)),]

  # Pass 3
  match.3 <- match.run(treat.d.xy.3, treat.d.z.3, treat.d.xyz.3)
  match.3$pair_id <- match.3$pair_id + max(match.1$pair_id, na.rm=T)

  # Join matches from pass 1 and pass 2
  match.1[match(match.3$treeID, match.1$treeID),] <- match.3

  # return(match.1)

  # Bind match ID to original dataframe
  df.matched <- left_join(df.paired[df.paired$src==0,], match.1, by=c('treeID'='obs')) %>%
    dplyr::select(pair_id,
                  src,
                  treeID,
                  pred,
                  #treeIDobs=treeID.x,
                  Zobs=Z.x,
                  Xobs=X.x,
                  Yobs=Y.x,
                  #treeIDpred=treeID.y,
                  Zpred=Z.y,
                  Xpred=X.y,
                  Ypred=Y.y,
                  dxy,
                  dz,
                  dxyz
    )

  # return(df.matched)

  # Calculate performance statistics
  nobs <- nrow(ctrl) # number of observed tree crowns in quad
  npred <- nrow(treat) # number of delineated tree crowns in quad
  tp <- length(unique(df.matched$pair_id[!is.na(df.matched$pair_id)])) # true positive = n matches
  ext.rt <- npred/nobs
  match.rt <- tp/nobs
  #obs.unmatched <- length(df.matched$pair_id[is.na(df.matched$pair_id)]) # unmatched observed
  #pred.unmatched <- nrow(df.matched[df.matched$src==1 & is.na(df.matched$pair_id),]) # unmatched predicted
  fn <- nobs - tp
  fp <- npred - tp
  ft <- fn + fp # false total = false negatives + false positives
  acc.rt <- tp/nobs
  omm.rt <- fn / nobs # obs fail rate
  com.rt <- fp / npred # pred fail rate
  xy.dists <- df.matched$dxy
  z.dists <- df.matched$dz
  xyz.dists <- df.matched$dxyz
  xy.rmse <- sqrt(sum(xy.dists^2, na.rm=T) / tp)
  z.rmse <- sqrt(sum(z.dists^2, na.rm=T) / tp)
  xyz.rmse <- sqrt(sum(xyz.dists^2, na.rm=T) / tp)
  #loss = rmse/(tp/(fn+fp))
  precision = tp/(tp+fp)
  recall = tp/(tp+fn)
  f = 2 * (recall*precision) / (recall+precision)
  loss = xyz.rmse/f

  # Performance results
  performance <- data.frame('nobstrees'=nobs,
                            'npredtrees'=npred,
                            'ext.rt'=ext.rt,
                            'tp'=tp,
                            'match.rt'=match.rt,
                            #'obs.unm'=obs.unmatched,
                            #'pred.unm'=pred.unmatched,
                            'fn'=fn,
                            'fp'=fp,
                            'ft'=ft,
                            'accuracy'=acc.rt,
                            'omission'=omm.rt,
                            'commission'=com.rt,
                            'xy.rmse'=xy.rmse,
                            'z.rmse'=z.rmse,
                            'xyz.rmse'=xyz.rmse,
                            'precision'=precision,
                            'recall'=recall,
                            'f'=f,
                            'loss'=loss
  )

  if(!plotdir==F) {

    # Bind match ID to original dataframe
    df.matched.plt <- left_join(df.paired[df.paired$src==0,], match.1, by=c('treeID'='obs')) %>%
      dplyr::select(pair_id,
             treeID,
             treeID.y,
             pred,
             Z.x,
             X.x,
             Y.x,
             Z.y,
             X.y,
             Y.y,
             dxy,
             dz,
             dxyz) %>%
      pivot_longer(cols=c(treeID, treeID.y),
                   names_to='src',
                   values_to='treeID') %>%
      arrange(pair_id) %>%
      mutate(src = case_when(src=='treeID.y' ~ 'Modeled',
                             T ~ 'Observed')) %>%
      mutate(across(Z.x:Y.x, ~ ifelse(src=='Modeled', NA, .)),
             across(Z.y:Y.y, ~ ifelse(src=='Observed', NA, .)),
             Z = coalesce(Z.x, Z.y),
             X = coalesce(X.x, X.y),
             Y = coalesce(Y.x, Y.y)) %>%
      dplyr::select(pair_id, src, treeID, pred, Z, X, Y, dxy, dz, dxyz)

    # For every model, generate X,Y,Z density plots and export
    nclr <- nrow(df.matched.plt)/2

    # Make directory if not exists
    dir.create(plotdir)

    # Plot matches in xyz space
    match.map <- ggplot(df.matched.plt,
                          aes(x=X,
                              y=Y,
                              size=Z,
                              shape=factor(src),
                              color=factor(pair_id),
                              label=factor(pair_id))) +
      geom_point() +
      geom_text() +
      scale_color_manual(values=rainbow(nclr, s=.75)[sample(1:nclr, nclr)]) +
      scale_shape_manual(values=c(2,3)) +
      guides(color='none', size='none')

    ggsave(file.path(plotdir,
                     paste(runid, 'matchmap.png', sep='_')),
           match.map,
           #dpi=72,
           device='png')

    # Plot density function of observed vs modeled
    df.matched.plt.l <- df.matched.plt %>%
      pivot_longer(cols=c(X,Y,Z),
                   names_to='dim')

    skill.density <- ggplot(df.matched.plt.l, aes(x=value, group=src, color=factor(src))) +
      geom_density() +
      facet_wrap(~dim, nrow=3, scales='free')

    ggsave(file.path(plotdir,
                     paste(runid, 'skill_density.png', sep='_')),
           skill.density,
           #dpi=72,
           device='png')
  }

  return(performance)
}
