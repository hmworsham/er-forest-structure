### Exportable function
#'@export
#'

###################################################
# End-to-end processing function
# Cleans ring width time series 
# Splits by site, species, and dated v undated
# Converts to rwl dataframe
# Writes out to Tucson format
###################################################

`generate.rwl` <- function(sites, species, rw.set, data.index, outdir){
  
  # Split site/spp subsets by dated and undated cores
  split.series = lapply(rw.set, raw.to.rwl)
  
  # Get site and spp id pairs
  sitespp.ids = outer(rownames(rw.set), colnames(rw.set), paste)
  
  # Get beginning and end years for the site
  year.ranges = lapply(split.series, get.year.ranges)
  
  # Generate headers for series
  rwl.headers <- lapply(
    seq(length(split.series)), 
    hdr.wrapper, 
    siteindex, 
    sitespp.ids, 
    year.ranges)
  
  # Create write out rwl for dated and undated cores
  lapply(
    seq(length(split.series)), 
    write.tcsn, 
    split.series, 
    sitespp.ids, 
    rwl.headers, 
    outdir)
}

#########################################
# Rename sites in ITRDB format
##########################################

`fix.names` <- function(rwfile, sitenamekey){
  if(
    grep('[A-Z][A-Z]-[A-Z][A-Z][0-9]', rwfile) == 0 || 
    grep('[A-Z][A-Z]-[A-Z][A-Z][A-Z][0-9]', rwfile) == 0
    ) {
      breakup <- as.character(unlist(strsplit(rwfile, '/|_|-')))
      old.site <- paste(breakup[c(13,14)], collapse='-')
      new.site <- paste(breakup[c(13,14)], collapse='')
      #new.site.short <- breakup[14]
      new.site.short <- sitenamekey[sitenamekey$plotid==old.site, 'siteid']
      #new.series <- paste(breakup[c(15, 16)], collapse='')
      new.series <- breakup[15]
      
      return(c(
        'old.site'=old.site, 
        'new.site'=new.site, 
        'new.site.short'=new.site.short, 
        'new.series'=new.series))
  } else {
    print('Site name is already in ITRDB format.')
  }
    }

#######################################################################
# Restructure raw ringwidth time series to prepare for rwl conversion
#######################################################################

`restructure.rw` <- function(rwlist, data.index) {
  
  # Take only reference year and measurement observations
  rw = lapply(rwlist, '[', c('Ref_Year', 'Measurement'))
  
  # Stop if any ringwidth dataframes are missing columns
  if(isFALSE(all(sapply(rw, function(x) dim(x)[2]==2)))){
    err.arrays = which(sapply(rw, function(x) dim(x)[2]!=2))
    stop(paste('Array', err.arrays, 'is missing a necessary column.\n'))
  }
  
  # Select ringwidths and years for each series
  for(r in seq(length(rw))){
    nm = names(rw[r])
    colnames(rw[[r]]) = c('Ref_Year', nm) # Assign series name to meas column
    rw[[r]] <- rw[[r]][rowSums(is.na(rw[[r]])) != ncol(rw[[r]]), ] # Remove trailing NAs
    rw[[r]][is.na(rw[[r]])] <- 0 # Set any remaining in-series NAs to 0
    # If the first measurement is 0, remove it; else the first measurement stands
    if(rw[[r]][1,2]==0) rw[[r]] <- rw[[r]][2:nrow(rw[[r]]),] 
    else rw[[r]] <- rw[[r]]
    rownames(rw[[r]]) <- NULL
  }
  
  # Return cleaned time series dataframes
  return(rw)
}

#####################################################
# Find member series per site and species
#####################################################

`get.series.by.site.spp` <- function(site, spp, rw.set, data.index) {
  
  # Find the series of the target spp in the target site
  series = data.index[(data.index['ITRDB_SiteID']==site) & (data.index['Species']==spp), c('ITRDB_SiteID', 'Tree', 'Core')]
  series$Tree = sprintf('%04d', series$Tree)
  series = do.call(paste, c(series, sep=''))
  
  # Make a list of selected series
  site.spp.set = rw.set[names(rw.set) %in% series]
  
  return(site.spp.set)
}

#######################################################
# Split series by site and species using vectorization
#######################################################

`split.site.spp` <- Vectorize(function(site, spp) {
  sss = get.series.by.site.spp(site, spp, rw, dendroindex)
  return(sss)
})

########################################################################
# Convert raw ring width time series to dated and undated rwl per site
########################################################################

`raw.to.rwl` <- function(series) {
  
  # Get dated and undated series
  split.d.ud = function(series) {
    
    this <- unlist(sapply(series, function(x) x$Ref_Year[1]>1))
    series.dated = series[this]
    series.undated = series[!this]
    
    return(list(series.dated, series.undated))
    
  }
  
  datesplit = split.d.ud(series)
  series.dated = datesplit[[1]]
  series.undated = datesplit[[2]]
  
  sd.long = rbindlist(series.dated, use.names=F, idcol='series')
  names(sd.long) = c('Series', 'Ref_Year', 'Ring_Width')
  sud.long = rbindlist(series.undated, use.names=F, idcol='series')
  names(sud.long) = c('Series', 'Ref_Year', 'Ring_Width')
  
  # Create matrices of ring widths aligned by date
  # Get range of years from dated chronologies
  rw.dated.range = data.frame(
    'Ref_Year'=seq(min(sd.long$Ref_Year, na.rm=T), 
                   max(sd.long$Ref_Year, na.rm=T)))
  rw.undated.range = data.frame(
    'Ref_Year'=seq(min(sud.long$Ref_Year, na.rm=T), 
                   max(sud.long$Ref_Year, na.rm=T)))
  
  # Add the dated series' full year range as first column
  series.dated[[length(series.dated)+1]] <- rw.dated.range
  rw.dated <- series.dated[c(
    length(series.dated), 
    1:length(series.dated)-1)]
  
  # Add the undated series' full time point range as first column
  series.undated[[length(series.undated)+1]] <- rw.undated.range
  rw.undated <- series.undated[c(
    length(series.undated), 
    1:length(series.undated)-1)]
  
  # Make a matrix of ring widths by site along the full year range
  rw.d.mat <- rw.dated %>%
    reduce(left_join, by='Ref_Year')
  rownames(rw.d.mat) <- rw.d.mat$Ref_Year
  
  # Make a matrix of ring widths by site along the full range of relative time points
  rw.ud.mat <- rw.undated %>%
    reduce(left_join, by='Ref_Year')
  rownames(rw.ud.mat) <- rw.ud.mat$Ref_Year
  
  # Drop the year and time point columns
  rw.d.mat <- rw.d.mat[2:length(rw.d.mat)]
  rw.ud.mat <- rw.ud.mat[2:length(rw.ud.mat)]
  
  # Convert to rwl
  dated.rwl <- as.rwl(rw.d.mat)
  undated.rwl <- as.rwl(rw.ud.mat)
  
  # Return dated and undated series as list
  return(list('dated'=dated.rwl, 'undated'=undated.rwl))
}

#####################################################
# Assemble header for rwl output
#####################################################

`assemble.hdr` <- function(site.info, site, spp, srange){
  
  siteline = site.info[site.info$ITRDB_SiteID == site & site.info$Species == spp, ]
  
  hdr = list(
    site.id=siteline$ITRDB_SiteID,
    site.name=siteline$Site_Name,
    spp.code=siteline$Species,
    state.country=siteline$State_Country,
    spp=siteline$Spp,
    elev=siteline$Elevation,
    lat=siteline$Latitude,
    long=siteline$Longitude,
    meas.code=siteline$M_Code,
    first.yr = srange[1],
    last.yr = srange[2],
    lead.invs = siteline$Lead_Invs,
    comp.date=format(Sys.time(), "%Y%m%d")
  )
  
  return(hdr)
}

#####################################
# Wrapper to lapply `assemble.headers`
####################################
`hdr.wrapper` <- function(i, site.info, ids, years){
  site = unlist(strsplit(ids[i], ' '))[1]
  spp = unlist(strsplit(ids[1], ' '))[2]
  d.yrange = unlist(years[[i]][1:2])
  ud.yrange = unlist(years[[i]][3:4])
  d.hdr = assemble.hdr(site.info, site, spp, d.yrange)
  ud.hdr = assemble.hdr(site.info, site, spp, ud.yrange)
  return(list('dated'=d.hdr, 'undated'=ud.hdr))
}

#####################################################
# Get year ranges
#####################################################

`get.year.ranges` <- function(series.split){
  xd = series.split$dated
  xu = series.split$undated
  years = as.numeric(rownames(xd))
  rels = as.numeric(rownames(xu))
  minyear = min(years)
  maxyear = max(years)
  minrel = min(rels)
  maxrel = max(rels)
  
  return(list(
    minyear, 
    maxyear, 
    minrel, 
    maxrel)
  )
}


###########################################################
# Turn dataframe of measurments into tucson file format
###########################################################

`df.to.tucson` <- function(rwl.df, fname, header=NULL, append=FALSE, prec=0.001, long.names=TRUE,
                           ...) 
{
  
  # Define line terminator
  line.term <- "\x0D\x0A" # CR+LF, ASCII carriage return and line feed
  
  # Catch rwl input error
  if (!is.data.frame(rwl.df)) {
    stop("'rwl.df' must be a data.frame")
  }
  
  # Catch precision input error
  if (!is.numeric(prec) || length(prec) != 1 || is.na(prec) ||
      !(prec == 0.01 || prec == 0.001)) {
    stop("'prec' must equal 0.01 or 0.001")
  }
  
  # Catch header errors, 
  header2 <- header
  # If append target file does not exist 
  if (append) {
    if (!file.exists(fname)) {
      stop(gettextf("file %s does not exist, cannot append", fname))
    }
    # If trying to append with header ... better to correct header manually or overwrite
    if (length(header2) > 0) {
      stop("bad idea to append with 'header'")
    }
  }
  # If header isn't a list
  if (length(header2) > 0) {
    if (!is.list(header2)) {
      stop("'header' must be a list")
    }
    
    # Otherwise proceed with formatting header
    header.names <-
      c("site.id", "site.name", "spp.code", "state.country",
        "spp", "elev", "lat", "long", "meas.code", "first.yr", "last.yr",
        "lead.invs", "comp.date")
    if (!all(header.names %in% names(header2))) {
      stop("'header' must be a list with the following names: ",
           paste(dQuote(header.names), collapse = ", "))
    }
    
    ## Record #1: 1-6 Site ID, 10-61 Site Name, 62-65 Species
    ## Code, optional ID#s
    
    ## Record #2: 1-6 Site ID, 10-22 State/Country, 23-40 Species,
    ## 41-45 Elevation, 48-57 Lat-Long, 62-63 optional meas. code, 
    ## 68-76 1st & last Year
    ## Note: lat-lons are in degrees and minutes, ddmm or dddmm
    
    ## Record #3: 1-6 Site ID, 10-72 Lead Investigator, 73-80
    ## comp. date
    
    header2 <- lapply(header2, as.character)
    site.id <- header2$site.id[1]
    site.name <- header2$site.name[1]
    spp.code <- header2$spp.code[1]
    state.country <- header2$state.country[1]
    spp <- header2$spp[1]
    elev <- header2$elev[1]
    lat <- header2$lat[1]
    long <- header2$long[1]
    meas.code <- header2$meas.code[1]
    lead.invs <- header2$lead.invs[1]
    comp.date <- header2$comp.date[1]
    lat.long <- if (isTRUE(nchar(long) > 5)) {
      paste0(lat, long)
    } else {
      paste(lat, long, sep='')
    }
    
    yrs <- paste(header2$first.yr[1], header2$last.yr[1], sep=' ')
    
    
    field.name <-
      c('site.id', 'site.name', 'spp.code', 'state.country', 'spp',
        'elev', 'lat.long', 'meas.code', 'yrs', 'lead.invs', 'comp.date')
    field.width <- c(6, 52, 4, 13, 18, 5, 10, 2, 9, 63, 8)
    
    for (i in seq_along(field.name)) {
      this.name <- field.name[i]
      this.width <- field.width[i]
      this.var <- get(this.name)
      this.nchar <- nchar(this.var)
      if (this.nchar > this.width) {
        assign(this.name, substr(this.var, 1, this.width))
      } else if (this.nchar < this.width) {
        assign(this.name, encodeString(this.var, width = this.width))
      }
    }
    
    hdr1 <- paste0(site.id, ' 1 ', site.name, spp.code)
    hdr2 <- paste0(site.id, ' 2 ', state.country, spp, elev, strrep(' ', 2),
                   lat.long, strrep(' ', 4), meas.code, strrep(' ', 4), yrs)
    hdr3 <- paste0(site.id, ' 3 ', lead.invs, comp.date)
  }
  
  ## Loop through series and write each one
  nseries <- ncol(rwl.df)
  yrs.all <- as.numeric(row.names(rwl.df))
  col.names <- names(rwl.df)
  stopifnot(is.character(col.names), !is.na(col.names),
            Encoding(col.names) != "bytes")
  
  ## Sort years using increasing order, reorder rwl.df accordingly
  yrs.order <- sort.list(yrs.all)
  yrs.all <- yrs.all[yrs.order]
  rwl.df2 <- rwl.df[yrs.order, , drop=FALSE]
  
  first.year <- yrs.all[1]
  last.year <- yrs.all[length(yrs.all)]
  long.years <- FALSE
  if (first.year < -999) {
    long.years <- TRUE
    if (first.year < -9999) {
      stop("years earlier than -9999 (10000 BC) are not supported")
    }
  }
  if (last.year > 9999) {
    long.years <- TRUE
    if (last.year > 99999) {
      stop("years later than 99999 are not supported")
    }
  }
  
  ## The basic name.width is 7.
  name.width <- 7
  
  ## If we set exploit.short to TRUE:
  ## In the absence of long year numbers, it is possible to use a name
  ## that is one character longer.
  ## use.space adjusts the following:
  ## Do we use an extra space between the name and the decade
  ## (reduces maximum length of name by one)?
  
  ## Different interpretations exist...
  ## Setting long.names to FALSE will produce the same behavior
  ## as in old versions of the function.
  ## We offer the user only one bit of customization (i.e. two options),
  ## at this time anyway. Maybe the original idea of two customization
  ## options was too fine-grained.
  
  if (long.names) { # http://www.cybis.se/wiki/index.php?title=.rwl on 2010-04-21
    exploit.short <- TRUE  # limit is
    use.space <- FALSE     # 7 or 8 characters
  } else { # http://www.ncdc.noaa.gov/paleo/treeinfo.html on 2010-04-21
    exploit.short <- FALSE # limit is
    use.space <- TRUE      # 6 characters
  }
  if (exploit.short && !long.years) {
    name.width <- name.width + 1
  }
  if (use.space) {
    name.width <- name.width - 1
    opt.space <- " "
  } else {
    opt.space <- ""
  }
  name.width <- as.integer(name.width)
  year.width <-
    as.integer(12 - name.width - nchar(opt.space)) # year ends at col 12
  
  # If append is true, set writing mode to append ('a'), otherwise to write ('w')
  if (append) {
    rwl.out <- file(fname, "a")
  } else {
    rwl.out <- file(fname, "w")
  }
  on.exit(close(rwl.out))
  
  if (length(header2) > 0) {
    cat(hdr1, line.term, file=rwl.out, sep="")
    cat(hdr2, line.term, file=rwl.out, sep="")
    cat(hdr3, line.term, file=rwl.out, sep="")
  }
  
  if (prec == 0.01) {
    na.str <- 999
    missing.str <- -999
    prec.rproc <- 0.1 # precision scale factor
  } else {
    na.str <- -9999
    missing.str <- 0
    prec.rproc <- 1 # precision scale factor
  }
  format.year <- sprintf("%%%d.0f", year.width)
  
  for (l in seq_len(nseries)) {
    series <- rwl.df2[[l]]
    idx <- !is.na(series)
    yrs <- yrs.all[idx]
    series <- series[idx]
    
    series <- c(series, na.str)
    yrs <- c(yrs, max(yrs) + 1)
    
    decades.vec <- yrs %/% 10 * 10
    ## Output for completely missing decades can be disabled by using
    ## the alternate definition of the "decades" list
    decades <- seq(from=min(decades.vec), to=max(decades.vec), by=10)
    ##     decades = unique(decades.vec)
    n.decades <- length(decades)
    
    ## 1--name.width
    rwl.df.name <- col.names[l]
    ## Pad to name.width
    rwl.df.name <- str_pad(rwl.df.name, name.width, side = "right")
    
    for (i in seq_len(n.decades)) {
      ## up to 4 numbers and a minus sign from long series
      dec <- decades[i]
      dec.idx <- decades.vec %in% dec
      dec.yrs <- yrs[dec.idx]
      dec.rwl <- series[dec.idx]
      
      ## Find negative values and mark as missing data, but
      ## allow the negative "end of series" marker when prec == 0.001
      neg.match <- dec.rwl < 0
      if (prec == 0.001 && i == n.decades) {
        neg.match[length(neg.match)] <- FALSE
      }
      dec.rwl[neg.match] <- missing.str
      
      ## Find missing data.
      if (n.decades == 1) {
        all.years <- dec.yrs[1]:dec.yrs[length(dec.yrs)]
      } else if (i == 1) {
        all.years <- dec.yrs[1]:(dec + 9)
      } else if (i == n.decades) {
        all.years <- dec:dec.yrs[length(dec.yrs)]
      } else {
        all.years <- dec:(dec + 9)
      }
      
      ## Mark missing data.
      if (length(all.years) > length(dec.yrs)) {
        missing.years <- setdiff(all.years, dec.yrs)
        dec.yrs <- c(dec.yrs, missing.years)
        dec.rwl <- c(dec.rwl,
                     rep(missing.str, times=length(missing.years)))
        dec.order <- sort.list(dec.yrs)
        dec.yrs <- dec.yrs[dec.order]
        dec.rwl <- dec.rwl[dec.order]
      }
      
      ## Pad to year.width (no leading zero)
      dec.year1 <- sprintf(format.year, dec.yrs[1])
      
      ## Convert millimeters to the desired precision
      dec.rwl <- round(dec.rwl * prec.rproc)
      
      ## Find and correct illegal uses of the stop marker
      if (prec == 0.01) {
        end.match <- dec.rwl == 999
        if (i == n.decades) {
          end.match[length(end.match)] <- FALSE
        }
        dec.rwl[end.match] <-
          sample(c(998, 1000), sum(end.match), replace=TRUE)
      }
      
      ## Pad to nchar 6 (no leading zero)
      dec.rwl <- sprintf("%6.0f", dec.rwl)
      
      cat(rwl.df.name, opt.space, dec.year1, dec.rwl, line.term,
          file = rwl.out, sep="")
    }
  }
  fname
}

##########################################
# Write rwl data to Tucson
##########################################

`write.tcsn` <- function(i, rwls, ids, hdrs, outdir) {
  
  rwl.d = rwls[[i]][[1]]
  rwl.ud = rwls[[i]][[2]]
  
  outpath.d = file.path(outdir, paste(str_replace(ids[[i]], ' ', '_'), 'dated.rwl', sep='_'))
  outpath.ud = file.path(outdir, paste(str_replace(ids[[i]], ' ', '_'), 'undated.rwl', sep='_'))
  
  hdr.d = hdrs[[i]][[1]]
  hdr.ud = hdrs[[i]][[2]]
  
  df.to.tucson(rwl.d,
               outpath.d,
               hdr.d,
               append=F,
               prec=0.001,
               long.names=T)
  
  df.to.tucson(rwl.ud,
               outpath.ud,
               hdr.ud,
               append=F,
               prec=0.001,
               long.names=T)
}
