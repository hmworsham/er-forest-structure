### Exportable function
#'@export
#'

#####################################################
# Turn dataframe of measurments into tucson file format
#####################################################

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

#########################################
# Get site names from files
##########################################
`fix.names` <- function(rwfile, sitenamekey){
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
}

#####################################################
# Split data by site and species
#####################################################
`split.site.spp` <- function(site, spp, rwlist, site.key, data.index){
  
  outlist <- list()
  
  for(i in seq(length(rwlist))){
    siteref = site.key[site.key$siteid == site, 'plotid']
    #series = as.numeric(unlist(strsplit(colnames(rwlist[[i]])[2], '_|-'))[1])
    series <- strsplit(colnames(rw[[i]])[2], split="[A-Z]+")[[1]]
    series <- as.numeric(series[series>0])
    
    if(
      as.character(
        first(
          data.index[grepl(paste0('^', series, '$'), data.index$Tree), 'Species']
        )
      ) == spp 
      & as.character(
        first(
          data.index[grepl(paste0('^', series, '$'), data.index$Tree), 'Site']
        )
      ) == siteref) {
      #& as.character(siteref) == site) {
      outlist[i] = rwlist[i]
    }
  }
  
  return(outlist[lengths(outlist)>0])
}


#####################################################
# Make an rwl file
#####################################################
`make.rwl` <- function(series){
  
  # Get dated and undated series
  split.d.ud = function(series) {
    
    splitset = list()
    
    for(i in seq(length(series))){
      if(series[[i]]$Ref_Year[1]>1) splitset[i] = i
      #else if(series[[i]]$Ref_Year[1]==1) splitset[i] = i
    }
    
    dated.idx = which(lengths(splitset)>0)
    undated.idx = which(lengths(splitset)==0)
    series.dated = series[dated.idx]
    series.undated = series[undated.idx]
    
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
  
  # Apply range ...?
  series.dated[[length(series.dated)+1]] <- rw.dated.range
  rw.dated <- series.dated[c(
    length(series.dated), 
    1:length(series.dated)-1)]
  
  series.undated[[length(series.undated)+1]] <- rw.undated.range
  rw.undated <- series.undated[c(
    length(series.undated), 
    1:length(series.undated)-1)]
  
  rw.d.mat <- rw.dated %>%
    reduce(left_join, by='Ref_Year')
  rownames(rw.d.mat) <- rw.d.mat$Ref_Year
  
  rw.ud.mat <- rw.undated %>%
    reduce(left_join, by='Ref_Year')
  rownames(rw.ud.mat) <- rw.ud.mat$Ref_Year
  
  rw.d.mat <- rw.d.mat[2:length(rw.d.mat)]
  rw.ud.mat <- rw.ud.mat[2:length(rw.ud.mat)]
  
  dated.rwl <- as.rwl(rw.d.mat)
  undated.rwl <- as.rwl(rw.ud.mat)
  
  rwl.out <- list('dated'=dated.rwl, 'undated'=undated.rwl)
  
  return(rwl.out)
}


#####################################################
# Wrapper to use `outer` on split.site.spp
#####################################################

sss.wrapper <- Vectorize(function(site, spp){
  return(split.site.spp(site, spp, rw, namekey, dendroindex))
})


#####################################################
# Assemble header for rwl output
#####################################################

`assemble.hdr` <- function(site.info, site, spp, srange){
  siteline <- site.info[site.info$ITRDB_SiteID == site & site.info$Species == spp, ]
  
  hdr <- list(
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

