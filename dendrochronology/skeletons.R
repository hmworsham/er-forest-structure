library(readxl)


kt.xl <- '~/Desktop/ER-APL1_Skeleton_Data_KT_22-03-10.xlsx'
qa.xl <- '~/Desktop/ER-APL1_Skeleton_Data_QCA_22-03-10.xlsx'
kt.tabs <- excel_sheets(kt.xl)
qa.tabs <- excel_sheets(qa.xl)

kt <- lapply(kt.tabs, function(x) read_excel(path = kt.xl, sheet = x))
qa <- lapply(qa.tabs, function(x) read_excel(path = qa.xl, sheet = x))

skelprep <- function(filepath, species, series){
  
  tabs = excel_sheets(filepath)
  skeldata = lapply(tabs, function(x) read_excel(path = filepath, sheet = x))
  
  targets = unlist(lapply('ABLA', paste, series, sep='_'))
  tabnums = which(tabs %in% targets)

  raw = skeldata[tabnums]

  for(r in seq(length(raw))){
    raw[[r]]$Ref_Year <- rev(seq(0, nrow(raw[[r]])-1))
  }
  
  raw.widths <- lapply(raw, '[', c('Ref_Year', 'Width_Score'))
  
  for(r in seq(length(raw))){
    nm = targets[r]
    colnames(raw.widths[[r]]) <- c('Ref_Year', nm)
  }
  
  raw.long = rbindlist(raw.widths, fill=T, idcol='tab')
  raw.range = data.frame('Ref_Year'=rev(seq(0, max(raw.long$Ref_Year, na.rm=T))))
  
  raw.widths[[length(raw.widths)+1]] = raw.range
  
  rw <- raw.widths[c(length(raw.widths), 1:length(raw.widths)-1)]
  
  rw.df = rw %>%
    reduce(left_join, by='Ref_Year')
  
  rw.df[rw.df=='B'] = NA
  
  rw.df <- as.data.frame(sapply(rw.df, as.numeric))
  
  rw.means <- rowMeans(rw.df[,-1], na.rm=T)
  
  return(list(rw.df, rw.means))
}
kt.abla
kt.ablas=c('01-01', '03-01', '05-01', '07-01', '11-01', '15-01', '16-01')
kt.abla <- skelprep(kt.xl, 'ABLA', kt.ablas)
qa.ablas <- c('01-01', '02-01', '03-01', '05-01', '07-01', '10-01', '11-01', '15-01', '16-01')
qa.abla <- skelprep(qa.xl, 'ABLA', qa.ablas)

skplots <- function(series, outdir, seriesname){
  
  # Plot master
  png(file.path(outdir, paste0(seriesname,'_master.png')), 800, 200, units='px')
  skel.plot(series[[2]], sname='Master', master=T)
  dev.off()
  
  # Plot others
  for(i in seq(2,length(series[[1]]))){
    png(file.path(outdir, paste0(seriesname,'_', i,'.png')), 800, 200, units='px')
    par(mar = rep(0, 4), xaxs='i', yaxs='i')
    skel.plot(series[[1]][i], sname=unlist(strsplit(colnames(series[[1]][i]),'_'))[2])
    dev.off()
  }
}

skplots(kt.abla, '~/Desktop/skeletons', 'kt_abla')
skplots(qa.abla, '~/Desktop/skeletons', 'qa_abla')

png('~/Desktop/skeletons/kt_abla_master.png', 800, 200, units='px')
skel.plot(kt.abla[[2]], sname='Master', master=T)
dev.off()

png('~/Desktop/skeletons/qa_abla_master.png', 800, 200, units='px')
skel.plot(qa.abla[[2]], sname='Master', master=T)
dev.off()

kt <- kt[c(2:19)]
qa <- qa[c(2:19)]

kt.tabs <- kt.tabs[c(2:19)]

for(k in seq(length(kt))){
  kt[[k]]$Ref_Year <- rev(seq(0, nrow(kt[[k]])-1))
  #kt[[k]]$Width_Score <- kt[[k]]$Width_Score
}

kt <- lapply(kt, '[', c('Ref_Year', 'Width_Score'))

for(r in seq(length(kt))){
  nm <- kt.tabs[r]
  colnames(kt[[r]]) <- c('Ref_Year', nm)
}

kt.long <- rbindlist(kt, fill=T, idcol='tab')

kt.range <- data.frame('Ref_Year'=rev(seq(0, max(kt.long$Ref_Year, na.rm=T))))

kt[[19]] <- kt.range
kt <- kt[c(19, 1:18)]

kt.df <- kt %>%
  reduce(left_join, by ='Ref_Year')

kt.df[kt.df=='B'] <- NA

kt.df <- as.data.frame(sapply(kt.df, as.numeric))

kt.means <- rowMeans(kt.df[,2:18], na.rm=T)
skel.plot(as.numeric(kt.means), master=T, sname='Master')

barplot(as.numeric(kt[[1]]$Width_Score))
?skel.plot
sk <- rbindlist(kt, idcol='tab', fill=T)

p <- ggplot(sk, aes(x=Ref_Year, y=Width_Score)) + 
  geom_col() + 
  xlab('') +
  facet_grid(cols=vars(tab))

p


