library(caTools)
library(data.table)
library(ggplot2)
library(rwaveform)
library(splitstackshape)
library(sqldf)
library(tidyverse)

fp <- ingest('/Users/hmworsham/Downloads/2018_CRBU_1_2018061214_FL001')
re <- fp$re
geo <- fp$geo

re.i.v <- as.vector(unlist(fp$re[2,]))
geo.i.v <- as.vector(unlist(fp$geol[2,]))
re.i <- data.table(1:501, as.vector(unlist(fp$re[2,])))
geo.i <- data.table(1:17, as.vector(unlist(fp$geol[2,])))

ggplot(re.i[2:48,], aes(x=V1, y=V2)) +
  geom_line(color='red', linewidth=3) +
  theme_classic(base_size = 24) +
  labs(x='Time (ns)', y='Signal intensity')

fp$geol


# Assign variables from geolocation columns
rindex<-table(re$index)
geo0<-data.frame(geo, rindex)
ngeo<-expandRows(geo0, "Freq")

orix<-ngeo$orix
oriy<-ngeo$oriy
oriz<-ngeo$oriz
dx<-ngeo$dx
dy<-ngeo$dy
dz<-ngeo$dz
refbin<-ngeo$refbin

## outref and outpeak become the reference points for decon+decom results
outref<-ngeo$outref
outpeak<-ngeo$outpeak


xvect <- -1*1:50*geo$dx[2]
yvect <- 1+1:50*geo$dy[2]
zvect <- 3428+1:50*geo$dz[2]
re.i.v <- re.i.v[1:50]

geovect <- data.frame(xvect,yvect,zvect,re.i.v)

ggplot(geovect, aes(x=xvect, y=yvect, color=re.i.v)) +
  geom_line() +
  scale_color_viridis_c(name='Intensity')

plot_ly(x=xvect, y=yvect, z=zvect, type='scatter3d', mode='markers',
                marker = list(size = 6, color=re.i.v, colorscale = 'Viridis'))
fig <- layout(fig, xaxis=list(title='X (m)', anchor='xvect'),
                 yaxis=list(title='Y (m)'))

fig

