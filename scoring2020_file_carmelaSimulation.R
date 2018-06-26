#build out last year's districts and scores along with the same data but allocating a weight of 0 to modeled parcels 
#repurposing this script to calculate FY19 district scores as well as FY20 using the same secvol makeups

library(foreign)
library(plyr)
library(ggplot2)

#this file's data:
#load('G:/Property/Luis_C/reDistricting/2020/scoring/reconcile_19_20.RData')
#save.image('G:/Property/Luis_C/reDistricting/2020/scoring/reconcile_19_20.RData')

#normalized FY19 data: district - secvol - assessor
fy19.dist<-read.csv('G:/Property/Luis_C/reDistricting/2020/fy19_latestDists.csv')

fy19.dist[fy19.dist$'Dist'=='3-22','Tier']
fy19.dist[fy19.dist$'Dist'=='5-2','Tier']
nrow(fy19.dist[fy19.dist$'Dist'=='4-18',])
fy19.dist[fy19.dist$'Dist'=='3-2','Tier']
variable.scores.dists[variable.scores.dists$Dist=='4-18','bsv']

#fy 2020 work:
fy20<-new.env()
load('G:/Property/Luis_C/reDistricting/2020/scoring/scoring.RData',envir=fy20)
#fy 2019 original scores:
fy19<-new.env()
load('G:/Property/Luis_C/reDistricting/2019/scoring/scoring.RData',envir=fy19)

#total scores on each secvol... ignore these/focus only on FY20
bsv.score.MH.19<-get('bsv.score.MH',envir=fy19)
bsv.score.BX.19<-get('bsv.score.BX',envir=fy19)
bsv.score.BK.19<-get('bsv.score.BK',envir=fy19)
bsv.score.QN.19<-get('bsv.score.QN',envir=fy19)
bsv.score.SI.19<-get('bsv.score.SI',envir=fy19)


#total scores on each secvol
bsv.score.MH.20<-get('bsv.score.MH',envir=fy20)
#write.csv(bsv.score.MH.20,'G:/Property/Luis_C/reDistricting/2020/mh_scores.csv',row.names=FALSE)
bsv.score.BX.20<-get('bsv.score.BX',envir=fy20)
bsv.score.BK.20<-get('bsv.score.BK',envir=fy20)
bsv.score.QN.20<-get('bsv.score.QNS',envir=fy20)
bsv.score.SI.20<-get('bsv.score.SI',envir=fy20)

#ORIGINAL SCORES DATA
#store the scores into a list - in order
scores.list.20<-list(bsv.score.MH.20,bsv.score.BX.20,bsv.score.BK.20,bsv.score.QN.20,bsv.score.SI.20)
scores.list.19<-list(bsv.score.MH.19,bsv.score.BX.19,bsv.score.BK.19,bsv.score.QN.19,bsv.score.SI.19)

dist.score.19<-function(boro,secvol){
  boro.scores<-scores.list.19[[boro]]
  #boro.scores is the dataframe containing the secvol scores
  g<-sum(boro.scores[boro.scores$bsv %in% secvol,'score.tot'])
  return(g)
}

#boro is an integer; secvol a character VECTOR
dist.score.20<-function(boro,secvol){
  boro.scores<-scores.list.20[[boro]]
  #boro.scores is the dataframe containing the secvol scores
  g<-sum(boro.scores[boro.scores$bsv %in% secvol,'score.tot'])
  return(g)
}


#calculate scores across district for each composite variable
gh<-rbind(bsv.score.MH.20,bsv.score.BX.20,bsv.score.BK.20,bsv.score.QN.20,bsv.score.SI.20)

#update the bsv identified by Mike F.
write.csv(gh,'G:/Property/Luis_C/reDistricting/2020/scoring/scores_bsv.csv',row.names=FALSE)

#######  HOT FIX Mike F.  #############
#lost 10601... it is in fy19
variable.scores.dists<-merge(gh,fy19.dist,by.x='bsv',by='SecVol')
#need to conver to str/char for relabeling
variable.scores.dists$Dist<-as.character(variable.scores.dists$Dist)


variable.scores.dists[variable.scores.dists$bsv=='30207','Dist']<-'3-2'
variable.scores.dists[variable.scores.dists$bsv=='30204','Dist']<-'3-2'
variable.scores.dists[variable.scores.dists$bsv=='30305','Dist']<-'3-1'
variable.scores.dists[variable.scores.dists$bsv=='31709','Dist']<-'3-20'
variable.scores.dists[variable.scores.dists$bsv=='50504','Dist']<-'5-3'
variable.scores.dists[variable.scores.dists$bsv=='50503','Dist']<-'5-3'
variable.scores.dists[variable.scores.dists$bsv=='31101','Dist']<-'3-6'
variable.scores.dists[variable.scores.dists$bsv=='31003','Dist']<-'3-6'
variable.scores.dists[variable.scores.dists$bsv=='31509','Dist']<-'3-16'
variable.scores.dists[variable.scores.dists$bsv=='31510','Dist']<-'3-16'
variable.scores.dists[variable.scores.dists$bsv=='41204','Dist']<-'4-17'
variable.scores.dists[variable.scores.dists$bsv=='41606','Dist']<-'4-17'
variable.scores.dists[variable.scores.dists$bsv=='50303','Dist']<-'5-1'
variable.scores.dists[variable.scores.dists$bsv=='50201','Dist']<-'5-1'
variable.scores.dists[variable.scores.dists$bsv=='10802','Dist']<-'1-21'

#change the tiers
unique(variable.scores.dists[variable.scores.dists$Dist %in% c('3-17','3-16','3-7','3-6','3-2'),c('Tier','Dist')])
unique(variable.scores.dists[variable.scores.dists$Dist %in% c('1-20','1-21'),c('Tier','Dist')])

variable.scores.dists[variable.scores.dists$bsv=='30207','Tier']<-'3A'
variable.scores.dists[variable.scores.dists$bsv=='41204','Tier']<-'2'
variable.scores.dists[variable.scores.dists$bsv=='41606','Tier']<-'2'
variable.scores.dists[variable.scores.dists$bsv=='50303','Tier']<-'3A'
variable.scores.dists[variable.scores.dists$bsv=='50201','Tier']<-'3A'
variable.scores.dists[variable.scores.dists$bsv=='10802','Tier']<-'2'
variable.scores.dists[variable.scores.dists$bsv=='31509','Tier']<-'1'
variable.scores.dists[variable.scores.dists$bsv=='31510','Tier']<-'1'
variable.scores.dists[variable.scores.dists$bsv=='50504','Tier']<-'2'
variable.scores.dists[variable.scores.dists$bsv=='50503','Tier']<-'2'
#some light quality checking
variable.scores.dists[variable.scores.dists$Dist=='5-2','bsv']

g<-ddply(variable.scores.dists,c('Dist','Tier','Boro'),function(x) sum(x$score.tot))

variable.scores.dists[variable.scores.dists$Dist=='3-12',c('bsv','score.tot')]

ggplot(g,aes(Dist,V1))+geom_bar(aes(fill = Tier),stat='identity')+geom_hline(aes(yintercept=103))+geom_hline(aes(yintercept=123))+geom_hline(aes(yintercept=154))+facet_wrap(~Boro, scales="free")+theme(axis.text.x=element_text(angle=90, hjust = 0, vjust=.5)) + ggtitle('Scores by district by Boro')

write.csv(variable.scores.dists,'G:/Property/Luis_C/reDistricting/2020/scoring/sec_vol_final2020.csv',row.names=FALSE)

############  end hot fix ##########

boros<-1:5
s.19<-vector()
for (i in boros)
{
  part<-fy19.dist[fy19.dist$Boro==i,]
  s.19<-c(s.19,ddply(part,'District.Num',function(x) dist.score.19(i,x$SecVol)))
}


boros<-1:5
s.20<-vector()
for (i in boros)
{
  part<-fy19.dist[fy19.dist$Boro==i,]
  s.20<-c(s.20,ddply(part,c('Dist','Tier'),function(x) dist.score.20(i,as.character(x$SecVol))))
}


#create dataframe for each boro from the list
name.vec<-c('district','score')
mh.dist.scores.19<-data.frame(s.19[[1]],s.19[[2]])
names(mh.dist.scores.19)<-name.vec
bx.dist.scores.19<-data.frame(s.19[[3]],s.19[[4]])
names(bx.dist.scores.19)<-name.vec
bk.dist.scores.19<-data.frame(s.19[[5]],s.19[[6]])
names(bk.dist.scores.19)<-name.vec
qn.dist.scores.19<-data.frame(s.19[[7]],s.19[[8]])
names(qn.dist.scores.19)<-name.vec
si.dist.scores.19<-data.frame(s.19[[9]],s.19[[10]])
names(si.dist.scores.19)<-name.vec

name.vec<-c('district','tier','score')
mh.dist.scores.20<-data.frame(s.20[[1]],s.20[[2]],s.20[[3]])
names(mh.dist.scores.20)<-name.vec
bx.dist.scores.20<-data.frame(s.20[[4]],s.20[[5]],s.20[[6]])
names(bx.dist.scores.20)<-name.vec
bk.dist.scores.20<-data.frame(s.20[[7]],s.20[[8]],s.20[[9]])
names(bk.dist.scores.20)<-name.vec
qn.dist.scores.20<-data.frame(s.20[[10]],s.20[[11]],s.20[[12]])
names(qn.dist.scores.20)<-name.vec
si.dist.scores.20<-data.frame(s.20[[13]],s.20[[14]],s.20[[15]])
names(si.dist.scores.20)<-name.vec

#first, need to create a boro field, district label for each district, then merge on district label
mh.dist.scores.19$boro<-'1'
bx.dist.scores.19$boro<-'2'
bk.dist.scores.19$boro<-'3'
qn.dist.scores.19$boro<-'4'
si.dist.scores.19$boro<-'5'

mh.dist.scores.20$boro<-'1'
bx.dist.scores.20$boro<-'2'
bk.dist.scores.20$boro<-'3'
qn.dist.scores.20$boro<-'4'
si.dist.scores.20$boro<-'5'

city.19<-rbind(mh.dist.scores.19,bx.dist.scores.19,bk.dist.scores.19,qn.dist.scores.19,si.dist.scores.19)

city.20<-rbind(mh.dist.scores.20,bx.dist.scores.20,bk.dist.scores.20,qn.dist.scores.20,si.dist.scores.20)

write.csv(city.20,'G:/Property/Luis_C/reDistricting/2020/scoring/city_20_post_revision.csv',row.names=FALSE)
#create a field of district labels
#city.20$dist.label<-paste(city.20$boro,city.20$district,sep='-')
#city.19$dist.label<-paste(city.19$boro,city.19$district,sep='-')

both.yrs<-merge(city.19,city.20,by='dist.label',suffixes=c('.fy20','.fy19'))

#calculate the difference in scores per district
both.yrs$dist.diff<-both.yrs$score.fy20-both.yrs$score.fy19

#VISUALIZATION
#plot a grid using boro and tier as a factor... ensure that same frequency of districts in both years, including the district labels

#plot the distribution or variance around each tier



#barplot of districts by boro
ggplot(city.20,aes(district,score, fill = district))+guides(fill=FALSE)+geom_hline(aes(yintercept=103))+geom_hline(aes(yintercept=123))+geom_hline(aes(yintercept=154))+geom_bar(stat='identity')+facet_wrap(~boro, scales="free")+theme(axis.text.x=element_text(angle=90, hjust = 0, vjust=.5)) + ggtitle('Scores by district by Boro')


#calculate differences of each district to the ideal district score for that tier
list.scores<-list("1"=103,"2"=123.6,"3A"=154.5)
list.scores["1"][[1]]
g<-apply(city.20,1,function(x) x['score']-list.scores[x['tier']][[1]])
city.20['tier']
g<-apply(city.20,1,function(x) x['score'])
g<-apply(city.20,1,function(x) list.scores[x['tier']][[1]])
#positive scores are where the district score is greater than ideal and negative scores are for small-sized districts
city.20$score.diff<-city.20$score-g
#plot a horizontal bar chart w/0 in the middle facet_wrap


write.csv(city.20,'G:/Property/Luis_C/reDistricting/2020/scoring/dist_scores_deviation.csv',row.names=FALSE)

#boxplot by tier and boro
ggplot(city.20,aes(district,score.diff,fill=tier))+geom_bar(stat='identity')+facet_wrap(~boro, scales="free")+coord_flip()

#CARMELA STUFF... UPDATED SCORES W/0 VALUES
scores.0.list<-list(bsv.score.0.MH,bsv.score.0.BX,bsv.score.0.BK,bsv.score.0.QN,bsv.score.0.SI)

dist.0.score<-function(boro,secvol){
  boro.scores<-scores.0.list[[boro]]
  #boro.scores is the dataframe containing the secvol scores
  g<-sum(boro.scores[boro.scores$bsv %in% secvol,'score.tot'])
  return(g)
}

#select by boro then by district number

boros<-1:5
s.0<-vector()
for (i in boros)
{
  part<-luke[luke$boro==as.character(i),]
  s.0<-c(s.0,ddply(part,'District.Num',function(x) dist.0.score(i,x$SecVol)))
}

s.0[[10]]
s[[10]]


