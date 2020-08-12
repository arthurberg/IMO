#Data Analysis of the International Mathematics Olympiad: Tables and Figures

library(plyr) #mapvalues
library(ggplot2)
library(hrbrthemes) #theme_ipsum
#library(gridExtra) #grid.arrange 
#library(cowplot) #plot_grid
library(patchwork) #plot_annotation
library(reshape2) #melt
library(extrafont) #font_import
library(xtable)
library(viridis) #viridis_pal, scale_fill_viridis
library(tidyverse)
library(tidyr) #spread
library(grid) #panel.spacing.x
library(maps) #map_data
library(wpp2019) #pop (world population data)

#font_import()

rm(list=ls())

#####################
##### Data Prep #####
#####################
#Data Analysis of the International Mathematics Olympiad: Data Preperation

#Load Datasets
ps=read.csv("Participant_Scores.csv",stringsAsFactors=F,header=T)
ca=read.csv("Country_Abbreviations.csv",stringsAsFactors=F,header=T)
pc=read.csv("Problem_Classifications_Updated.csv",stringsAsFactors=F,header=T)

# Rename some countries with simpler names
ca[ca$Country=="Republic of Korea","Country"]="South Korea"
ca[ca$Country=="Russian Federation","Country"]="Russia"
ca[ca$Country=="Democratic People's Republic of Korea","Country"]="North Korea"
ca[ca$Country=="Republic of Moldova","Country"]="Moldova"
ca[ca$Country=="Republic of North Macedonia","Country"]="North Macedonia"
ca[ca$Country=="People's Republic of China","Country"]="China"
ca[ca$Country=="Islamic Republic of Iran","Country"]="Iran"
ca[ca$Country=="The former Yugoslav Republic of Macedonia","Country"]="North Macedonia"
ca[ca$Country=="Turkish Republic of Northern Cyprus","Country"]="Northern Cyprus"

ca[ca$Country=="United States of America","Country"]="United States"
#ca[ca$Country=="","Country"]=""

ps[ps$Country=="Republic of Korea","Country"]="South Korea"
ps[ps$Country=="Russian Federation","Country"]="Russia"
ps[ps$Country=="Democratic People's Republic of Korea","Country"]="North Korea"
ps[ps$Country=="Republic of Moldova","Country"]="Moldova"
ps[ps$Country=="Republic of North Macedonia","Country"]="North Macedonia"
ps[ps$Country=="People's Republic of China","Country"]="China"
ps[ps$Country=="Islamic Republic of Iran","Country"]="Iran"
ps[ps$Country=="The former Yugoslav Republic of Macedonia","Country"]="North Macedonia"
ps[ps$Country=="Turkish Republic of Northern Cyprus","Country"]="Northern Cyprus"
ps[ps$Country=="United States of America","Country"]="United States"

rownames(ca)=ca$Country

# Extract gender from ps and add gender column

ind.m=grep("♂",ps$Name)
ind.f=grep("♀",ps$Name)
ind.u=which(!is.element(1:dim(ps)[1],c(ind.m,ind.f)))
years=ps[,1]
gender=rep(NA,dim(ps)[1])
names(gender)=years

gender[ind.m]="Male"
gender[ind.f]="Female"
gender[ind.u]="Unknown"

gender2=factor(gender)
ps$Gender=gender2

mydat1=ps[,c("Year","Country","P1s","P2s","P3s","P4s","P5s","P6s","Total","Gender")]

#mydat1: original dataset including year, country, scores, total, and gender
#mydat2: removes years with extensive missingness

###############################################
###############################################

# Fraction of missing scores by year
(x=round(sapply(split(is.na(ps$P1),ps$Year),mean),2))

# Years with score missingness > 5% excluded
mydat2=mydat1[is.element(mydat1$Year,names(x[x<=.05])),]
pc2=pc[is.element(pc$Year,names(x[x<=.05])),]

# Total number of participants over all time
dim(mydat1)[1]

# 2019 participation numbers
length(unique(mydat2$Country[mydat2$Year==2019]))

# Scores averages by year
mu=sapply(split(mydat2$Total,mydat2$Year),mean,na.rm=T)

# Score standard deviations by year
sig=sapply(split(mydat2$Total,mydat2$Year),sd,na.rm=T)

# Plot of participation, means and std devs over time
n=table(mydat2$Year)
df=data.frame(Year=as.numeric(names(n)),Number=as.numeric(n),Mu=as.numeric(mu),Sig=as.numeric(sig))

gr1=ggplot(df,aes(x=Year,y=Number)) + geom_point(color="#69b3a2",size=2,alpha=0.8) + 
ylab(NULL) + 
stat_smooth(geom="line",method="loess",formula="y~x",alpha=.8,color="#69b3a2",size=1.2) + ggtitle("Num of participants")

gr2=ggplot(df,aes(x=Year,y=Mu)) + geom_point(color="#69b3a2",size=2,alpha=0.8) + 
ylab(NULL) + 
stat_smooth(geom="line",method="loess",formula="y~x",alpha=.8,color="#69b3a2",size=1.2) + ggtitle("Score Averages")

gr3=ggplot(df,aes(x=Year,y=Sig)) + geom_point(color="#69b3a2",size=2,alpha=0.8) + 
ylab(NULL) + 
stat_smooth(geom="line",method="loess",formula="y~x",alpha=.8,color="#69b3a2",size=1.2) + ggtitle("Score Std Dev's")

####################
##### FIGURE 1 #####
####################

gr4 <- gr1 + gr2 + gr3 + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 13),text = element_text(family = "Comic Sans MS"))+ theme_ipsum(plot_margin=margin(r=5),base_family = "Comic Sans MS",plot_title_size=13) 

dev.new(width=7.5, height=2.5, unit="in")
gr4

ggsave("fig1.png",type="cairo",dpi=600)

###############################################
###############################################

#####################
##### Functions ######
#####################

#f.filter removes the few individuals without all six scores and removes the few individuals on teams without six participants

f.filter = function(dat){
	f.no.missing=function(x){sum(is.na(x))==0}
dat2=dat[apply(dat[,c("P1s","P2s","P3s","P4s","P5s","P6s")],1,f.no.missing),] 

dat3=dat2
for(year in unique(dat2$Year)){
	x=(dat3$Year==year & is.element(dat3$Country,names(which(table(dat3[dat3$Year==year,"Country"])!=6) )))
dat3=dat3[!x,]
}

dat4=dat3[!is.element(dat3$Country,c("Commonwealth of Independent States","Union of Soviet Socialist Republics","German Democratic Republic","Czechoslovakia","Yugoslavia","Serbia and Montenegro")),]

dat4
}

mydat2.filter=f.filter(mydat2)
dim(mydat2)
dim(mydat2.filter)


#f.stand.year standardizes so that total scores have mean 0 and standard devaation of 1 for each year.

f.stand.year = function(dat){
mu=sapply(split(dat$Total,dat$Year),mean,na.rm=T)
sig=sapply(split(dat$Total,dat$Year),sd,na.rm=T)
		
	f.standardize=function(x){
	(x[c("P1s","P2s","P3s","P4s","P5s","P6s")]-(mu[as.character(x["Year"])])/6) /sig[as.character(x["Year"])]
}
ss=apply(dat[,c("Year","P1s","P2s","P3s","P4s","P5s","P6s")],1,f.standardize)

dat2=dat
dat2[,c("P1s","P2s","P3s","P4s","P5s","P6s")]=t(ss)
dat2$Total=apply(dat2[,c("P1s","P2s","P3s","P4s","P5s","P6s")],1,sum)

dat2	
}

mydat2.filter.stand=f.stand.year(mydat2.filter)

head(mydat2.filter)
head(mydat2.filter.stand)
sapply(split(mydat2.filter.stand$Total,mydat2.filter.stand$Year),mean)
sapply(split(mydat2.filter.stand$Total,mydat2.filter.stand$Year),sd)

# f.stand.problem standardizes so that scores for each problem has a mean 0 and standard devaation of 1

f.stand.problem = function(dat){

mu1=sapply(split(dat$P1s,dat$Year),mean,na.rm=T)
mu2=sapply(split(dat$P2s,dat$Year),mean,na.rm=T)
mu3=sapply(split(dat$P3s,dat$Year),mean,na.rm=T)
mu4=sapply(split(dat$P4s,dat$Year),mean,na.rm=T)
mu5=sapply(split(dat$P5s,dat$Year),mean,na.rm=T)
mu6=sapply(split(dat$P6s,dat$Year),mean,na.rm=T)

sig1=sapply(split(dat$P1s,dat$Year),sd,na.rm=T)
sig2=sapply(split(dat$P2s,dat$Year),sd,na.rm=T)
sig3=sapply(split(dat$P3s,dat$Year),sd,na.rm=T)
sig4=sapply(split(dat$P4s,dat$Year),sd,na.rm=T)
sig5=sapply(split(dat$P5s,dat$Year),sd,na.rm=T)
sig6=sapply(split(dat$P6s,dat$Year),sd,na.rm=T)

f.standardize=function(x){
	year=as.character(x[1])
	o1=(x["P1s"]-mu1[year])/sig1[year]
	o2=(x["P2s"]-mu2[year])/sig2[year]
	o3=(x["P3s"]-mu3[year])/sig3[year]
	o4=(x["P4s"]-mu4[year])/sig4[year]		
	o5=(x["P5s"]-mu5[year])/sig5[year]
	o6=(x["P6s"]-mu6[year])/sig6[year]
c(o1,o2,o3,o4,o5,o6)
}
ss=apply(dat[,c("Year","P1s","P2s","P3s","P4s","P5s","P6s")],1,f.standardize)

dat2=dat
dat2[,c("P1s","P2s","P3s","P4s","P5s","P6s")]=t(ss)
dat2$Total=apply(dat2[,c("P1s","P2s","P3s","P4s","P5s","P6s")],1,sum)

dat2
}

mydat2.stand.problem=f.stand.problem(mydat2)

mean(mydat2.stand.problem$P2s[mydat2.stand.problem$Year==2018])
sd(mydat2.stand.problem$P2s[mydat2.stand.problem$Year==2018])

# f.country.agg aggregates over countries
f.country.agg = function(dat){
agg1=aggregate(P1s~Year+Country,data=dat,function(x){sum(x,na.rm=T)})
agg2=aggregate(P2s~Year+Country,data=dat,function(x){sum(x,na.rm=T)})
agg3=aggregate(P3s~Year+Country,data=dat,function(x){sum(x,na.rm=T)})
agg4=aggregate(P4s~Year+Country,data=dat,function(x){sum(x,na.rm=T)})
agg5=aggregate(P5s~Year+Country,data=dat,function(x){sum(x,na.rm=T)})
agg6=aggregate(P6s~Year+Country,data=dat,function(x){sum(x,na.rm=T)})
agg.tot=aggregate(Total~Year+Country,data=dat,function(x){sum(x,na.rm=T)})

agg.gen=aggregate(Gender~Year+Country,data=dat,function(x){sum(x=="Female")})
dat2=Reduce(merge,list(agg1,agg2,agg3,agg4,agg5,agg6,agg.tot,agg.gen))

dat2
}

mydat2.filter.stand.country=f.country.agg(mydat2.filter.stand)

head(mydat2.filter.stand)
head(mydat2.filter.stand.country)

# Converts wide data to long and adds problem type

f.long.problem = function(dat){	
dat2=reshape2::melt(dat[,c("Year","Country","P1s","P2s","P3s","P4s","P5s","P6s","Gender")],id.vars=c("Year","Country","Gender"),value.name="Score")

dat2$id=paste(dat$Year,sapply(strsplit(as.character(dat2$variable),"s"),function(x){x[1]}),sep="_")

dat2$P.type=pc.long[dat2$id,"value"]	
dat2$P.type2=mapvalues(dat2$P.type,from=c("A","C","G","N"),to=c("Alg","Com","Geo","Num"))
	
dat3=dat2[,c("Year","Country","Gender","id","P.type2","Score")]

dat3
}

##### End of Functions ######

##############################
# Section: The Hardest and Easiest IMO Problems
##############################

mydat=f.stand.year(mydat2)

# Sorting the problems by difficulty
avg.scores=t(sapply(split(mydat[,c("P1s","P2s","P3s","P4s","P5s","P6s")],mydat$"Year"),apply,2,mean,na.rm=T))

## (the average question scores average to 0 over the entire test for each year)
apply(avg.scores,1,mean)

avg.scores.long=reshape2::melt(avg.scores)
ord1=order(avg.scores.long[,"value"],decreasing=T) 
ord2=order(avg.scores.long[,"value"],decreasing=F) 
avg.scores.long[ord1,][1:10,]
avg.scores.long[ord2,][1:10,]

## get the number of correct scores corresponding to the hardest and easiest problems
f.correct=function(x){sum(x==7,na.rm=T)}
f.total=function(x){sum(!is.na(x))}
correct.scores=t(sapply(split(mydat2[,c("P1s","P2s","P3s","P4s","P5s","P6s")],mydat2$"Year"),apply,2,f.correct))
total.scores=t(sapply(split(mydat2[,c("P1s","P2s","P3s","P4s","P5s","P6s")],mydat2$"Year"),apply,2,f.total))

correct.scores.long=reshape2::melt(correct.scores)
total.scores.long=melt(total.scores)

# Table of Hardest and easiest scores
n=10
N.Correct=correct.scores.long[,"value"]
N.total=total.scores.long[,"value"]
Prop=round(N.Correct/N.total * 100, 1)

df=data.frame(Year=avg.scores.long[,1],Problem=avg.scores.long[,2],S.Score=avg.scores.long[,3],N.Correct=N.Correct,N.total=N.total,Prop=Prop)

df$Problem2=sapply(strsplit(as.character(df$Problem),"s"),function(x){x[1]})

rownames(df)=paste(df$Year,df$Problem2,sep="_")

# Bring in the problem categories
pc.long=reshape2::melt(pc[,c("Year","P1.t","P2.t","P3.t","P4.t","P5.t","P6.t")],id="Year")

rownames(pc.long)=paste(pc.long$Year,sapply(strsplit(as.character(pc.long$variable),".",fixed=T),function(x){x[1]}),sep="_")

df$P.type=pc.long[rownames(df),"value"]

df1=df[ord1,]
df2=df[ord2,]

### Note that "Problem2" refers to a modified form of "Problem" in the dataframes, which converts P1s to P1, P2s to P2, etc.
mat1=df1[1:n,c("Year","Problem2","P.type","Prop","S.Score")]
mat2=df2[1:n,c("Year","Problem2","P.type","Prop","S.Score")]

#####################
##### TABLE 1 ######
#####################

print(xtable(cbind(mat1,mat2),digits=c(F,rep(c(F,F,F,1,2),2))),include.rownames=F)

###############################################
###############################################

# double check the proportion of correct responses
mean(mydat2[mydat2$Year==2017,"P1s"]==7) 

##############################
# Section: Analysis by Country
##############################

mydat=f.country.agg(f.stand.year(f.filter(mydat2)))

mu=sapply(split(mydat$Total,mydat$Country),mean)
n=sapply(split(mydat$Total,mydat$Country),length)

ord=order(mu,decreasing=T)
df2=data.frame(Std.Score=round(mu,2),N=n)

data(pop)
country.list=rownames(df2[ord,][1:15,])
country.list[!is.element(country.list,pop$name)]

grep("Iran",pop$name,value=T)
country.list[country.list=="Russia"]="Russian Federation"
country.list[country.list=="United States"]="United States of America"
country.list[country.list=="South Korea"]="Republic of Korea"
country.list[country.list=="North Korea"]="Dem. People's Rep. of Korea"
country.list[country.list=="Vietnam"]="Viet Nam"
country.list[country.list=="Taiwan"]="China, Taiwan Province of China"
country.list[country.list=="Iran"]="Iran (Islamic Republic of)"

country.list[!is.element(country.list,pop$name)]
pop.sub=pop[is.element(pop$name,country.list),]
rownames(pop.sub)=pop.sub$name
round(pop.sub[country.list,"2020"])

mypop=round(pop.sub[country.list,"2020"],digits=-3)/1000

df3=data.frame(df2[ord,][1:15,],Pop=mypop)

#####################
##### TABLE 2 ######
#####################
xtable(df3,digits=c(0,2,0,0))

###############################################
###############################################

#####################
##### FIGURE 2 ######
#####################

df3=mydat[grep("China|Russia|United States|South Korea|Romania|North Korea",mydat$Country) ,c("Country","Year","Total")]

df3$Country=factor(df3$Country,levels=c("United States","South Korea","China","North Korea","Russia","Romania"))

dev.new(width=8, height=4, unit="in")

ggplot(df3, aes(Year, Total, colour=Country)) +
  geom_point(alpha=.8) + ylab(NULL)+stat_smooth(geom="line",method="loess",formula="y~x",alpha=.8,size=1.2) + ggtitle("Country Performance By Year")+ theme_ipsum(plot_margin=margin(t=2,l=2,r=0),base_family = "Comic Sans MS",plot_title_size=13)+
  theme(plot.title = element_text(hjust = 0.5)) + labs(color=NULL)

ggsave("fig2.png",type="cairo",dpi=600)

###############################################
###############################################

##############################
# Section: Analysis by Problem Type
##############################

# Scores difficulty and problem type

## add problem type 1/4, 2/5, and 3/6 to dataframe
df$Problem3=mapvalues(df$Problem2,from=paste("P",1:6,sep=""),to=rep(c("Prob 1/Prob 4","Prob 2/Prob 5","Prob 3/Prob 6"),2))

df$P.type2=mapvalues(df$P.type,from=c("A","C","G","N"),to=c("Alg","Com","Geo","Num"))

(tab=table(df$Problem3,df$P.type2))

##########################
##### FIGURE 3A    ######
##########################

dev.new(width=4, height=3, unit="in")

g=df %>%
  ggplot( aes(x=P.type2, y=S.Score, fill=P.type2)) +
    geom_boxplot(show.legend=F) +
    geom_jitter(color="black", size=0.4, alpha=0.2,width=.2) +
    theme_ipsum(plot_margin=margin(t=2),base_family = "Comic Sans MS",plot_title_size=13) +
      theme(plot.tag = element_text(size = 13),text = element_text(family = "Comic Sans MS"),legend.position="none")+ 
    ggtitle("(A) Standardized Scores By Prob Type") +
    xlab("") + ylab("")
g

ggsave("fig3a.png",type="cairo",dpi=600)

###############################################
###############################################

##########################
##### FIGURE 3B    ######
##########################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

png("fig3b.png",width=4,height=3,units="in",res=600)
#dev.new(width=4, height=3, unit="in")
par(mar=c(1,2,2,0),family="Comic Sans MS")
mosaicplot(Problem3~P.type2,data=df,col=gg_color_hue(4),ylab="",xlab="",main="",cex.axis=.85)
title("(B) Mosaic Plot",cex.main=.95)
dev.off()

###############################################
###############################################





tmp=f.stand.problem(f.country.agg(f.filter(mydat2)))
head(tmp)
mydat=f.long.problem(tmp)

mycountries=unique(mydat$Country)

res=rep(NA,length(mycountries))
names(res)=mycountries
counter=1
for(i in mycountries){
fit=summary(lm(Score~P.type2,data=mydat,subset=Country==i))	

res[counter]=pf(fit$fstatistic[1],fit$fstatistic[2],fit$fstatistic[3],lower.tail=FALSE)
counter=counter+1
}

res2=sort(res,dec=F)

# Format the results to include averages and numbers of each problem type

mydat.score=aggregate(mydat$Score,list(mydat$Country,mydat$P.type2),mean,na.rm=T)

mydat.n=aggregate(mydat$Score,list(mydat$Country,mydat$P.type2),length)

colnames(mydat.score)=c("Country","Type","Score")
colnames(mydat.n)=c("Country","N","Score")

mat1=spread(mydat.score,Type,Score)
mat2=spread(mydat.n,N,Score)
mat3=cbind(mat1,mat2[,-1])

f=function(x){
	x1=x[1:4]
	x2=x[5:8]
	ind.min=which.min(x1)
	ind.max=which.max(x1)
	x1b=format(signif(x1,2),nsmall=2)
	x1b[ind.min]=paste("{\\color{orange}",x1b[ind.min],"}")
	x1b[ind.max]=paste("{\\color{blue}",x1b[ind.max],"}")
	o1=paste(x1b[1]," (",x2[1],")",sep="")
	o2=paste(x1b[2]," (",x2[2],")",sep="")
	o3=paste(x1b[3]," (",x2[3],")",sep="")
	o4=paste(x1b[4]," (",x2[4],")",sep="")
	c(o1,o2,o3,o4)
	}

mat4=cbind(mat1$Country,t(apply(mat3[,-1],1,f)))
colnames(mat4)=colnames(mat1)
rownames(mat4)=mat1$Country

mat5=cbind(mat4[names(res2),],signif(res2,2))
mat5[,-1]

#####################
##### TABLE 3 ######
#####################

print(xtable(mat5[,-1][1:15,]),sanitize.text.function=identity)

###############################################
###############################################

#####################
##### FIGURE 4 ######
#####################


df4=mydat[grep("Greece|Norway|United States",mydat$Country),]

df4$Country=ordered(df4$Country,c("Greece","Norway","United States"))

dev.new(width=8.5, height=3, unit="in")

ggplot(data=df4, aes(x=Year, y=Score, group=P.type2, colour=P.type2)) + labs(color=NULL)+scale_color_manual(values=gg_color_hue(4)) +   geom_point(alpha=.8,cex=.5)+
facet_grid(cols=vars(Country))+stat_smooth(geom="line",method="loess",formula="y~x",alpha=.8,size=1.2)+coord_cartesian(ylim=c(-1.25, 2.75))+  theme_ipsum(plot_margin=margin(t=2,l=2,r=0),base_family = "Comic Sans MS",plot_title_size=13) + theme(panel.spacing.x=grid::unit(1,"lines"))

ggsave("fig4.png",type="cairo",dpi=600)

###############################################
###############################################

##############################
# Section: Home Advantage?
##############################

#Host

tmp=f.stand.year(f.country.agg(f.filter(mydat2)))
tmp2=reshape2::melt(tmp[,c("Year","Country","Total")],id.vars=c("Year","Country"),value.name="Score")
mydat=tmp2[,c("Year","Country","Score")]

tmp=f.country.agg(f.filter(mydat2))
tmp2=reshape2::melt(tmp[,c("Year","Country","Total")],id.vars=c("Year","Country"),value.name="Score")
mydat.original.scale=tmp2[,c("Year","Country","Score")]

# Host each year
host=pc2$Host
names(host)=pc2$Year

# Add Host and Contributer and add short form of country name

mydat$Country2=ca[as.character(mydat$Country),"Symbol"]
mydat$Host=host[as.character(mydat$Year)]

mydat$Host2=factor(c("Non-Host","Host")[(mydat$Country2==mydat$Host)+1],levels=c("Non-Host","Host"))

fit1=lm(Score~Host2+Country*(Year),data=mydat)
fit2=lm(Score~Host2+Country*(Year+I(Year^2)),data=mydat)
anova(fit1,fit2)

summary(fit2)$coef["Host2Host",]
summary(fit2)$adj.r.squared

(host.effect=summary(fit2)$coef["Host2Host",1] * sd(mydat.original.scale$Score))
host.effect/mean(mydat.original.scale$Score)*100

# Hosts with the most extreme effects

myhost=unique(host)

res1=rep(NA,length(myhost))
res2=rep(NA,length(myhost))
res3=rep(NA,length(myhost))

head(mydat)
counter=1
for(i in myhost){
	mat=mydat[mydat$Country2==i,]
	ind=(mat[,"Country2"]==mat[,"Host"])
if(sum(ind)>0){
	mu=mean(mat$Score[ind])
	res1[counter]=wilcox.test(mat$Score[!ind],mu=mu)$p.val
	res2[counter]=mean(mat$Score[!ind])-mu
}
	res3[counter]=sum(ind)
counter=counter+1
}
ord=order(res1)

tab=cbind(myhost[ord],signif(res1[ord],2),signif(res2[ord],2),res3[ord])

colnames(tab)=c("Country","p-value","difference","N years as host")

tab

#####################
##### FIGURE 5 ######
#####################

mydat.sub=mydat[(mydat$Country2=="CHN" |mydat$Country2=="USA"|mydat$Country2=="RUS") ,c("Year","Country","Score","Host2")]

head(mydat.sub)

dev.new(width=8, height=3, unit="in")

ggplot(mydat.sub,aes(x=Year,y=Score,color=Host2,shape=Host2)) + geom_point(size=2.5) + labs(color=NULL,shape=NULL)+
facet_grid(cols=vars(Country))+  theme_ipsum(plot_margin=margin(t=2,l=2,r=0),base_family = "Comic Sans MS",plot_title_size=13) + theme(panel.spacing.x=grid::unit(1,"lines")) + scale_color_manual(values=rev(gg_color_hue(2)))

ggsave("fig5.png",type="cairo",dpi=600)

###############################################
###############################################

# home-advantage for home-country problem contributions

pc3=reshape2::melt(pc2[,c("Year","P1.c","P2.c","P3.c","P4.c","P5.c","P6.c")],id=c("Year"))

rownames(pc3)=paste(pc3$Year,sapply(strsplit(as.character(pc3$variable),".",fixed=T),function(x){x[1]}),sep="_")

contributer=pc3[,"value",drop=F]

tmp=f.stand.problem(f.country.agg(f.filter(mydat2)))
mydat=f.long.problem(tmp)

head(mydat)

tmp=f.country.agg(f.filter(mydat2))
mydat.original.scale=f.long.problem(tmp)
head(mydat.original.scale)

mydat$contributer=contributer[as.character(mydat$id),1]
mydat$Country2=ca[as.character(mydat$Country),"Symbol"]
mydat$contributer2=c("No","Yes")[(mydat$contributer==mydat$Country2)+1]

table(mydat$contributer2)
head(mydat)

dim(mydat)

fit1=lm(scale(Score)~contributer2+Country*(Year),data=mydat)
fit2=lm(scale(Score)~contributer2+Country*(Year+I(Year^2)),data=mydat)
anova(fit1,fit2)

summary(fit1)$coef["contributer2Yes",]
summary(fit1)$adj.r.squared

(contributer.effect=summary(fit2)$coef["contributer2Yes",1] * sd(mydat.original.scale$Score))

contributer.effect/mean(mydat.original.scale$Score)*100

#####################################
#####################################

# Contributer with most extreme effects
sort(table(contributer))
mycontributer=names(which(table(contributer)>=5))

res=array(NA,dim=c(length(mycontributer),4))

head(mydat)
counter=1
for(i in mycontributer){
	mat=mydat[mydat$Country2==i,]
	ind.yes=mat$contributer2=="Yes"
	ind.no=mat$contributer2=="No"
res[counter,1]=sum(ind.yes)
res[counter,2]=sum(ind.no)

if(sum(ind.yes)>=4){	

myfit=lm(Score~contributer2+Year,data=mat)
res[counter,3]=summary(myfit)$coef["contributer2Yes","Estimate"]
res[counter,4]=summary(myfit)$coef["contributer2Yes",4]

}
counter=counter+1
}

ord=order(res[,4])

mydf=data.frame(mycontributer[ord],res[ord,])
colnames(mydf)=c("Country","N.contr","N.non.contr","effect est","p-val")

mydf

cat(sort(mycontributer[!is.na(res[,4])]),sep=", ")

####


#####################
##### FIGURE 6 ######
#####################

dat=mydat[mydat$Country2=="BGR",]

dev.new(width=6, height=3, unit="in")

ggplot(dat,aes(x=Year,y=Score,color=contributer2,shape=contributer2)) +geom_rect(aes(xmin=2002.5, xmax=2003.5,ymin=min(Score),ymax=max(Score)+.1),color="transparent",fill="yellow",alpha=.00197)+
 geom_point(size=2.5) + geom_point(data=subset(dat,contributer2 == "Yes"),size=2.5) + labs(color="BGR Problem",shape="BGR Problem",title="Bulgaria")+  theme_ipsum(plot_margin=margin(t=2,l=2,r=0),base_family = "Comic Sans MS",plot_title_size=13) + theme(panel.spacing.x=grid::unit(1,"lines"),plot.title = element_text(face="plain")) + scale_color_manual(values=rev(gg_color_hue(2)))

ggsave("fig6.png",type="cairo",dpi=600)

###############################################
###############################################

##############################
# Section: Gender Differences
##############################

world_map <- map_data("world")

grep("Cyprus",world_map$subregion,value=T)

world_map$region[world_map$region=="UK"]="United Kingdom"
world_map$region[world_map$region=="USA"]="United States"
world_map$region[world_map$region=="Trinidad"]="Trinidad and Tobago"
world_map$region[world_map$region=="Tobago"]="Trinidad and Tobago"
world_map$region[world_map$region=="Macedonia"]="North Macedonia"

world_map$region[world_map$subregion=="Hong Kong"& !is.na(world_map$subregion)]="Hong Kong"
world_map$region[world_map$subregion=="Macao"& !is.na(world_map$subregion)]="Macau"
world_map$region[world_map$subregion=="Northern Cyprus"& !is.na(world_map$subregion)]="Northern Cyprus"

mydat=f.filter(mydat2)

tab=t(sapply(split(mydat$Gender,mydat$Country),table))
tab2=cbind(tab,prop=tab[,1]/(tab[,2]+tab[,1]),tot=apply(tab[,1:2],1,sum))
tab3=tab2[!is.nan(tab2[,"prop"]),]
#tab3=tab2[tab2[,"tot"]>=48,]
df4=data.frame(region=rownames(tab3),prop=tab3[,"prop"],tot=tab3[,"tot"],stringsAsFactors=F)
df4$prop2=log(df4$prop+.04)
df4$prop3=cut(df4$prop,breaks=c(0,.025,.05,.1,.2,1),right=F)
df4$tot2=cut(df4$tot,c(0,50,100,150,300))

df4.map=right_join(df4,world_map, by = "region")

rownames(df4)[!is.element(rownames(df4),unique(world_map$region))]

##########################################
##### Old FIGURE 7 (with opacity) ######
##########################################

dev.new(width=7, height=4, unit="in")

theme_set(theme_void())
ggplot(df4.map, aes(long, lat, group = group, alpha = tot))+
  geom_polygon(aes(fill = prop3), color = "gray",size = 0.05)  + scale_fill_viridis_d(option = "D",direction=1,na.value="gray95",name = "% Female",labels=c("0%-2.5%","2.5%-5%","5%-10%","10%-20%","20%-75%","NA"))  + guides(alpha = F) +scale_alpha(range=c(.2,1))+ theme(text=element_text(size=13,family="Comic Sans MS"),plot.margin = unit(c(0, 0, 0, -1), "cm"),plot.title = element_text(hjust = 0.5,face = "bold")) + ggtitle("Proportion of Female Participation By Country")
  
ggsave("fig7.png",type="cairo",dpi=600)

#####################################
#####################################

#####################
##### FIGURE 7 ######
#####################

dev.new(width=7, height=4, unit="in")

theme_set(theme_void())
ggplot(df4.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = prop3), color = "gray",size = 0.05)  + scale_fill_viridis_d(option = "D",direction=1,na.value="gray95",name = "% Female",labels=c("0%-2.5%","2.5%-5%","5%-10%","10%-20%","20%-75%","NA"))  + guides(alpha = F) + theme(text=element_text(size=13,family="Comic Sans MS"),plot.margin = unit(c(0, 0, 0, -1), "cm"),plot.title = element_text(hjust = 0.5,face = "bold")) + ggtitle("Proportion of Female Participation By Country")
  
ggsave("fig7.png",type="cairo",dpi=600)

#####################################
#####################################



ord1=order(tab3[,"prop"],decreasing=FALSE)
ord2=order(tab3[,"prop"],decreasing=TRUE)

tab4=tab3[ord1,]
tab5=tab4[tab4[,"tot"]>150,]
tab5
  
mydat=f.long.problem(f.country.agg(f.stand.problem(f.filter(mydat2))))
mydat.original.scale=f.long.problem(f.country.agg(f.filter(mydat2)))


fit1=lm(Score~Country*(Year)+Gender,data=mydat)
fit2=lm(Score~Country*(Year+I(Year^2))+Gender,data=mydat)

anova(fit1,fit2)

summary(fit2)$coef["Gender",1]
summary(fit2)$adj.r.squared

(gender.effect=summary(fit2)$coef["Gender",1] * sd(mydat.original.scale$Score))

gender.effect/mean(mydat.original.scale$Score)*100


#################################################
#################################################

mydat=f.long.problem(f.stand.problem(f.filter(mydat2)))

mydatB=droplevels(subset(mydat,Gender!="Unknown" & !is.na(Gender)))

fit1=lm(Score~Country*(Year)+Gender*P.type2,data=mydatB) 
fit2a=lm(Score~Country*(Year+I(Year^2))+Gender+P.type2,data=mydatB)

fit2b=lm(Score~Country*(Year+I(Year^2))+Gender*P.type2,data=mydatB)

anova(fit1,fit2b) 
anova(fit2a,fit2b) 

sfit2b=summary(fit2b)
summary(fit2b)$adj.r.squared

myvars=unique(c(grep("Gender",dimnames(sfit2b$coef)[[1]]),grep("P.type",dimnames(sfit2b$coef)[[1]])))
sfit2b$coef[myvars,]


#####################
##### FIGURE 8 ######
#####################

dev.new(width=6, height=2.5, unit="in")

# compute lower and upper whiskers
ylim1 = boxplot.stats(mydatB$Score)$stats[c(1, 5)]

ggplot(mydatB, aes(x=Gender, y=Score, fill=P.type2))+    theme_ipsum(plot_margin=margin(t=2),base_family = "Comic Sans MS",plot_title_size=13)+coord_cartesian(ylim = ylim1*1.05)+ labs(fill=NULL)+ geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Standardized Scores By Problem Category and Gender")

ggsave("fig8.png",type="cairo",dpi=600)

#####################################
#####################################














