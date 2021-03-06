write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

derivative <- function(f, x, ..., order = 1, delta = 0.1, sig = 6) {
  # Numerically computes the specified order derivative of f at x
  vals <- matrix(NA, nrow = order + 1, ncol = order + 1)
  grid <- seq(x - delta/2, x + delta/2, length.out = order + 1)
  vals[1, ] <- sapply(grid, f, ...) - f(x, ...)
  for (i in 2:(order + 1)) {
    for (j in 1:(order - i + 2)) {
      stepsize <- grid[i + j - 1] - grid[i + j - 2]
      vals[i, j] <- (vals[i - 1, j + 1] - vals[i - 1, j])/stepsize
    }
  }
  return(signif(vals[order + 1, 1], sig))
}

g<-function(x,z=1,b=2){
  return(z/(x^b))
}


textplot2 <- function(x, 
                      y, 
                      words, 
                      cex = 1, 
                      pch = 16, 
                      pointcolor = "red", 
                      new = TRUE,
                      show.lines=TRUE, 
                      ...){
  if(new)
    plot(x,y,type="n",...)
  lay <- wordlayout(x,y,words,cex,...)
  if(show.lines){
    for(i in 1:length(x)){
      xl <- lay[i,1]
      yl <- lay[i,2]
      w <- lay[i,3]
      h <- lay[i,4]
      if(x[i]<xl || x[i]>xl+w ||
         y[i]<yl || y[i]>yl+h){
        points(x[i],y[i],pch= pch,col= pointcolor,cex= .5)
        nx <- xl+.5*w
        ny <- yl+.5*h
        lines(c(x[i],nx),c(y[i],ny),col="grey")
      }
    }
  }
  text(lay[,1]+.5*lay[,3],lay[,2]+.5*lay[,4],words,cex = cex,...)
}

#load datasets
data(YKEnvData,envir = environment()) # Environmental dataset
data(YKBioData,envir = environment()) # Biological dataset

#Calculate summary Metrics
bio.data.test<-benth.met(YKBioData,2,2)
bio.data<-bio.data.test$Summary.Metrics

#Transform remaining metrics to approximate normality
bio.data[,grep("Richness",colnames(bio.data))]<-log(bio.data[,grep("Richness",colnames(bio.data))]+1)
bio.data[,grep("Percent",colnames(bio.data))]<-car::logit(bio.data[,grep("Percent",colnames(bio.data))])
bio.data<-bio.data[,-c(5,7:10,11,20,21,24,28)]


#scale data, log transform area, rename rows to match bio.data
YKEnvData[,6]<-log(YKEnvData[,6])
YKEnvData<-data.frame(apply(YKEnvData,2,scale))
rownames(YKEnvData)<-bio.data.test$Site.List

#PLotting


#Romeve some environmental variables that add noise to the dataset
#YKEnvData<-YKEnvData[,-c(2,3,5,40,41,10:16)]

n.train<-length(which(YKBioData$V2==1))
n.test<-(nrow(YKBioData)-2)-n.train

#Create output file
output<-data.frame(matrix(nrow=n.test,ncol=18))
colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p",
                    "M1","M2","M3","M4","M5","M6","M7","M8","M9")
output$Site<-rownames(bio.data)[(n.train+1):(nrow(YKBioData)-2)]
output$Class<-c(rep("D0",n.test/4),rep("D1",n.test/4),rep("D2",n.test/4),rep("D3",n.test/4))
output<-rbind(output,output,output,output)
output$Analysis.type<-c(rep("Original",n.test),rep("Ad.Met.Sel",n.test),rep("Ad.Site.Sel",n.test),rep("Combined",n.test))

nn.sites<-BenthicAnalysistesting::site.matchUI(Test=YKEnvData[119:nrow(YKEnvData),-c(1)],
                                     Reference=YKEnvData[1:118,-c(1)])

library(ggplot2)
for(i in 1:length(119:nrow(YKEnvData))){
  site<-nn.sites$distance.matrix[i,][order(nn.sites$distance.matrix[i,])]#[-c(1:2)]
  g<-function(x,z=1,b=2){
    return((z/(x^b)))
  }
  d1<-data.frame(x=site,y=log(g(site)))
  d2<-seq(from=min(site),to=max(site),length.out = length(site))
  der2<-sapply(site,derivative,f=g,order=1,delta=0.01)
  cusu<-cumsum(abs(der2))/sum(abs(der2))
  
  
  #d1<-data.frame(n=1:length(site),Dist=site,H0=1/(1:length(site)))[-c(1),]
  #d2<-data.frame(apply(d1[,2:3],2,diff))
  #d2$n<-1:nrow(d2)+1
  #plot(d2$n,d2$Dist,main=i)
  #points(d2$n,d2$H0,col="red")
  
  thresh1<-max(max(log(g(d2))))-(max(log(g(d2)))-min(log(g(d2))))*2/3
  thresh.x<-sqrt(1/exp(thresh1))
  
  #thresh.d2<-site[max(which(cusu<=0.95))]
  final<-site[(which(site<=thresh.x))]
  print(length(final))
  #if(length(site[site<thresh.d2])<10){
  #  stop()
  #}
  #if(length(site[site<thresh.d2])>50){
  #  stop()
  #}
  
  #plot(d1,main=i)
  #plot(d2,log(g(d2)),main=i)
  #plot(d2,sapply(log(g(d2)),derivative,f=g,order=1,delta=0.01),main=i)
  
  hull<-nn.sites$ordination.scores[nn.sites$ordination.scores$Class=="Reference",]
  hull$Ref<-F
  hull$Ref[rownames(hull)%in%names(final)]<-T
  hull<-hull[hull$Ref==T,]
  hull<-hull[chull(hull[,c("PC1","PC2")]),]
  
  test.site<-nn.sites$ordination.scores[rownames(nn.sites$ordination.scores)%in%rownames(nn.sites$distance.matrix)[i],]
  
  reference.sites<-nn.sites$ordination.scores[nn.sites$ordination.scores$Class=="Reference",]
  reference.sites$Ref<-F
  reference.sites$Ref[rownames(reference.sites)%in%names(final)]<-T
  reference.sites<-reference.sites[reference.sites$Ref==T,]
  
  p1 <- ggplot(data=nn.sites$ordination.scores,aes(x=PC1, y=PC2)) + 
    geom_vline(xintercept = 0, color="darkgrey") + geom_hline(yintercept = 0, color="darkgrey") +
    geom_point(aes(color=Class))  + theme_bw() + labs(title=paste0("Nearest-neighbour Ordination of site ",i))
    
  p1<- p1 + geom_polygon(data=hull[,c("PC1","PC2")],alpha=0.5)
  p1<- p1 + geom_text(data=reference.sites[,c("PC1","PC2")], label=rownames(reference.sites))
  p1<- p1 + geom_point(data=test.site[,c("PC1","PC2")],size=3)
  print(p1)

}

#What if you include D0 in the training set?
#output<-data.frame(matrix(nrow=120,ncol=9))
#colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p")
#output$Site<-rownames(bio.data)[159:278]
#output$Class<-c(rep("D1",40),rep("D2",40),rep("D3",40))
#output<-rbind(output,output,output,output)
#output$Analysis.type<-c(rep("Original",120),rep("Ad.Met.Sel",120),rep("Ad.Site.Sel",120),rep("Combined",120))


#Computational loop
#
#What if you include D0 in the training set?
#Removal of potentially noisy environmental data?
#YKEnvData<-YKEnvData[,-c(2,3,5,40,41,10:16)]
#
for (i in unique(output$Site)){
  for (n in unique(output$Analysis.type)){
    nn.sites<-site.match(Test=YKEnvData[i,-c(1)], Reference=YKEnvData[1:118,-c(1)],
                         k=if (n=="Ad.Site.Sel"|n=="Combined") {NULL} else {30},
                         adaptive=if (n=="Ad.Site.Sel"|n=="Combined") {T} else {F},
                         ad.factor = 2)
    
    taxa.data<-cbind(bio.data[c(names(nn.sites$final.dist),i),],
                     add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),]))
    
    if (n=="Original"|n=="Ad.Site.Sel") {
      taxa.data<-taxa.data[,c("Percent.Dominance","Richness","O:E","Bray-Curtis")]
    }
    
    tsa.result<-tsa.test(Test=taxa.data[nrow(taxa.data),],
                         Reference=taxa.data[1:(nrow(taxa.data)-1),],
                         distance=NULL,
                         outlier.rem=F,
                         m.select= if (n=="Ad.Met.Sel"|n=="Combined") {T} else {F},
                         rank=F,
                         na.cutoff=0.7,
                         outbound=0.1)
    
    output[(output$Site==i & output$Analysis.type==n),3:9]<-c(tsa.result$tsa.results[5,1],
                                  tsa.result$tsa.results[1,1],
                                  tsa.result$general.results[3,1],
                                  tsa.result$general.results[5,1],
                                  tsa.result$general.results[6,1],
                                  tsa.result$jacknife[1,1],
                                  tsa.result$tsa.results[4,1])
    
    if (n=="Combined"|n=="Ad.Met.Sel") {
      output[(output$Site==i & output$Analysis.type==n),10:(9+length(tsa.result$selected.metrics))]<-tsa.result$selected.metrics
    }
  }
}

output2<-data.frame(matrix(nrow=(n.test),ncol=10))
colnames(output2)<-c("Original","Ad.Met.Sel","Ad.Site.Sel","Combined","Sites","Class",
                     "Original.er",
                     "Ad.Met.Sel.er",
                     "Ad.Site.Sel.er",
                     "Combined.er")
output2$Sites<-unique(output$Site)
output2$Class<-c(rep("D0",n.test/4),rep("D1",n.test/4),rep("D2",n.test/4),rep("D3",n.test/4))

output2$Original<-output$Impair.rank[output$Analysis.type=="Original"]
output2$Ad.Met.Sel<-output$Impair.rank[output$Analysis.type=="Ad.Met.Sel"]
output2$Ad.Site.Sel<-output$Impair.rank[output$Analysis.type=="Ad.Site.Sel"]
output2$Combined<-output$Impair.rank[output$Analysis.type=="Combined"]

output2$Original.er[output2$Class=="D0" & output2$Original=="Impaired"]<-1
output2$Ad.Met.Sel.er[output2$Class=="D0" & output2$Ad.Met.Sel=="Impaired"]<-1
output2$Ad.Site.Sel.er[output2$Class=="D0" & output2$Ad.Site.Sel=="Impaired"]<-1
output2$Combined.er[output2$Class=="D0" & output2$Combined=="Impaired"]<-1

output2$Original.er[output2$Class%in%c("D1","D2","D3") & output2$Original=="Not Impaired"]<-1
output2$Ad.Met.Sel.er[output2$Class%in%c("D1","D2","D3") & output2$Ad.Met.Sel=="Not Impaired"]<-1
output2$Ad.Site.Sel.er[output2$Class%in%c("D1","D2","D3") & output2$Ad.Site.Sel=="Not Impaired"]<-1
output2$Combined.er[output2$Class%in%c("D1","D2","D3") & output2$Combined=="Not Impaired"]<-1

errors<-data.frame(matrix(nrow=4,ncol=4))
rownames(errors)<-unique(output$Analysis.type)
colnames(errors)<-c("D0","D1","D2","D3")
errors$D0<-colSums(output2[output2$Class=="D0",7:10],na.rm = T)/(n.test/4)
errors$D1<-colSums(output2[output2$Class=="D1",7:10],na.rm = T)/(n.test/4)
errors$D2<-colSums(output2[output2$Class=="D2",7:10],na.rm = T)/(n.test/4)
errors$D3<-colSums(output2[output2$Class=="D3",7:10],na.rm = T)/(n.test/4)
errors

mets.used<-data.frame(matrix(nrow=n.test,ncol=4))
colnames(mets.used)<-c("Site","D1","D2","D3")
mets.used[,1]<-unique(output$Site)[1:(n.test/4)]
mets.used[,2]<-apply(output[output$Analysis.type=="Combined" & output$Class=="D1",10:18],1,function(x)paste(x[!is.na(x)], collapse=","))
mets.used[,3]<-apply(output[output$Analysis.type=="Combined" & output$Class=="D2",10:18],1,function(x)paste(x[!is.na(x)], collapse=","))
mets.used[,4]<-apply(output[output$Analysis.type=="Combined" & output$Class=="D3",10:18],1,function(x)paste(x[!is.na(x)], collapse=","))


output[,10:18]<-apply(output[,10:18],2,function(x) as.numeric(factor(x=x,levels=c(colnames(bio.data),"O:E","Bray-Curtis","CA1","CA2"))))
output$met.numbs[output$Analysis.type=="Combined"]<-apply(output[output$Analysis.type=="Combined",10:18],1,function(x)paste(x[!is.na(x)],collapse=","))

for (i in sort(unique(unlist(output[,10:18])))){
  eval(parse(text=paste0("output$gr",i,"<-NA")))
  eval(parse(text=paste0("output$gr",i,"<-apply(output[,10:18],1,function(x) any(x==i))")))
  #eval(parse(text=paste0("output$gr",i,"[is.na(output$gr",i,")]<-0")))
  eval(parse(text=paste0("output$gr",i,"[output$gr",i,"==T]<-1")))
}

sort(colSums(output[output$Analysis.type=="Combined" & output$Class=="D0",21:ncol(output)],na.rm = T))

p1<-rda(YKEnvData[YKEnvData$Ref>0,-c(1)])
t1<-plot(p1,scaling=1)

p2<-predict(p1,newdata = YKEnvData[YKEnvData$Ref<0,-c(1)][1:(n.test/4),],type="wa",scaling=1)
plot(p1,display="sites",scaling=1,cex=0.3,pch=16,col="black",type="n",
     main="YT Site Matching",
     xlab=paste0("PC1 (",signif(p1$CA$eig[1]/sum(p1$CA$eig)*100,4),"%)"),
     ylab=paste0("PC2 (",signif(p1$CA$eig[2]/sum(p1$CA$eig)*100,4),"%)"))#,
     #ylim=c(-0.20,0.20))
#plot(x=p1$CA$u[,1],y=p1$CA$u[,2],cex=0.5,pch=16,
#     main="YK Number of Metrics",
#     xlab=paste0("PC1 (",signif(p1$CA$eig[1]/sum(p1$CA$eig)*100,4),"%)"),
#     ylab=paste0("PC2 (",signif(p1$CA$eig[2]/sum(p1$CA$eig)*100,4),"%)"),
#     ylim=c(-0.20,0.20))
points(x=t1$sites[,1],y=t1$sites[,2],pch=16,cex=0.6,col="darkgrey")
points(x=p2[,1],y=p2[,2],pch=16,cex=0.8,col="black")
#points(p1,display="species",scaling=1)
legend("topright",legend=c("Reference","Test"),pch=c(16,16),pt.cex=c(0.6,0.8),col=c("darkgrey","black"))
for(i in colnames(output)[21:ncol(output)]){
  ordiellipse(ord=p2,groups=as.factor(output[output$Analysis.type=="Combined" & output$Class=="D3",i]),
              scaling=1,display="sites",kind="sd")
}
#text(x=p2[,1],y=p2[,2],pos=2, cex=1.0,col="white",
#     labels=output[output$Analysis.type=="Combined" & output$Class=="D0","n.Ref"])
#text(x=p2[,1],y=p2[,2],pos=2, cex=0.7,
#     labels=output[output$Analysis.type=="Combined" & output$Class=="D0","n.Ref"])
#text(x=p2[,1],y=p2[,2],pos=2, cex=0.7,
#     labels=output[output$Analysis.type=="Combined" & output$Class=="D3","met.numbs"])
library(wordcloud)
textplot(x=p2[1:40,1],
         y=p2[1:40,2],
         new=F,cex=0.8,
         words=output[output$Analysis.type=="Combined" & output$Class=="D3","n.Ref"])

textplot2(x=p2[1:40,1]+0.05,
         y=p2[1:40,2],
         new=F,cex=0.8,
         words=output[output$Analysis.type=="Combined" & output$Class=="D3","n.Ref"],pointcolor = "black")

for (i in sort(unique(unlist(output[,10:18])))){
  eval(parse(text=paste0("output$gr",i,"<-NA")))
  eval(parse(text=paste0("output$gr",i,"<-apply(output[,10:18],1,function(x) any(x==i))")))
  eval(parse(text=paste0("output$gr",i,"[is.na(output$gr",i,")]<-0")))
  eval(parse(text=paste0("output$gr",i,"[output$gr",i,"==T]<-1")))
}


sig.t<-NA
for(i in colnames(output)[21:ncol(output)]){
  rhs<-ordiareatest(ord=p2,groups=as.factor(output[output$Analysis.type=='Combined' & output$Class=='D3',i]),area='ellipse',permutations=999,kind = "sd")
  eval(parse(text=paste0("sig.t$",i,"<-",rhs$pvalues[2])))
}

library(fpc)
p1<-pamk(output[,21:52],krange=(1:10))
p2<-cascadeKM(output[,21:52],1,10)


#Create data table with env and bio distances for all pairwise combinations
library(cluster)

p1<-rda(scale(YKEnvData[1:(n.train+(n.test/4)),-c(1)]))
#p2<-rda(scale(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),colSums(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),])>(ncol(bio.data.test$Raw.Data)*0.1)]))

env.dist<-daisy(p1$CA$u[,1:4], metric="euclidean",weights=(p1$CA$eig/sum(p1$CA$eig))[1:4])
#env.dist<-dist(p1$CA$u)
#bio.dist<-dist(p2$CA$u)
bio.dist<-vegdist(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),colSums(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),])>(ncol(bio.data.test$Raw.Data)*0.1)],method="bray")

m <- data.frame(t(combn(rownames(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),]),2)), as.numeric(bio.dist),as.numeric(env.dist))

smoothScatter(m$as.numeric.bio.dist,m$as.numeric.env.dist, main="YT",xlab="Biological Distance", ylab="Habitat Distance")
abline(lm(m$as.numeric.env.dist~m$as.numeric.bio.dist),lwd=2)
##############################################################################################################################
#
#
##############################################################################################################################

#load datasets
data(ACTEnvData,envir = environment()) # Environmental dataset
data(ACTBioData,envir = environment()) # Biological dataset

#Calculate summary Metrics
bio.data.test<-benth.met(ACTBioData,2,2)
bio.data<-bio.data.test$Summary.Metrics

#Transform remaining metrics to approximate normality
bio.data[,grep("Richness",colnames(bio.data))]<-log(bio.data[,grep("Richness",colnames(bio.data))]+1)
bio.data[,grep("Percent",colnames(bio.data))]<-logit(bio.data[,grep("Percent",colnames(bio.data))])
bio.data<-bio.data[,-c(7:10,11,24:36)]


#scale data, log transform area, rename rows to match bio.data
ACTEnvData[,7]<-log(ACTEnvData[,7])
ACTEnvData<-data.frame(apply(ACTEnvData,2,scale))
rownames(ACTEnvData)<-bio.data.test$Site.List

p1<-rda(ACTEnvData[ACTEnvData$Ref>0,-c(1)])
p2<-predict(p1,newdata = ACTEnvData[ACTEnvData$Ref<0,-c(1)],type="wa",scaling=1)
plot(p1,display="sites",scaling=1)
points(x=p2[,1],y=p2[,2],pch=16)
#points(p1,display="species",scaling=1)
legend("topright",legend=c("Reference","Test"),pch=c(1,16))

#Romeve some environmental variables that add noise to the dataset
#YKEnvData<-YKEnvData[,-c(2,3,5,40,41,10:16)]

n.train<-length(which(ACTBioData$V2==1))
n.test<-(nrow(ACTBioData)-2)-n.train

#Create output file
output<-data.frame(matrix(nrow=n.test,ncol=18))
colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p",
                    "M1","M2","M3","M4","M5","M6","M7","M8","M9")
output$Site<-rownames(bio.data)[(n.train+1):(nrow(ACTBioData)-2)]
output$Class<-c(rep("D0",n.test/4),rep("D1",n.test/4),rep("D2",n.test/4),rep("D3",n.test/4))
output<-rbind(output,output,output,output)
output$Analysis.type<-c(rep("Original",n.test),rep("Ad.Met.Sel",n.test),rep("Ad.Site.Sel",n.test),rep("Combined",n.test))

#What if you include D0 in the training set?
#output<-data.frame(matrix(nrow=120,ncol=9))
#colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p")
#output$Site<-rownames(bio.data)[159:278]
#output$Class<-c(rep("D1",40),rep("D2",40),rep("D3",40))
#output<-rbind(output,output,output,output)
#output$Analysis.type<-c(rep("Original",120),rep("Ad.Met.Sel",120),rep("Ad.Site.Sel",120),rep("Combined",120))


#Computational loop
#
#What if you include D0 in the training set?
#Removal of potentially noisy environmental data?
#YKEnvData<-YKEnvData[,-c(2,3,5,40,41,10:16)]
#
for (i in unique(output$Site)){
  for (n in unique(output$Analysis.type)){
    nn.sites<-site.match(Test=ACTEnvData[i,-c(1)], Reference=ACTEnvData[1:n.train,-c(1)],
                         k=if (n=="Ad.Site.Sel"|n=="Combined") {NULL} else {30},
                         adaptive=if (n=="Ad.Site.Sel"|n=="Combined") {T} else {F},
                         ad.factor = 2)
    
    taxa.data<-cbind(bio.data[c(names(nn.sites$final.dist),i),],
                     add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),]))
    
    if (n=="Original"|n=="Ad.Site.Sel") {
      taxa.data<-taxa.data[,c("Percent.Dominance","Richness","O:E","Bray-Curtis")]
    }
    
    tsa.result<-tsa.test(Test=taxa.data[nrow(taxa.data),],
                         Reference=taxa.data[1:(nrow(taxa.data)-1),],
                         distance=NULL,
                         outlier.rem=F,
                         m.select= if (n=="Ad.Met.Sel"|n=="Combined") {T} else {F},
                         rank=F,
                         na.cutoff=0.7,
                         outbound=0.1)
    
    output[(output$Site==i & output$Analysis.type==n),3:9]<-c(tsa.result$tsa.results[5,1],
                                                              tsa.result$tsa.results[1,1],
                                                              tsa.result$general.results[3,1],
                                                              tsa.result$general.results[5,1],
                                                              tsa.result$general.results[6,1],
                                                              tsa.result$jacknife[1,1],
                                                              tsa.result$tsa.results[4,1])
    if (n=="Combined"|n=="Ad.Met.Sel") {
      output[(output$Site==i & output$Analysis.type==n),10:(9+length(tsa.result$selected.metrics))]<-tsa.result$selected.metrics
    }
    
  }
}

output2<-data.frame(matrix(nrow=(n.test),ncol=10))
colnames(output2)<-c("Original","Ad.Met.Sel","Ad.Site.Sel","Combined","Sites","Class",
                     "Original.er",
                     "Ad.Met.Sel.er",
                     "Ad.Site.Sel.er",
                     "Combined.er")
output2$Sites<-unique(output$Site)
output2$Class<-c(rep("D0",n.test/4),rep("D1",n.test/4),rep("D2",n.test/4),rep("D3",n.test/4))

output2$Original<-output$Impair.rank[output$Analysis.type=="Original"]
output2$Ad.Met.Sel<-output$Impair.rank[output$Analysis.type=="Ad.Met.Sel"]
output2$Ad.Site.Sel<-output$Impair.rank[output$Analysis.type=="Ad.Site.Sel"]
output2$Combined<-output$Impair.rank[output$Analysis.type=="Combined"]

output2$Original.er[output2$Class=="D0" & output2$Original=="Impaired"]<-1
output2$Ad.Met.Sel.er[output2$Class=="D0" & output2$Ad.Met.Sel=="Impaired"]<-1
output2$Ad.Site.Sel.er[output2$Class=="D0" & output2$Ad.Site.Sel=="Impaired"]<-1
output2$Combined.er[output2$Class=="D0" & output2$Combined=="Impaired"]<-1

output2$Original.er[output2$Class%in%c("D1","D2","D3") & output2$Original=="Not Impaired"]<-1
output2$Ad.Met.Sel.er[output2$Class%in%c("D1","D2","D3") & output2$Ad.Met.Sel=="Not Impaired"]<-1
output2$Ad.Site.Sel.er[output2$Class%in%c("D1","D2","D3") & output2$Ad.Site.Sel=="Not Impaired"]<-1
output2$Combined.er[output2$Class%in%c("D1","D2","D3") & output2$Combined=="Not Impaired"]<-1

errors<-data.frame(matrix(nrow=4,ncol=4))
rownames(errors)<-unique(output$Analysis.type)
colnames(errors)<-c("D0","D1","D2","D3")
errors$D0<-colSums(output2[output2$Class=="D0",7:10],na.rm = T)/(n.test/4)
errors$D1<-colSums(output2[output2$Class=="D1",7:10],na.rm = T)/(n.test/4)
errors$D2<-colSums(output2[output2$Class=="D2",7:10],na.rm = T)/(n.test/4)
errors$D3<-colSums(output2[output2$Class=="D3",7:10],na.rm = T)/(n.test/4)
errors

mets.used<-data.frame(matrix(nrow=n.test,ncol=4))
colnames(mets.used)<-c("Site","D1","D2","D3")
mets.used[,1]<-unique(output$Site)[1:(n.test/4)]
mets.used[,2]<-apply(output[output$Analysis.type=="Combined" & output$Class=="D1",10:18],1,function(x)paste(x[!is.na(x)], collapse=","))
mets.used[,3]<-apply(output[output$Analysis.type=="Combined" & output$Class=="D2",10:18],1,function(x)paste(x[!is.na(x)], collapse=","))
mets.used[,4]<-apply(output[output$Analysis.type=="Combined" & output$Class=="D3",10:18],1,function(x)paste(x[!is.na(x)], collapse=","))


output[,10:18]<-apply(output[,10:18],2,function(x) as.numeric(factor(x=x,levels=c(colnames(bio.data),"O:E","Bray-Curtis","CA1","CA2"))))
output$met.numbs[output$Analysis.type=="Combined"]<-apply(output[output$Analysis.type=="Combined",10:18],1,function(x)paste(x[!is.na(x)],collapse=","))
for (i in sort(unique(unlist(output[,10:18])))){
  eval(parse(text=paste0("output$gr",i,"<-NA")))
  eval(parse(text=paste0("output$gr",i,"<-apply(output[,10:18],1,function(x) any(x==i))")))
  eval(parse(text=paste0("output$gr",i,"[is.na(output$gr",i,")]<-0")))
  eval(parse(text=paste0("output$gr",i,"[output$gr",i,"==T]<-1")))
}

sort(colSums(output[output$Analysis.type=="Combined" & output$Class=="D0",21:ncol(output)],na.rm = T))

p1<-rda(ACTEnvData[ACTEnvData$Ref>0,-c(1)])
t1<-plot(p1,scaling=1)

p2<-predict(p1,newdata = ACTEnvData[ACTEnvData$Ref<0,-c(1)][1:(n.test/4),],type="wa",scaling=1)
plot(p1,display="sites",scaling=1,cex=0.3,pch=16,col="black",type="n",
     main="ACT Site Matching",
     xlab=paste0("PC1 (",signif(p1$CA$eig[1]/sum(p1$CA$eig)*100,4),"%)"),
     ylab=paste0("PC2 (",signif(p1$CA$eig[2]/sum(p1$CA$eig)*100,4),"%)"))#,
#ylim=c(-0.20,0.20))
#plot(x=p1$CA$u[,1],y=p1$CA$u[,2],cex=0.5,pch=16,
#     main="YK Number of Metrics",
#     xlab=paste0("PC1 (",signif(p1$CA$eig[1]/sum(p1$CA$eig)*100,4),"%)"),
#     ylab=paste0("PC2 (",signif(p1$CA$eig[2]/sum(p1$CA$eig)*100,4),"%)"),
#     ylim=c(-0.20,0.20))
points(x=t1$sites[,1],y=t1$sites[,2],pch=16,cex=0.6,col="darkgrey")
points(x=p2[,1],y=p2[,2],pch=16,cex=0.8,col="black")
#points(p1,display="species",scaling=1)
legend("topright",legend=c("Reference","Test"),pch=c(16,16),pt.cex=c(0.6,0.8),col=c("darkgrey","black"))
for(i in colnames(output)[21:ncol(output)]){
  ordiellipse(ord=p2,groups=as.factor(output[output$Analysis.type=="Combined" & output$Class=="D3",i]),
              scaling=1,display="sites",kind="sd")
}
#text(x=p2[,1],y=p2[,2],pos=2, cex=1.0,col="white",
#     labels=output[output$Analysis.type=="Combined" & output$Class=="D0","n.Ref"])
#text(x=p2[,1],y=p2[,2],pos=2, cex=0.7,
#     labels=output[output$Analysis.type=="Combined" & output$Class=="D0","n.Ref"])
#text(x=p2[,1],y=p2[,2],pos=2, cex=0.7,
#     labels=output[output$Analysis.type=="Combined" & output$Class=="D3","met.numbs"])
library(wordcloud)
textplot(x=p2[1:20,1],
         y=p2[1:20,2],
         new=F,cex=0.8,
         words=output[output$Analysis.type=="Combined" & output$Class=="D3","met.numbs"])

textplot2(x=p2[1:20,1],
          y=p2[1:20,2]+0.05,
          new=F,cex=0.8,
          words=output[output$Analysis.type=="Combined" & output$Class=="D3","n.Ref"],pointcolor = "black")

#Create data table with env and bio distances for all pairwise combinations
library(cluster)

p1<-rda(scale(ACTEnvData[1:(n.train+(n.test/4)),-c(1)]))
#p2<-rda(scale(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),colSums(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),])>(ncol(bio.data.test$Raw.Data)*0.1)]))

env.dist<-daisy(p1$CA$u[,1:3], metric="euclidean",weights=(p1$CA$eig/sum(p1$CA$eig))[1:3])
#env.dist<-dist(p1$CA$u)
#bio.dist<-dist(p2$CA$u)
bio.dist<-vegdist(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),colSums(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),])>(ncol(bio.data.test$Raw.Data)*0.1)],method="bray")

m <- data.frame(t(combn(rownames(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),]),2)), as.numeric(bio.dist),as.numeric(env.dist))

smoothScatter(m$as.numeric.bio.dist,m$as.numeric.env.dist, main="ACT",xlab="Biological Distance", ylab="Habitat Distance")
abline(lm(m$as.numeric.env.dist~m$as.numeric.bio.dist),lwd=2)

######################################################################################################
#
#
#######################################################################################################


#load datasets
data(GLEnvData,envir = environment()) # Environmental dataset
data(GLBioData,envir = environment()) # Biological dataset

GLEnvData<-GLEnvData[-c(grep("1216",rownames(GLEnvData))),-c(21)]
GLBioData<-GLBioData[-c(grep("1216",GLBioData$V1)),]

#Calculate summary Metrics
bio.data.test<-benth.met(GLBioData,2,2)

bio.data.test$Raw.Data<-round(bio.data.test$Raw.Data)
bio.data.test$Raw.Data<-rbind(bio.data.test$Raw.Data[grep("T",rownames(bio.data.test$Raw.Data)),],
                                     bio.data.test$Raw.Data[grep("D0",rownames(bio.data.test$Raw.Data)),],
                                     bio.data.test$Raw.Data[grep("D1",rownames(bio.data.test$Raw.Data)),],
                                     bio.data.test$Raw.Data[grep("D2",rownames(bio.data.test$Raw.Data)),],
                                     bio.data.test$Raw.Data[grep("D3",rownames(bio.data.test$Raw.Data)),])

bio.data.test$Summary.Metrics<-rbind(bio.data.test$Summary.Metrics[grep("T",rownames(bio.data.test$Summary.Metrics)),],
                                     bio.data.test$Summary.Metrics[grep("D0",rownames(bio.data.test$Summary.Metrics)),],
                                     bio.data.test$Summary.Metrics[grep("D1",rownames(bio.data.test$Summary.Metrics)),],
                                     bio.data.test$Summary.Metrics[grep("D2",rownames(bio.data.test$Summary.Metrics)),],
                                     bio.data.test$Summary.Metrics[grep("D3",rownames(bio.data.test$Summary.Metrics)),])






bio.data<-bio.data.test$Summary.Metrics

#Transform remaining metrics to approximate normality
bio.data[,grep("Richness",colnames(bio.data))]<-log(bio.data[,grep("Richness",colnames(bio.data))]+1)
bio.data[,grep("Percent",colnames(bio.data))]<-logit(bio.data[,grep("Percent",colnames(bio.data))])
bio.data<-bio.data[,-c(5,7:12,14,15:27)]


#scale data, rename rows to match bio.data
GLEnvData<-rbind(GLEnvData[grep("T",rownames(GLEnvData)),],
                                     GLEnvData[grep("D0",rownames(GLEnvData)),],
                                     GLEnvData[grep("D1",rownames(GLEnvData)),],
                                     GLEnvData[grep("D2",rownames(GLEnvData)),],
                                     GLEnvData[grep("D3",rownames(GLEnvData)),])

GLEnvData<-data.frame(apply(GLEnvData,2,scale))
rownames(GLEnvData)<-rownames(bio.data.test$Summary.Metrics)

p1<-rda(GLEnvData[GLEnvData$Ref>0,-c(1)])
p2<-predict(p1,newdata = GLEnvData[GLEnvData$Ref<0,-c(1)],type="wa",scaling=1)
plot(p1,display="sites",scaling=1)
points(x=p2[,1],y=p2[,2],pch=16)
#points(p1,display="species",scaling=1)
legend("topright",legend=c("Reference","Test"),pch=c(1,16))

#Romeve some environmental variables that add noise to the dataset
#YKEnvData<-YKEnvData[,-c(2,3,5,40,41,10:16)]

n.train<-length(which(GLBioData$V2==1))
n.test<-(nrow(GLBioData)-2)-n.train

#Create output file
output<-data.frame(matrix(nrow=n.test,ncol=18))
colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p",
                    "M1","M2","M3","M4","M5","M6","M7","M8","M9")
output$Site<-rownames(bio.data)[(n.train+1):(nrow(GLBioData)-2)]
output$Class<-c(rep("D0",n.test/4),rep("D1",n.test/4),rep("D2",n.test/4),rep("D3",n.test/4))
output<-rbind(output,output,output,output)
output$Analysis.type<-c(rep("Original",n.test),rep("Ad.Met.Sel",n.test),rep("Ad.Site.Sel",n.test),rep("Combined",n.test))

#What if you include D0 in the training set?
#output<-data.frame(matrix(nrow=120,ncol=9))
#colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p")
#output$Site<-rownames(bio.data)[159:278]
#output$Class<-c(rep("D1",40),rep("D2",40),rep("D3",40))
#output<-rbind(output,output,output,output)
#output$Analysis.type<-c(rep("Original",120),rep("Ad.Met.Sel",120),rep("Ad.Site.Sel",120),rep("Combined",120))


#Computational loop
#
#What if you include D0 in the training set?
#Removal of potentially noisy environmental data?
#YKEnvData<-YKEnvData[,-c(2,3,5,40,41,10:16)]
#
for (i in unique(output$Site)){
  for (n in unique(output$Analysis.type)){
    nn.sites<-site.match(Test=GLEnvData[i,-c(1)], Reference=GLEnvData[1:n.train,-c(1)],
                         k=if (n=="Ad.Site.Sel"|n=="Combined") {NULL} else {30},
                         adaptive=if (n=="Ad.Site.Sel"|n=="Combined") {T} else {F},
                         ad.factor = 2)
    
    taxa.data<-cbind(bio.data[c(names(nn.sites$final.dist),i),],
                     add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),]))
    
    if (n=="Original"|n=="Ad.Site.Sel") {
      taxa.data<-taxa.data[,c("Percent.Dominance","Richness","O:E","Bray-Curtis")]
    }
    
    tsa.result<-tsa.test(Test=taxa.data[nrow(taxa.data),],
                         Reference=taxa.data[1:(nrow(taxa.data)-1),],
                         distance=NULL,
                         outlier.rem=F,
                         m.select= if (n=="Ad.Met.Sel"|n=="Combined") {T} else {F},
                         rank=F,
                         na.cutoff=0.7,
                         outbound=0.1)
    
    output[(output$Site==i & output$Analysis.type==n),3:9]<-c(tsa.result$tsa.results[5,1],
                                                              tsa.result$tsa.results[1,1],
                                                              tsa.result$general.results[3,1],
                                                              tsa.result$general.results[5,1],
                                                              tsa.result$general.results[6,1],
                                                              tsa.result$jacknife[1,1],
                                                              tsa.result$tsa.results[4,1])
    if (n=="Combined"|n=="Ad.Met.Sel") {
      output[(output$Site==i & output$Analysis.type==n),10:(9+length(tsa.result$selected.metrics))]<-tsa.result$selected.metrics
    }
    
  }
}

output2<-data.frame(matrix(nrow=(n.test),ncol=10))
colnames(output2)<-c("Original","Ad.Met.Sel","Ad.Site.Sel","Combined","Sites","Class",
                     "Original.er",
                     "Ad.Met.Sel.er",
                     "Ad.Site.Sel.er",
                     "Combined.er")
output2$Sites<-unique(output$Site)
output2$Class<-c(rep("D0",n.test/4),rep("D1",n.test/4),rep("D2",n.test/4),rep("D3",n.test/4))

output2$Original<-output$Impair.rank[output$Analysis.type=="Original"]
output2$Ad.Met.Sel<-output$Impair.rank[output$Analysis.type=="Ad.Met.Sel"]
output2$Ad.Site.Sel<-output$Impair.rank[output$Analysis.type=="Ad.Site.Sel"]
output2$Combined<-output$Impair.rank[output$Analysis.type=="Combined"]

output2$Original.er[output2$Class=="D0" & output2$Original=="Impaired"]<-1
output2$Ad.Met.Sel.er[output2$Class=="D0" & output2$Ad.Met.Sel=="Impaired"]<-1
output2$Ad.Site.Sel.er[output2$Class=="D0" & output2$Ad.Site.Sel=="Impaired"]<-1
output2$Combined.er[output2$Class=="D0" & output2$Combined=="Impaired"]<-1

output2$Original.er[output2$Class%in%c("D1","D2","D3") & output2$Original=="Not Impaired"]<-1
output2$Ad.Met.Sel.er[output2$Class%in%c("D1","D2","D3") & output2$Ad.Met.Sel=="Not Impaired"]<-1
output2$Ad.Site.Sel.er[output2$Class%in%c("D1","D2","D3") & output2$Ad.Site.Sel=="Not Impaired"]<-1
output2$Combined.er[output2$Class%in%c("D1","D2","D3") & output2$Combined=="Not Impaired"]<-1

errors<-data.frame(matrix(nrow=4,ncol=4))
rownames(errors)<-unique(output$Analysis.type)
colnames(errors)<-c("D0","D1","D2","D3")
errors$D0<-colSums(output2[output2$Class=="D0",7:10],na.rm = T)/(n.test/4)
errors$D1<-colSums(output2[output2$Class=="D1",7:10],na.rm = T)/(n.test/4)
errors$D2<-colSums(output2[output2$Class=="D2",7:10],na.rm = T)/(n.test/4)
errors$D3<-colSums(output2[output2$Class=="D3",7:10],na.rm = T)/(n.test/4)
errors


mets.used<-data.frame(matrix(nrow=n.test,ncol=4))
colnames(mets.used)<-c("Site","D1","D2","D3")
mets.used[,1]<-unique(output$Site)[1:(n.test/4)]
mets.used[,2]<-apply(output[output$Analysis.type=="Combined" & output$Class=="D1",10:18],1,function(x)paste(x[!is.na(x)], collapse=","))
mets.used[,3]<-apply(output[output$Analysis.type=="Combined" & output$Class=="D2",10:18],1,function(x)paste(x[!is.na(x)], collapse=","))
mets.used[,4]<-apply(output[output$Analysis.type=="Combined" & output$Class=="D3",10:18],1,function(x)paste(x[!is.na(x)], collapse=","))


output[,10:18]<-apply(output[,10:18],2,function(x) as.numeric(factor(x=x,levels=c(colnames(bio.data),"O:E","Bray-Curtis","CA1","CA2"))))
output$met.numbs[output$Analysis.type=="Combined"]<-apply(output[output$Analysis.type=="Combined",10:18],1,function(x)paste(x[!is.na(x)],collapse=","))
for (i in sort(unique(unlist(output[,10:18])))){
  eval(parse(text=paste0("output$gr",i,"<-NA")))
  eval(parse(text=paste0("output$gr",i,"<-apply(output[,10:18],1,function(x) any(x==i))")))
  eval(parse(text=paste0("output$gr",i,"[is.na(output$gr",i,")]<-0")))
  eval(parse(text=paste0("output$gr",i,"[output$gr",i,"==T]<-1")))
}

sort(colSums(output[output$Analysis.type=="Combined" & output$Class=="D0",21:ncol(output)],na.rm = T))

p1<-rda(GLEnvData[GLEnvData$Ref>0,-c(1)])
t1<-plot(p1,scaling=1)

p2<-predict(p1,newdata = GLEnvData[GLEnvData$Ref<0,-c(1)][1:(n.test/4),],type="wa",scaling=1)
plot(p1,display="sites",scaling=1,cex=0.3,pch=16,col="black",type="n",
     main="GL Site Matching",
     xlab=paste0("PC1 (",signif(p1$CA$eig[1]/sum(p1$CA$eig)*100,4),"%)"),
     ylab=paste0("PC2 (",signif(p1$CA$eig[2]/sum(p1$CA$eig)*100,4),"%)"))#,
#ylim=c(-0.20,0.20))
#plot(x=p1$CA$u[,1],y=p1$CA$u[,2],cex=0.5,pch=16,
#     main="YK Number of Metrics",
#     xlab=paste0("PC1 (",signif(p1$CA$eig[1]/sum(p1$CA$eig)*100,4),"%)"),
#     ylab=paste0("PC2 (",signif(p1$CA$eig[2]/sum(p1$CA$eig)*100,4),"%)"),
#     ylim=c(-0.20,0.20))
points(x=t1$sites[,1],y=t1$sites[,2],pch=16,cex=0.6,col="darkgrey")
points(x=p2[,1],y=p2[,2],pch=16,cex=0.8,col="black")
#points(p1,display="species",scaling=1)
legend("topright",legend=c("Reference","Test"),pch=c(16,16),pt.cex=c(0.6,0.8),col=c("darkgrey","black"))
for(i in colnames(output)[21:ncol(output)]){
  ordiellipse(ord=p2,groups=as.factor(output[output$Analysis.type=="Combined" & output$Class=="D3",i]),
              scaling=1,display="sites",kind="sd")
}
#text(x=p2[,1],y=p2[,2],pos=2, cex=1.0,col="white",
#     labels=output[output$Analysis.type=="Combined" & output$Class=="D0","n.Ref"])
#text(x=p2[,1],y=p2[,2],pos=2, cex=0.7,
#     labels=output[output$Analysis.type=="Combined" & output$Class=="D0","n.Ref"])
#text(x=p2[,1],y=p2[,2],pos=2, cex=0.7,
#     labels=output[output$Analysis.type=="Combined" & output$Class=="D3","met.numbs"])
library(wordcloud)
textplot(x=p2[1:40,1],
         y=p2[1:40,2],
         new=F,cex=0.8,
         words=output[output$Analysis.type=="Combined" & output$Class=="D3","met.numbs"])

textplot2(x=p2[1:40,1]+0.05,
          y=p2[1:40,2],
          new=F,cex=0.8,
          words=output[output$Analysis.type=="Combined" & output$Class=="D3","n.Ref"],pointcolor = "black")


#Create data table with env and bio distances for all pairwise combinations
library(cluster)

p1<-rda(scale(GLEnvData[1:(n.train+(n.test/4)),-c(1)]))
#p2<-rda(scale(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),colSums(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),])>(ncol(bio.data.test$Raw.Data)*0.1)]))

env.dist<-daisy(p1$CA$u[,1:4], metric="euclidean",weights=(p1$CA$eig/sum(p1$CA$eig))[1:4])
#env.dist<-dist(p1$CA$u)
#bio.dist<-dist(p2$CA$u)
bio.dist<-vegdist(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),colSums(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),])>(ncol(bio.data.test$Raw.Data)*0.1)],method="bray")

m <- data.frame(t(combn(rownames(bio.data.test$Raw.Data[1:(n.train+(n.test/4)),]),2)), as.numeric(bio.dist),as.numeric(env.dist))

smoothScatter(m$as.numeric.bio.dist,m$as.numeric.env.dist, main="YT",xlab="Biological Distance", ylab="Habitat Distance")
abline(lm(m$as.numeric.env.dist~m$as.numeric.bio.dist),lwd=2)

##########################################################################################
#
#
##########################################################################################

test<-data.frame(matrix(nrow=441,ncol=2))
test[,1]<-rep(seq(from=-1,to=1,by=0.1),times= 21)
test[,2]<-rep(seq(from=-1,to=1,by=0.1),each= 21)

distance=unique(signif(sort(sqrt(0.7*(0-test[,1])^2 + 0.3*(0-test[,2])^2)),3))

cummean.dist.dif<-cumsum(distance) / seq_along(distance)

dist.dif<-unique(diff(distance))
plot(1:30,cum.dist.dif[1:30], main="Distance Decay", xlab="Ranked Distance",ylab="Iterated Weighted Euchlidean Distance",type="p")
scatter.smooth(1:30,dist.dif[1:30])
lines(predict(loess(I(1*(1/((3:32)^(2))))~I(1:30))))
points(3:32,(1*(2/((3:32)^(0.5)))))
points(1:30,exp(-(1:30)))
#distance<-dist(test, method = "euclidean")
