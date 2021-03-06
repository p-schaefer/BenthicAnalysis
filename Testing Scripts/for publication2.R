xpand <- function(d) do.call("expand.grid", rep(list(1:nrow(d)), 2))
euc_norm <- function(x) sqrt(sum(x^2))
euc_dist <- function(mat, weights=1) {
  iter <- xpand(mat)
  vec <- mapply(function(i,j) euc_norm(weights*(mat[i,] - mat[j,])), 
                iter[,1], iter[,2])
  matrix(vec,nrow(mat), nrow(mat))
}

format.pval(pv = pvalues, 
            
            # digits : number of digits, but after the 0.0
            digits = 2, 
            
            # eps = the threshold value above wich the 
            # function will replace the pvalue by "<0.0xxx"
            eps = 0.001, 
            
            # nsmall = how much tails 0 to keep if digits of 
            # original value < to digits defined
            nsmall = 3
)

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
p1<-rda(YKEnvData[YKEnvData$Ref>0,-c(1)])
p2<-predict(p1,newdata = YKEnvData[YKEnvData$Ref<0,-c(1)],type="wa",scaling=1)
plot(p1,display="sites",scaling=1)
points(x=p2[,1],y=p2[,2],pch=16)
#points(p1,display="species",scaling=1)
legend("topright",legend=c("Reference","Test"),pch=c(1,16))

#Romeve some environmental variables that add noise to the dataset
#YKEnvData<-YKEnvData[,-c(2,3,5,40,41,10:16)]

n.train<-length(which(YKBioData$V2==1))
n.test<-(nrow(YKBioData)-2)-n.train

#Create output file
output<-data.frame(matrix(nrow=n.test,ncol=9))
colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p")
output$Site<-rownames(bio.data)[(n.train+1):(nrow(YKBioData)-2)]
output$Class<-c(rep("D0",n.test/4),rep("D1",n.test/4),rep("D2",n.test/4),rep("D3",n.test/4))
output<-rbind(output,output)
output$Analysis.type<-c(rep("Original",n.test),rep("Ad.Site.Sel",n.test))

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
    nn.sites<-site.match(Test=YKEnvData[i,-c(1)], Reference=YKEnvData[1:n.train,-c(1)],
                         #RDA.ref=bio.data[c(rownames(bio.data)[1:n.train]),c("Percent.Dominance","Richness","Percent.ICHAEBO","Shannon")],
                         k=if (n=="Ad.Site.Sel") {NULL} else {30},
                         adaptive=if (n=="Ad.Site.Sel") {T} else {F},
                         ad.factor = 2, ad.constant = 1)
    
    taxa.data<-bio.data[c(names(nn.sites$final.dist),i),c("Percent.Dominance","Richness","Percent.ICHAEBO","Shannon")]
    
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
output<-data.frame(matrix(nrow=n.test,ncol=9))
colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p")
output$Site<-rownames(bio.data)[(n.train+1):(nrow(ACTBioData)-2)]
output$Class<-c(rep("D0",n.test/4),rep("D1",n.test/4),rep("D2",n.test/4),rep("D3",n.test/4))
output<-rbind(output,output)
output$Analysis.type<-c(rep("Original",n.test),rep("Ad.Site.Sel",n.test))

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
                         RDA.ref=bio.data[c(rownames(bio.data)[1:n.train]),c("Percent.Dominance","Richness","Percent.ICHAEBO","Shannon")],
                         k=if (n=="Ad.Site.Sel") {NULL} else {30},
                         adaptive=if (n=="Ad.Site.Sel") {T} else {F},
                         ad.factor = 1.8, ad.constant = 1)
    
    taxa.data<-bio.data[c(names(nn.sites$final.dist),i),c("Percent.Dominance","Richness","Percent.ICHAEBO","Shannon")]
    
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

######################################################################################################
#
#
#######################################################################################################


#load datasets
data(GLEnvData,envir = environment()) # Environmental dataset
data(GLBioData,envir = environment()) # Biological dataset

GLEnvData<-GLEnvData[-c(grep("1216",rownames(GLEnvData))),]
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
GLEnvData<-GLEnvData[,-c(21)]
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
output<-data.frame(matrix(nrow=n.test,ncol=9))
colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p")
output$Site<-rownames(bio.data)[(n.train+1):(nrow(GLBioData)-2)]
output$Class<-c(rep("D0",n.test/4),rep("D1",n.test/4),rep("D2",n.test/4),rep("D3",n.test/4))
output<-rbind(output,output)
output$Analysis.type<-c(rep("Original",n.test),rep("Ad.Site.Sel",n.test))

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
                         RDA.ref=bio.data[c(rownames(bio.data)[1:n.train]),c("Percent.Dominance","Richness","Percent.ICHAEBO","Shannon")],
                         k=if (n=="Ad.Site.Sel") {NULL} else {30},
                         adaptive=if (n=="Ad.Site.Sel") {T} else {F},
                         ad.factor = 2, ad.constant = 1)
    
    taxa.data<-bio.data[c(names(nn.sites$final.dist),i),c("Percent.Dominance","Richness","Percent.ICHAEBO","Shannon")]
    
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


##########################################################################################
#
#
##########################################################################################

test<-data.frame(matrix(nrow=441,ncol=2))
test[,1]<-rep(c(-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),times=21)
test[,2]<-rep(c(-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),each=21)

distance=unique(sort(sqrt(0.7*(0-test[,1])^2 + 0.3*(0-test[,2])^2)))
dist.dif<-sort(unique(diff(distance)[-c(1:3)]))
plot(1:30,dist.dif[1:30])
points(1:30,(1*(1/((3:32)^(2)))))
points(1:30,exp(-(1:30)))
#distance<-dist(test, method = "euclidean")

##########################################################################################
#habitat and bio distance
##########################################################################################


yk1<-bio.data.test$Raw.Data
yk1<-yk1[1:158,]
#yk1<-yk1[,apply(yk1,2,function(x)length(which(x>1)))>5]
#yk1<-yk1[,colSums(yk1)>5]

#M<-data.frame(cor(cbind(yk1,yk2)))
M<-(cor(cbind(yk1,yk2),method="spearman"))
M1<-as.data.frame(as.table(M))
M1<-M1[M1$Var1%in%colnames(yk1),]
M1<-M1[M1$Var2%in%colnames(yk2),]

M2<-M1[order(abs(M1$Freq),decreasing = T),]
M2<-M2[abs(M2$Freq)>0.3,]
unique(M2$Var2)

#yk2<-YKEnvData[,colnames(YKEnvData)%in%as.character(unique(M2$Var2))]
yk2<-YKEnvData
yk2<-scale(yk2[1:158,])
habitat.dist1<-dist(yk2)
habitat.dist2.1<-vegan::rda(yk2,scale=T)
habitat.dist2<-habitat.dist2.1$CA$u[,1:4]
habitat.dist2<-decostand(habitat.dist2,method="range")
habitat.dist2<-as.dist(euc_dist(habitat.dist2,weights=(eigenvals(habitat.dist2.1)/sum(eigenvals(habitat.dist2.1)))[1:4]))

#habitat.dist2<-habitat.dist2*(eigenvals(habitat.dist2.1)[1:ncol(habitat.dist2)]/sum(eigenvals(habitat.dist2.1)[1:ncol(habitat.dist2)]))
#habitat.dist2<-dist(habitat.dist2) 
 

M<-M[1:ncol(yk1),(ncol(yk1)+1):ncol(M)]
corrplot::corrplot(M, method = "number")

bio.dist1<-dist(decostand(yk1,method="hellinger"))
bio.dist1.1<-vegan::rda(decostand(yk1,method="hellinger"),scale=T)
bio.dist1<-bio.dist1.1$CA$u[,1:4]
bio.dist1<-decostand(bio.dist1,method="range")
bio.dist1<-as.dist(euc_dist(bio.dist1,weights=(eigenvals(bio.dist1.1)/sum(eigenvals(bio.dist1.1)))[1:4]))

bio.dist2<-vegan::vegdist(yk1)

lm1<-lm(dist(habitat.dist2.1$CA$u[,1:2])~dist(bio.dist1.1$CA$u[,1:2]))
pro1<-protest(habitat.dist2.1,bio.dist1.1,permutations=10000)
man1<-mantel(bio.dist2,habitat.dist2,method="pearson", permutations = 9999)


#plot(protest(bio.dist1.1,habitat.dist2.1))
smoothScatter(dist(bio.dist1.1$CA$u[,1:2]),dist(habitat.dist2.1$CA$u[,1:2]),cex=0.1,col="grey",main="YK",xlab="Biological Distance",ylab="Habitat Distance")
abline(lm1)
text(0.4,0.55,bquote(R^2==.(round(man1$statistic, 2))))
text(0.4,0.6,paste0("p = ",(round(pro1$signif, 3))))



summary(lm(bio.dist1~habitat.dist2))
summary(lm(bio.dist2~habitat.dist2))

summary(lm(bio.dist1~habitat.dist1))
summary(lm(bio.dist2~habitat.dist1))

smoothScatter(habitat.dist2,bio.dist2)
scatter.smooth(habitat.dist2,bio.dist2,degree=2,cex=0.1,col="grey")

##########################################################################################
#habitat and bio distance1
##########################################################################################
#data(c("YKBioData","GLBioData","ACTBioData","YKEnvData","GLEnvData","ACTEnvData"))
GLEnvData<-GLEnvData[-c(grep("1216",rownames(GLEnvData))),]
GLBioData<-GLBioData[-c(grep("1216",GLBioData$V1)),]
GLEnvData<-GLEnvData[,-c(21)]

bio.data<-list(YKBioData,GLBioData,ACTBioData)
env.data<-list(YKEnvData,GLEnvData,ACTEnvData)
plot.main<-c("YK","GL","ACT")



for (i in 1:3){
  bio.data.test<-BenthicAnalysis::benth.met(bio.data[[i]],2,2)

  yk1<-bio.data.test$Raw.Data
#site.max<-max(grep("D0",rownames(yk1)))
  
  yk1<-yk1[grep("D0|T",rownames(yk1)),]
  yk1<-yk1[,apply(yk1,2,function(x)length(which(x>1)))>5]
  yk1<-yk1[,colSums(yk1)>5]
  
  bio.dist1<-dist(decostand(yk1,method="hellinger"))
  bio.dist1.1<-vegan::rda(decostand(yk1,method="hellinger"),scale=T)
  bio.dist1<-bio.dist1.1$CA$u[,1:4]
  bio.dist1<-decostand(bio.dist1,method="range")
  bio.dist1<-as.dist(euc_dist(bio.dist1,weights=(eigenvals(bio.dist1.1)/sum(eigenvals(bio.dist1.1)))[1:4]))
  bio.dist2<-vegan::vegdist(yk1)

  yk2<-env.data[[i]]
  yk2<-scale(yk2[grep("D0|T",rownames(yk2)),])
  habitat.dist1<-dist(yk2)
  habitat.dist2.1<-vegan::rda(yk2,scale=T)
  habitat.dist2<-habitat.dist2.1$CA$u[,1:4]
  habitat.dist2<-decostand(habitat.dist2,method="range")
  habitat.dist2<-as.dist(euc_dist(habitat.dist2,weights=(eigenvals(habitat.dist2.1)/sum(eigenvals(habitat.dist2.1)))[1:4]))
  
  lm1<-lm(dist(habitat.dist2.1$CA$u[,1:2])~dist(bio.dist1.1$CA$u[,1:2]))
  #pro1<-protest(habitat.dist2.1,bio.dist1.1,permutations=10000)
  man1<-mantel(dist(bio.dist1.1$CA$u[,1:2]),dist(habitat.dist2.1$CA$u[,1:2]),method="pearson", permutations = 9999)
  
  x.max<-max(dist(bio.dist1.1$CA$u[,1:2]))
  y.max<-max(dist(habitat.dist2.1$CA$u[,1:2]))
  
  #plot(protest(bio.dist1.1,habitat.dist2.1))
  smoothScatter(dist(bio.dist1.1$CA$u[,1:2]),dist(habitat.dist2.1$CA$u[,1:2])
                ,cex=0.1,col="grey",main=plot.main[i],xlab="Biological Distance",ylab="Habitat Distance")
  abline(lm1)
  text(x.max-0.1,y.max-0.05,bquote(R^2==.(round(man1$statistic, 3))))
  text(x.max-0.1,y.max-0.11,paste0("p = ",format.pval(pv = man1$signif,
                                                      digits = 2, 
                                                      eps = 0.001,
                                                      nsmall = 3
  )))

}

