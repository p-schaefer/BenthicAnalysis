#load datasets
data(YKEnvData,envir = environment()) # Environmental dataset
data(YKBioData,envir = environment()) # Biological dataset

#Calculate summary Metrics
bio.data.test<-benth.met(YKBioData,2,2)
bio.data<-bio.data.test$Summary.Metrics

#scale data, log transform area, rename rows to match bio.data
YKEnvData[,6]<-log(YKEnvData[,6])
YKEnvData<-data.frame(apply(YKEnvData,2,scale))
rownames(YKEnvData)<-bio.data.test$Site.List


#Create output file
output<-data.frame(matrix(nrow=160,ncol=9))
colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p")
output$Site<-rownames(bio.data)[119:(119+159)]
output$Class<-c(rep("D0",40),rep("D1",40),rep("D2",40),rep("D3",40))

output<-rbind(output,output,output,output)
output$Analysis.type<-c(rep("Original",160),rep("Ad.Met.Sel",160),rep("Ad.Site.Sel",160),rep("Combined",160))

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
                         adaptive=if (n=="Ad.Site.Sel"|n=="Combined") {T} else {F})
    
    taxa.data<-cbind(bio.data[c(names(nn.sites$final.dist),i),],
                     add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),]))
    
    if (n=="Original"|n=="Ad.Site.Sel") {
      taxa.data<-taxa.data[,c("Shannon","HBI","O:E","Bray-Curtis")]
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
    #print(i)
    #print(n)
  }
}