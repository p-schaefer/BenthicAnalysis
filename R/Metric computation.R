#' Benthic Metric Calculation
#'
#' Calculation of a variety of metrics for determing impairment of Benthic Macroinvertebrate Communities.
#' @param x data.frame with sampling events in rows and taxa in columns. No row or column names should be defined
#' @param tax.fields The number of rows in x used for taxa names (will be concatenated in output)
#' @param site.fields The number of columns in x used for site names (will be concatenated in output)
#' @param HBI Custom sensitivity values for HBI calculation. Must follow format of data(HBI1,envir = environment())
#' @return $Summary.Metrics - Calculated indicator metrics
#' @return $Raw.Data - Raw taxon data
#' @return $Taxa.List - Concatenated taxon names
#' @return $Site.List - Concatenated site names
#' @keywords Benthic Metrics
#' @export
#' @examples
#' data(YKEnvData,envir = environment())
#' benth.met(bio.data.yk1,2,2)

benth.met<-function(x,tax.fields,site.fields,HBI=NULL) {
  if (is.null(HBI)) {
    data(HBI1,envir = environment())
    CEFI<-HBI[,c(3,6,7)]
    CEFI<-na.omit(CEFI)
    HBI<-HBI[,c(3,5)]
    HBI<-na.omit(HBI)
  } else {
    HBI<-data.frame(HBI)
  }

  if (site.fields>1){
    site.names<-apply(as.matrix(x[(tax.fields+1):nrow(x),1:site.fields]),1,FUN=paste0,collapse="",sep="-")# get site names
    site.names<-substr(site.names,start=1,stop=nchar(site.names)-1)
  } else if (site.fields==1){
    site.names<-x[(tax.fields+1):nrow(x),1]
  }

  taxa.names<-apply(as.matrix(x[1:tax.fields,(site.fields+1):ncol(x)]),2,FUN=paste0,collapse="",sep=";")# get taxa names
  taxa.names<-substr(taxa.names,start=1,stop=nchar(taxa.names)-1)

  taxa<-data.frame(x[(tax.fields+1):nrow(x),(site.fields+1):ncol(x)])
  if (nrow(x)-site.fields==1){
    taxa<-t(x.frame(apply(taxa,2,as.numeric)))
  } else {
    taxa<-data.frame(apply(taxa,2,as.numeric))
  }
  colnames(taxa)<-taxa.names
  rownames(taxa)<-site.names

  taxa.pa<-decostand(taxa,method="pa")
  taxa.rel<-sweep(taxa,rowSums(taxa),MARGIN=1,FUN="/")
  taxa.intol<-taxa[,taxa.names[grep(grep.paste(HBI[which(HBI[,2]<5),1]),taxa.names)]]
  n.taxa<-ncol(taxa)

  summ<-data.frame(matrix(nrow=nrow(taxa),ncol=26))
  rownames(summ)<-rownames(taxa)
  colnames(summ)<-c("Richness","Simpson","Shannon",
                    "Percent Dominance","Percent Oligochaeta",
                    "Percent Chironomidae","Percent Isopoda","Percent Amphipoda",

                    "Percent Coleoptera", "Trich as Hydropsychidae","Ephem as Baetidae",
                    "Intolerants Richness","Percent Intolerants",

                    "Percent EPT","EPT Richness",
                    "Ephem Richness","Percent Ephem",
                    "Plec Richness","Percent Plec",
                    "Trich Richness","Percent Trich",
                    "EPT per EPT and Chir","Percent Non Chir Dip","Percent CIGH","HBI","CEFI")


  summ[,1]<-specnumber(taxa)
  summ[,2]<-diversity(taxa,index="simpson")
  summ[,3]<-diversity(taxa,index="shannon")
  summ[,4]<-(apply(taxa, 1, max))/rowSums((taxa))
  summ[,5]<-adapt.sum(taxa[,grep(grep.paste(c("Oligochaetae","Oligochaeta","Oligochaete","Lumbriculida","Haplotaxida","Clitellata","Naidina")),colnames(taxa))])/rowSums(taxa)
  summ[,6]<-adapt.sum(taxa[,grep("Chironomidae",colnames(taxa))])/rowSums(taxa)
  summ[,7]<-log((adapt.sum(taxa[,grep("Isopoda",colnames(taxa))])/rowSums(taxa))+1)
  summ[,8]<-log((adapt.sum(taxa[,grep("Amphipoda",colnames(taxa))])/rowSums(taxa))+1)
  summ[,9]<-adapt.sum(taxa[,grep("Coleoptera",colnames(taxa))])/rowSums((taxa))
  summ[,10]<-adapt.sum(taxa[,grep("Hydropsychidae",colnames(taxa))])/adapt.sum(taxa[,grep("Trichoptera",colnames(taxa))])
  summ[,11]<-adapt.sum(taxa[,grep("Baetidae",colnames(taxa))])/adapt.sum(taxa[,grep("Ephemeroptera",colnames(taxa))])

  summ[,12]<-apply(taxa.intol, 1, function(x) length(which(x>0)))
  summ[,13]<-adapt.sum(taxa.intol)/rowSums(taxa)

  summ[,14]<-adapt.sum(taxa[,grep(paste0("Ephemeroptera|Plecoptera|Trichoptera"),colnames(taxa))])/rowSums(taxa)
  summ[,15]<-adapt.sum(taxa.pa[,grep(paste0("Ephemeroptera|Plecoptera|Trichoptera"),colnames(taxa.pa))])
  summ[,16]<-adapt.sum(taxa.pa[,grep("Ephemeroptera",colnames(taxa.pa))])
  summ[,17]<-adapt.sum(taxa[,grep("Ephemeroptera",colnames(taxa))])/rowSums(taxa)
  summ[,18]<-adapt.sum(taxa.pa[,grep("Plecoptera",colnames(taxa.pa))])
  summ[,19]<-adapt.sum(taxa[,grep("Plecoptera",colnames(taxa))])/rowSums(taxa)
  summ[,20]<-adapt.sum(taxa.pa[,grep("Trichoptera",colnames(taxa.pa))])
  summ[,21]<-adapt.sum(taxa[,grep("Trichoptera",colnames(taxa))])/rowSums(taxa)

  summ[,22]<-adapt.sum(taxa[,grep(paste0("Ephemeroptera|Plecoptera|Trichoptera"),colnames(taxa))])/
    (adapt.sum(taxa[,grep(paste0("Ephemeroptera|Plecoptera|Trichoptera"),colnames(taxa))])+
       adapt.sum(taxa[,grep("Chironomidae",colnames(taxa))]))
  summ[,23]<-1-(adapt.sum(taxa[,grep("Chironomidae",colnames(taxa))])/adapt.sum(taxa[,grep(paste0("Diptera"),colnames(taxa))]))
  
  summ[,24]<-adapt.sum(taxa[,grep("Corixidae|Hirudinea|Isopoda|Gastropoda",colnames(taxa))])/rowSums(taxa)

  if (tax.fields==2) {
    taxa.hbi<-taxa[,taxa.names[grep(grep.paste(HBI[,1]),taxa.names)]]
    t1<-lapply(colnames(taxa.hbi), function(x) substr(x, start=(gregexpr(pattern =';',x)[[1]][1]+1),stop=nchar(x))) #find matches between taxa.hbi and HBI in HBI
    t3<-match(t1,HBI[,1])
    if (any(is.na(t3))) {
      t2<-lapply(taxa.names[grep(grep.paste(t1[which(is.na(t3))]),taxa.names)], function(x) substr(x, stop=(gregexpr(pattern =';',x)[[1]][1]-1),start=1))
      t3[is.na(t3)]<-match(t2,HBI[,1])
    }
    summ[,25]<-apply(taxa.hbi,1,function(x) sum(x*HBI[t3, 2])/sum(x))
  }

  taxa.rel.cefi<-taxa.rel
  taxa.rel.cefi[taxa.rel.cefi<=0.05]<-0
  taxa.rel.cefi<-taxa.rel.cefi[,taxa.names[grep(grep.paste(CEFI[,1]),taxa.names)]]
  t1<-lapply(colnames(taxa.rel.cefi), function(x) substr(x, start=(gregexpr(pattern =';',x)[[1]][1]+1),stop=nchar(x))) #find matches between taxa.hbi and HBI in HBI
  t3<-match(t1,CEFI[,1])
  if (any(is.na(t3))) {
    t2<-lapply(taxa.names[grep(grep.paste(t1[which(is.na(t3))]),taxa.names)], function(x) substr(x, stop=(gregexpr(pattern =';',x)[[1]][1]-1),start=1))
    t3[is.na(t3)]<-match(t2,HBI[,1])
  }
  summ[,26]<-apply(taxa.rel.cefi,1,function(x) sum(x*CEFI[t3, 2]*CEFI[t3, 3])/sum(x*CEFI[t3, 3]))

  summ[is.nan.data.frame(summ)]<-0

  output<-NULL
  output$Summary.Metrics<-summ
  output$Raw.Data<-taxa
  output$Taxa.List<-taxa.names
  output$Site.List<-site.names
  class(output)<-"benth.metric"
  return(output)
}

#############################################################################################################
#' Reference specific Benthic Metric Calculation
#'
#' Calculation of a variety of metrics for determing impairment of Benthic Macroinvertebrate Communities, based on available Reference Condition information.
#' @param Test.taxa Output of \code{\link[BenthicAnalysis]{benth.met}}$Raw.Data for the test site
#' @param Reference.taxa Output of \code{\link[BenthicAnalysis]{benth.met}}$Raw.Data for the Reference site
#' @return Indicator metrics caluclated from \code{\link[BenthicAnalysis]{benth.met}} in addition to O:E ratios and Bray-Curtis Distance.
#' @keywords Benthic Metrics
#' @export
#' @examples
#' #load datasets
#' data(YKEnvData,envir = environment()) #Biological dataset
#' data(YKBioData,envir = environment()) #Environmental dataset
#'
#' #Calculate indicator metrics from raw biological data
#' bio.data.test<-benth.met(bio.data.yk1,2,2)
#'
#' #standardize row names between datasets
#' rownames(env.data.yk)<-rownames(bio.data.test$Site.List)
#'
#' #Match a test site (#201) to the nearest neighbour reference set
#' nn.sites<-site.match(env.data.yk[201,],env.data.yk[1:118,],k=F,adaptive=T)
#'
#' #Extract the raw taxa data for additional metric calculation
#' taxa.data<-rbind(bio.data.test$Raw.Data[names(nn.sites$final.dist),],bio.data.test$Raw.Data[rownames(bio.data[i,]),])
#'
#' #Calculate additional metrics based on nearest neighbour reference sites
#' additional.metrics<-add.met(Test=bio.data.test$Raw.Data[rownames(bio.data[201,]),],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),])
#' additional.metrics

add.met<-function (Test,Reference) {
  if (any(colnames(Test)%in%colnames(Reference)==F)){
    stop("Column name mismatch between Test and Reference Set")
  }
  
  raw.data<-rbind(Reference,Test)

  nRef<-nrow(Reference)

  pRef<-colSums(decostand(raw.data[rownames(Reference),],"pa"))/nrow(Reference)
  e<-adapt.sum1(pRef[names(which(pRef>=0.5))])
  e.var<-apply(decostand(raw.data[rownames(Reference),names(which(pRef>=0.5))],"pa"),1,function(x) adapt.sum1(x)/e)
  o<-adapt.sum1(decostand(raw.data[rownames(Test),names(which(pRef>=0.5))],"pa"))/e
  e.o<-c(e.var,o)
  e.o.stand<-(e.o-mean(e.o[1:nRef]))/sd(e.o[1:nRef])

  ref.bray<-rowMeans(braydist(raw.data[rownames(Reference),]))
  test.bray<-rowMeans(braydist(raw.data))[nrow(raw.data)]
  bray<-c(ref.bray,test.bray)
  bray.stand<-(bray-mean(bray[1:nRef]))/sd(bray[1:nRef])

  tax.names<-as.data.frame(t(matrix(c(lapply(colnames(raw.data), function(x) substr(x, start=1,stop=(gregexpr(pattern =';',x)[[1]][1]-1))),
                                      lapply(colnames(raw.data), function(x) substr(x, start=(gregexpr(pattern =';',x)[[1]][1]+1),stop=nchar(x)))),ncol=2)))
  colnames(tax.names)<-colnames(raw.data)
  raw.data<-rbind(tax.names,raw.data)
  raw.data<-cbind(rownames(raw.data),raw.data)
  
  ca.ord<-cca(log(rbind(Reference,Test)[,names(which(pRef>=0.1))]+1))
  #ca1<-c(ca.ord$CA$u[,1],predict(ca.ord,log(Test[,names(which(pRef>=0.1))]+1),type="wa")[1])
  #names(ca1)[nRef+1]<-rownames(Test)
  #ca2<-c(ca.ord$CA$u[,2],predict(ca.ord,log(Test[,names(which(pRef>=0.1))]+1),type="wa")[2])
  #names(ca2)[nRef+1]<-rownames(Test)
  ca1<-ca.ord$CA$u[,1]
  ca2<-ca.ord$CA$u[,2]
  
  raw.data<-cbind(benth.met(x=raw.data,tax.fields=2,site.fields=1)$Summary.Metrics,c(e.var,o),c(ref.bray,test.bray),ca1,ca2)
  colnames(raw.data)[(ncol(raw.data)-3):ncol(raw.data)]<-c("O:E","Bray-Curtis","CA1","CA2")

  return(raw.data)
}



##############################################################################################################

#' Indicator metric selection
#'
#' Determines which indicator metrics which best differentiate the test site from its nearest-neighbbour reference sites. Metrics that indicate impairment will be
#' used preferentially.
#'
#' A interative selection algorithm is used as follows:
#'
#' 1. The first metric selected for the final set is the one which displayes the greatest distance from the Reference condition mean
#'
#' 2. Metrics with a pearson correlation greater than 0.7 to (any of) the selected metric(s) are excluded from further steps
#'
#' 3. The ranked departure of remaining metrics is divided by the (maximum) correlation with the metric(s) previously included in the analysis
#'
#' 4. The metric with the greatest score is selected for inclusion in the final set
#'
#' 5. Return to step 2 until the number of selected metrics is equal to the greater of 4 or 1/5 the number of Reference sites
#'
#' If no metrics or too few metrics demonstrate impairment, the following metrics are included until the maximum is reached:
#' Richness, Percent Dominance, HBI, Percent EPT.
#'
#' @param Test Vector containing metric scores at the test site. Should be a single row from \code{benth.met} or \code{add.met}.
#' @param Reference Data frame of metric scores at the reference sites. Should be output from \code{benth.met} or \code{add.met}.
#' @param Rank Use rank differences in metric selection
#' @return $Best.Metrics - Vector containing the final selected indicator metrics
#' @return $Indicative.Metrics - Vector containing all metrics that indicate impairment
#' @keywords Benthic Metrics
#' @export
#' @examples
#' data(YKEnvData,envir = environment())
#' bio.data<-benth.met(bio.data.yk1,2,2)$Summary.Metrics
#' nn.refsites<- c("075-T-1", "019-T-1","003-T-1","076-T-1","071-T-1","022-T-1","074-T-1",
#' "002-T-1","004-T-1","073-T-1","186-T-1","062-T-1","005-T-1","025-T-1",
#' "187-T-1","023-T-1","193-T-1","192-T-1","196-T-1","194-T-1")
#' metric.select(bio.data[201,],bio.data[nn.refsites,])

metric.select <- function (Test,Reference,outlier.rem=T,rank=F) {
  raw.data1<-rbind(Reference,Test)
  raw.data<-tsa.zscore(Test=Test,Reference=Reference)
  raw.data[is.nan(as.matrix(raw.data))]<-0

  if (outlier.rem==T) {
    Reference<-Reference[,apply(Reference, 2, mad)!=0]
    Reference<-Reference[c(which(pcout(Reference,outbound=0.1)$wfinal01==1)),]
    raw.data<-tsa.zscore(Test[,colnames(Reference)],Reference)
  }

  nRef<-nrow(raw.data)-1
  nInd<-ncol(raw.data)
  test.var<-NULL

  hg<-c("Richness","Simpson","Shannon","Intolerants Richness","Percent Intolerants","Percent EPT","EPT Richness","Ephem Richness",
        "Percent Ephem","Plec Richness","Percent Plec","Trich Richness","Percent Trich","EPT per EPT and Chir",
        "Percent Non Chir Dip" , "O:E", "Bray-Curtis","CA1","CA2")

  hb<-c("Percent Dominance","Percent Oligochaeta","Percent Chironomidae","Percent Isopoda","Percent Amphipoda","FBI",
        "Trich as Hydropsychidae","Ephem as Baetidae","Percent Coleoptera","HBI", "Percent CIGH","CA1","CA2")

  lg<-c("Percent Dominance","Percent Oligochaeta","Percent Chironomidae","Percent Isopoda","Percent Amphipoda","FBI",
        "Trich as Hydropsychidae","Ephem as Baetidae","Percent Coleoptera", "HBI", "Percent CIGH","CA1","CA2")

  lb<-c("Richness","Simpson","Shannon","Intolerants Richness","Percent Intolerants","Percent EPT","EPT Richness","Ephem Richness",
        "Percent Ephem","Plec Richness","Percent Plec","Trich Richness","Percent Trich","EPT per EPT and Chir",
        "Percent Non Chir Dip", "O:E", "Bray-Curtis" ,"CA1","CA2")

  #This loops through each metric and removes metrics that have fewer than 25% unique variables
  restricted.metrics<-NULL
  for (i in 1:nInd) {
    if ((length(unique(raw.data[1:nRef,i]))>1) & !(max(table(raw.data[1:nRef,i]))>((1/3)*(nRef))) & (IQR(raw.data[1:nRef,i])>0)){ #& !has_warning(!has_error(invisible(covMcd(data[,i]))))){ #ceiling(nrow(tsa3)*0.1)
      restricted.metrics[i]<-paste0(colnames(raw.data)[i])
    } else {
      next
    }
  }
  data<-raw.data[,colnames(raw.data) %in% restricted.metrics]

  indicative.metrics<-c(hb[which(hb%in%colnames(data)[which(data[nrow(data),]>0)])],lb[which(lb%in%colnames(data)[which(data[nrow(data),]<0)])])
  indicative.metrics<-indicative.metrics[!duplicated(indicative.metrics)]
  reduced.data<-data[,colnames(data)%in%indicative.metrics]

  if (length(indicative.metrics)>1) {
    
    if (rank==T){
      diff<-length(reduced.data):1
    } else {
      diff<-sort(as.vector(reduced.data[nrow(reduced.data),]),decreasing=T)
    }
    names(diff)<-names(sort(abs(reduced.data[nrow(reduced.data),]),decreasing=T))
    test.var<-names(diff[1])
    cors<-abs(cor(reduced.data[1:nRef,],method="p")[,test.var])+0.001

    if (any(cors<0.7)) {
      cors<-cors[which(cors<0.7)]
      cors<-cors[(names(diff))]
      cors<-cors[which(!is.na(cors))]

      test.var[2]<-names(sort((diff[names(cors)]/cors),decreasing=T)[1])
      #[which(names(sort(abs(diff/cor(reduced.data[1:(nrow(reduced.data)-1),],method="k")[,names(test.var)]),decreasing=T))%in%(names(cor(reduced.data[1:(nrow(reduced.data)-1),],method="k")[,names(test.var)])[which(cor(reduced.data[1:(nrow(reduced.data)-1),],method="k")[,names(test.var)]<0.7)]))][1]

      for (var in 1:(min((length(indicative.metrics)-2),ceiling(1/5*nrow(data))-2))) {
        cors<-apply((abs(cor(reduced.data[1:nRef,],method="p")[,test.var])+0.001),1,max)
        if (any(cors<0.7)) {
          cors<-cors[which(cors<0.7)]
          cors<-cors[(names(diff))]
          cors<-cors[which(!is.na(cors))]
          test.var[var+2]<-names(sort((diff[names(cors)]/cors),decreasing=T)[1])
        } else {
          break
        }
      }
    }
  }

  if ((!is.null(test.var)) & (length(test.var)<max(4,ceiling(1/5*nrow(data))))){
    if (("O:E" %in% colnames(raw.data)) & !("O:E" %in% test.var)) {
      test.var<-c(test.var,"O:E")
    }
    if (("Percent Dominance" %in% colnames(raw.data)) & !("Percent Dominance" %in% test.var)) {
      test.var<-c(test.var,"Percent Dominance")
    }
    if (("Richness" %in% colnames(raw.data)) & !("Richness" %in% test.var)) {
      test.var<-c(test.var,"Richness")
    }
    if (("Percent EPT" %in% colnames(raw.data)) & !("Percent EPT" %in% test.var)) {
      test.var<-c(test.var,"Percent EPT")
    }

  }

  if (is.null(test.var)){
    if (length(indicative.metrics)==1) {
      test.var<-indicative.metrics
    } else {test.var<-NULL}
    if (("O:E" %in% colnames(data)) & !("O:E" %in% test.var)) {
      test.var<-c(test.var,"O:E")
    }
    if (("Percent Dominance" %in% colnames(data)) & !("Percent Dominance" %in% test.var)) {
      test.var<-c(test.var,"Percent Dominance")
    }
    if (("Richness" %in% colnames(data)) & !("Richness" %in% test.var)) {
      test.var<-c(test.var,"Richness")
    }
    if (("Percent EPT" %in% colnames(data)) & !("Percent EPT" %in% test.var)) {
      test.var<-c(test.var,"Percent EPT")
    }
  }

  test.var<-test.var[1:max(3,ceiling((1/5)*nRef))]
  test.var<-test.var[!is.na(test.var)]

  metric.auto<-NULL
  metric.auto$Best.Metrics<-test.var
  metric.auto$Indicative.Metrics<-indicative.metrics
  metric.auto$raw.data<-raw.data1[,colnames(raw.data1) %in% test.var]
  class(metric.auto)<-"met.sel"
  return(metric.auto)
}

print.met.sel<-function(met.sel){
  cat("Selected Metrics:\n")
  print(met.sel$Best.Metrics)
  cat("\n")
  cat("All indicative metrics:\n")
  print(met.sel$Indicative.Metrics)
}
