##########################################
#### FINAL PROJECT FOR BIOINFORMATICS ####
##########################################
#Group Members:
#
#####ggplot2 is he best for quick, clear analysis of data #####
library(ggplot2)

######### my attempt at stuff #########
#files will be put in a github as part of rubrick, path will be:
# 
seed <- data.frame(read.csv("seedling_data_sep_DeadvAlive.csv")) 
seed
str(seed)
head(seed)
tail(seed)
Dead <- seed$S_Count[which(seed$DvA == "DEAD")]
Alive <- seed$S_Count[which(seed$DvA == "ALIVE")]

#phenotype by seed count
qplot(Pheno,S_Count, data = seed, color = DvA) #interesting! 
qplot(Pheno,S_Count, data = seed, facets = .~DvA)# interesting!
qplot(Pheno,S_Count, data = seed, facets = .~DvA)+ geom_smooth(method = lm)
#Genotype by seed count
qplot(Geno,S_Count, data = seed, color = DvA)
qplot(Geno,S_Count, data = seed, facets = .~DvA)
qplot(Geno,log(S_Count), data = seed, color = DvA)

#total by seedcount
qplot(SC_Total, data = seed, color = Pheno) #bad, does not show data well
qplot(SC_Total,S_Count, data = seed, color = DvA)
qplot(SC_Total,log(S_Count), data = seed, color = DvA)
qplot(SC_Total,S_Count, data = seed, facets = .~DvA)

#total by Genotype
qplot(SC_Total, data = seed, color = Geno) 
qplot(SC_Total,Geno, data = seed, color = DvA)
qplot(SC_Total,Geno, data = seed, facets = .~DvA)

#total by Phenotype
qplot(SC_Total, data = seed, color = Pheno) 
qplot(SC_Total,Pheno, data = seed, color = DvA)
qplot(SC_Total,Pheno, data = seed, facets = .~DvA)

qplot(SC_Total, data = seed, color = Pheno) #shows distribution of samples!
qplot(S_Count,SC_STDEV_WT, data = seed, color = DvA)
qplot(log(SC_Total),log(SC_STDEV_WT), data = seed, color = DvA)
qplot(SC_Total,S_Count, data = seed, facets = .~DvA)
qplot(log(SC_Total),log(seed$SC_STDEV_WT), data = seed, facets = .~DvA)


#examples of using ggplot2 for 2 factor box plotting
head(df)
ggplot(aes(y = boxthis, x = f2, fill = f1), data = df) + geom_boxplot()

#using ggplot2 for 2 factor box plotting of the standard DEV from WT 
head(seed)
seed$DVA_Pheno <- interaction(seed$DvA, seed$Pheno)
seed$DVA_Geno <- interaction(seed$DvA, seed$Geno)

ggplot(aes(y = SC_STDEV_WT, x = DVA_Pheno), data = seed) + geom_boxplot()
ggplot(aes(y = SC_STDEV_WT, x = DVA_Geno), data = seed) + geom_boxplot()
ggplot(aes(y = SC_STDEV_WT, x = DvA, fill = Geno), data = seed) + geom_boxplot()
ggplot(aes(y = SC_STDEV_WT, x = DvA, fill = Pheno), data = seed) + geom_boxplot()

ggplot(aes(y = SC_STDEV_Tot, x = DVA_Pheno), data = seed) + geom_boxplot()
ggplot(aes(y = SC_STDEV_Tot, x = DVA_Geno), data = seed) + geom_boxplot()
ggplot(aes(y = SC_STDEV_Tot, x = DvA, fill = Geno), data = seed) + geom_boxplot()
ggplot(aes(y = SC_STDEV_Tot, x = DvA, fill = Pheno), data = seed) + geom_boxplot()

ggplot(aes(y = S_Count, x = DVA_Pheno), data = seed) + geom_boxplot()
ggplot(aes(y = S_Count, x = DVA_Geno), data = seed) + geom_boxplot()
ggplot(aes(y = S_Count, x = DvA, fill = Geno), data = seed) + geom_boxplot()
ggplot(aes(y = S_Count, x = DvA, fill = Pheno), data = seed) + geom_boxplot() #!

qplot(y = S_Count, x = SC_Total, data = seed,facets = .~DvA) + geom_smooth(method = lm)#!!

?ggplot
#random data example....
df <- data.frame(f1=factor(rbinom(100, 1, 0.45), label=c("WT","HZM")), 
                 f2=factor(rbinom(100, 1, 0.45), label=c("Alive","Dead")),
                 boxthis=rnorm(100))
df
df$f1f2 <- interaction(df$f1, df$f2)

#?ggplot
ggplot(aes(y = boxthis, x = f1f2), data = df) + geom_boxplot()
ggplot(aes(y = boxthis, x = f2, fill = f1), data = df) + geom_boxplot()

# to use ggplot2 I need to manipulate the data.frame better to show my output. 
#i need to simplify my data.frame to suit the plot I wish to make
str(mpg) #data.frame that comes with ggplot2
str(seed)   #my data.frame
qplot(displ, hwy, data = mpg) #simple example of qplot
qplot(displ, hwy, data = mpg, color = drv) #diligniates color by the factors in mpg$drv 
qplot(displ, hwy, data = mpg, geom = c("point","smooth"))
# a statistic = a summary of the data
#geom arguments "points show the data, "smooth shows the confidence of the data for the best fit line
qplot(hwy, data = mpg, fill = drv) 
# single variables will create a histogram in qplot
#facets
qplot(displ, hwy, data = mpg, facets = .~drv) 
qplot(hwy, data = mpg, facets = drv~.,binwith = 2)

#scatter plot 
qplot(log(displ), log(hwy), data = mpg, color = drv) 
qplot(log(displ), log(hwy), data = mpg, color = drv) + geom_smooth(method = lm)
#facets argument will split the overlay 
qplot(log(displ), log(hwy), data = mpg, facets = .~drv) + geom_smooth(method = lm)
# facets varible is directional (right or left) .~column or Row~. 
#this is determined by the side the ~ is placed 
#?ggplot
ggplot(aes(y = n$S_Count, x = f1f2), data = n) + geom_boxplot()
ggplot(aes(y = boxthis, x = f2, fill = f1), data = df) + geom_boxplot()

stop("running stop")
#attempt at boxplot for data 1
boxplot(df$boxthis ~ df$f2,            # x variable, y variable
        notch = F,                                     # Draw notch
        las = 1,                                       # Orientate the axis tick labels
        xlab = "Generation 1",                             # X-axis label
        ylab = "Seed Count",                         # Y-axis label
        main = "Phenotype Segrigation by Seed Count",  # Plot title
        cex.lab = 1.5,                                 # Size of axis labels
        cex.axis = 1.5,                                # Size of the tick mark labels
        cex.main = 2)                                  # Size of the plot title

stop("running stop")
##### talk with pleuni 12/07/17######
plot(1,1,col=0,xlim = c(.5, 12.5), ylim = c(0,200),
     xlab = "Generation", ylab = "Seed Count", main = "Phenotype Segrigation by Seed Count")
    points(n$total[which(n$Pheno == "WT")])
    points(n$total[which(n$Pheno == "HZM")])
    points(n$total[which(n$Pheno == "HE")])
    points(n$total[which(n$Pheno == "HZM/HE")])
    
    value = points(n$total[which(n$Pheno == "WT")])
    rep(1,length(yvalue))
n
n$alive[which(n$genotype=="HZM")]
n$alive[which(n$genotype=="HE")]
n$alive[which(n$genotype=="HZM/HE")]
        
dfx <- read.csv("11AM_Influ_team/seedling_data.csv")
str(dfx)
head(dfx)
df$Pheno
dfx
stop("running stop")
####################### test #######################
n <- read.csv("11AM_Influ_team/seedling_data.csv")

# look at n, how does the data.frame pan out. i.e. what are the col vs row within. 
str(n)
head(n)
plot(c(n$alive, n$dead))
n$genotype
which(n$genotype=="MODEL")
which(n$genotype=="HE")
n$alive[which(n$genotype=="MODEL")]
n$alive[which(n$genotype=="WT")]
n$alive[which(n$genotype=="HZM")]
n$alive[which(n$genotype=="HE")]
n$alive[which(n$genotype=="HZM/HE")]

n$dead[which(n$genotype=="MODEL")]
n$dead[which(n$genotype=="WT")]
n$alive[which(n$genotype=="HZM")]
n$alive[which(n$genotype=="HE")]
n$alive[which(n$genotype=="HZM/HE")]



plot(x = c(n$alive, n$dead), #plot it!
             xlab = "Generation", ylab = "Seed Count", main = "Phenotype Segrigation by Seed Count",
             col = (as.integer(n$makesCpG))
)

stop("running stop")
#########function for fig 3 #################
Fig3<-function(dfx, virusname){
    #begin plot func tion for figure 3; ends on line 651
    #  df<-dffin3
    library(ggplot2)
    library(plyr)
    library(grid)
    library(scales)
    library(gridExtra)
    CSV<-dfx
    
    
    #This portion establishes plotting order => ggplot will plot things in aplhabetic order. The selections of data from the incoming data.frame will be tagged a thru r.
    
    ##SYN SITES (LEFT GRAPH)
    #all green points for the left synonomous site grapha
    
    ##SYN SITES (LEFT GRAPH)
    #all green points for the left synonomous site graphs
    a <- frequenciesOfSynAmutsNONCP <- CSV[which(((CSV$TypeOfSite == "syn"  )) & (CSV$wtnt == "a" & (CSV$bigAAchange == "0") &(CSV$makesCpG == "0"))),"MeanFreq"]
    summary(a)
    c <- frequenciesOfSynAmutsNONCP <- CSV[which(((CSV$TypeOfSite == "syn"  )) & (CSV$wtnt == "t" & (CSV$bigAAchange == "0") &(CSV$makesCpG == "0"))),"MeanFreq"]
    e <- frequenciesOfSynAmutsNONCP <- CSV[which(((CSV$TypeOfSite == "syn"  )) & (CSV$wtnt == "c" & (CSV$bigAAchange == "0") &(CSV$makesCpG == "0"))),"MeanFreq"]
    f <- frequenciesOfSynAmutsNONCP <- CSV[which(((CSV$TypeOfSite == "syn"  )) & (CSV$wtnt == "g" & (CSV$bigAAchange == "0") &(CSV$makesCpG == "0"))),"MeanFreq"]
    
    #blue dots of left graph that show a CpG-forming mutation
    b <- frequenciesOfSynAmutsCP <- CSV[which(((CSV$TypeOfSite == "syn"  )) & (CSV$wtnt == "a") & (CSV$bigAAchange == "0") & (CSV$makesCpG == "1")),"MeanFreq"]
    d <- frequenciesOfSynTmutsCP <- CSV[which(((CSV$TypeOfSite == "syn"  )) & (CSV$wtnt == "t") & (CSV$bigAAchange == "0") & (CSV$makesCpG == "1")),"MeanFreq"]
    
    #NON SYNONYMOUS SITES (RIGHT GRAPH)
    #all green points for the right nonsynonomous site graph
    g <- frequenciesOfNONSynAmutsNONCP <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "a" & (CSV$bigAAchange == "0") &(CSV$makesCpG == "0"))),"MeanFreq"]
    k <- frequenciesOfNONSynAmutsNONCP <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "t" & (CSV$bigAAchange == "0") &(CSV$makesCpG == "0"))),"MeanFreq"]
    o <- frequenciesOfNONSynAmutsNONCP <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "c" & (CSV$bigAAchange == "0") &(CSV$makesCpG == "0"))),"MeanFreq"]
    q <- frequenciesOfNONSynAmutsNONCP <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "g" & (CSV$bigAAchange == "0") &(CSV$makesCpG == "0"))),"MeanFreq"]
    
    #blue dots of right graph that show a CpG-forming mutation
    h <- frequenciesOfNONSynAmutsCP <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "a") & (CSV$bigAAchange == "0") & (CSV$makesCpG == "1")),"MeanFreq"]
    l <- frequenciesOfNONSynTmutsCP <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "t") & (CSV$bigAAchange == "0") & (CSV$makesCpG == "1")),"MeanFreq"]
    
    #all yellow points for the right nonsyn site graph
    i <- frequenciesOfNONSynAmutsDRASTIC <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "a" & (CSV$bigAAchange == "1") &(CSV$makesCpG == "0"))),"MeanFreq"]
    m <- frequenciesOfNONSynTmutsDRASTIC <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "t" & (CSV$bigAAchange == "1") &(CSV$makesCpG == "0"))),"MeanFreq"]
    p <- frequenciesOfNONSynGmutsDRASTIC <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "c" & (CSV$bigAAchange == "1") &(CSV$makesCpG == "0"))),"MeanFreq"]
    r <- frequenciesOfNONSynCmutsDRASTIC <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "g" & (CSV$bigAAchange == "1") &(CSV$makesCpG == "0"))),"MeanFreq"]
    
    
    #red points on the right
    j <- frequenciesOfNONSynAmutsCPDRASTIC <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "a" & (CSV$bigAAchange == "1") &(CSV$makesCpG == "1"))),"MeanFreq"]
    n <- frequenciesOfNONSynTmutsCPDRASTIC <- CSV[which(((CSV$TypeOfSite == "nonsyn"  )) & (CSV$wtnt == "t" & (CSV$bigAAchange == "1") &(CSV$makesCpG == "1"))),"MeanFreq"]
    
    #Since the results of the data selections are all vectors of differing lengths, they are collected into a list with tags according to plotting order
    mylist <- list (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) 
    k
    mylist
    namvec<-c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r") 
    names(mylist)<-namvec
    (mylist)
    #The list has some NULL elements; these must be replaced with NA since the rest of the code requires values in all lettered locations
    mylistvec<-c()
    for (i in 1:length(mylist)){
        if (length(mylist[[i]])==0){mylist[[i]] = NA}
        mylistvec<-c(mylistvec,mylist[i])
    }
    
    #ggplot requires that data be in a data.frame and to make most effective use of the plotting functions, data will be arranged in to a column of values 
    #and a column of plotting order identifiers (refvec)
    dfmylist<-data.frame(mylistvec[1])
    dfmylist$refvec<-names(mylistvec[1])
    colnames(dfmylist)<-c("values","refvec")
    for (i in 2:length(mylist)){
        dfwkg<-data.frame((mylistvec[i]))
        dfwkg$refvec<-names(mylistvec[i])
        colnames(dfwkg)<-c("values","refvec")
        dfmylist<-rbind(dfmylist,dfwkg)
    }
    #View (dfmylist)
    
    
    
    #Now add a column for AA_Category and a column for colors so tha ggplot can pull colors and legend from data.frame 
    dfmylist[which(dfmylist$refvec == "a"),"AA_category"]<-"Non-drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "b"),"AA_category"]<-"Non-drastic, CpG" 
    dfmylist[which(dfmylist$refvec == "a"),"AA_category"]<-"Non-drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "b"),"AA_category"]<-"Non-drastic, CpG" 
    dfmylist[which(dfmylist$refvec == "c"),"AA_category"]<-"Non-drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "d"),"AA_category"]<-"Non-drastic, CpG" 
    dfmylist[which(dfmylist$refvec == "e"),"AA_category"]<-"Non-drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "f"),"AA_category"]<-"Non-drastic, non-CpG" 
    
    dfmylist[which(dfmylist$refvec == "g"),"AA_category"]<-"Non-drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "h"),"AA_category"]<-"Non-drastic, CpG" 
    dfmylist[which(dfmylist$refvec == "i"),"AA_category"]<-"Drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "j"),"AA_category"]<-"Drastic, CpG" 
    
    dfmylist[which(dfmylist$refvec == "k"),"AA_category"]<-"Non-drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "l"),"AA_category"]<-"Non-drastic, CpG" 
    dfmylist[which(dfmylist$refvec == "m"),"AA_category"]<-"Drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "n"),"AA_category"]<-"Drastic, CpG"
    
    
    dfmylist[which(dfmylist$refvec == "o"),"AA_category"]<-"Non-drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "p"),"AA_category"]<-"Drastic, non-CpG" 
    
    
    dfmylist[which(dfmylist$refvec == "q"),"AA_category"]<-"Non-drastic, non-CpG" 
    dfmylist[which(dfmylist$refvec == "r"),"AA_category"]<-"Drastic, non-CpG" 
    
    
    dfmylist[which(dfmylist$refvec == "a"),"color"]<-"green" 
    dfmylist[which(dfmylist$refvec == "b"),"color"]<-"blue" 
    dfmylist[which(dfmylist$refvec == "a"),"color"]<-"green" 
    dfmylist[which(dfmylist$refvec == "b"),"color"]<-"blue" 
    dfmylist[which(dfmylist$refvec == "c"),"color"]<-"green" 
    dfmylist[which(dfmylist$refvec == "d"),"color"]<-"blue" 
    dfmylist[which(dfmylist$refvec == "e"),"color"]<-"green" 
    dfmylist[which(dfmylist$refvec == "f"),"color"]<-"green" 
    
    dfmylist[which(dfmylist$refvec == "g"),"color"]<-"green" 
    dfmylist[which(dfmylist$refvec == "h"),"color"]<-"blue" 
    dfmylist[which(dfmylist$refvec == "i"),"color"]<-"orange" 
    dfmylist[which(dfmylist$refvec == "j"),"color"]<-"red" 
    
    dfmylist[which(dfmylist$refvec == "k"),"color"]<-"green" 
    dfmylist[which(dfmylist$refvec == "l"),"color"]<-"blue" 
    dfmylist[which(dfmylist$refvec == "m"),"color"]<-"orange" 
    dfmylist[which(dfmylist$refvec == "n"),"color"]<-"red"
    
    
    dfmylist[which(dfmylist$refvec == "o"),"color"]<-"green" 
    dfmylist[which(dfmylist$refvec == "p"),"color"]<-"orange" 
    
    
    dfmylist[which(dfmylist$refvec == "q"),"color"]<-"green" 
    dfmylist[which(dfmylist$refvec == "r"),"color"]<-"orange" 
    
    #View(dfmylist)
    #the plot will have a log y scale so all zero y values must go away.
    
    dfmylist <-dfmylist[which((dfmylist$values != 0)|(is.na(dfmylist$values))),]
    
    
    
    #split the data as there will be two plots one for "Synonomous" & one for "Non-synonomous" sites
    vec1<-c("a","b","c","d","e","f")
    dfmylist1<-dfmylist[which((dfmylist$refvec=="a")|(dfmylist$refvec=="b")|(dfmylist$refvec=="c")
                              |(dfmylist$refvec=="d")|(dfmylist$refvec=="e")|(dfmylist$refvec=="f")),]
    
    dfmylist2<-dfmylist[-which((dfmylist$refvec=="a")|(dfmylist$refvec=="b")|(dfmylist$refvec=="c")
                               |(dfmylist$refvec=="d")|(dfmylist$refvec=="e")|(dfmylist$refvec=="f")),]
    
    
    
    
    #prepare mean & error values for synonomous plot
    
    sem<-function(x){
        return(sd(x,na.rm = FALSE)/sqrt(length(x)))
    }
    
    dfmylist1$mean_vals = 0.0001
    dfmylist1$mean_vals[(which(dfmylist1$refvec == "a"))]<-mean(dfmylist1$values[(which(dfmylist1$refvec == "a"))], na.rm = FALSE)
    dfmylist1$mean_vals[(which(dfmylist1$refvec == "b"))]<-mean(dfmylist1$values[(which(dfmylist1$refvec == "b"))], na.rm = FALSE)
    dfmylist1$mean_vals[(which(dfmylist1$refvec == "c"))]<-mean(dfmylist1$values[(which(dfmylist1$refvec == "c"))], na.rm = FALSE)
    dfmylist1$mean_vals[(which(dfmylist1$refvec == "d"))]<-mean(dfmylist1$values[(which(dfmylist1$refvec == "d"))], na.rm = FALSE)
    dfmylist1$mean_vals[(which(dfmylist1$refvec == "e"))]<-mean(dfmylist1$values[(which(dfmylist1$refvec == "e"))], na.rm = FALSE)
    dfmylist1$mean_vals[(which(dfmylist1$refvec == "f"))]<-mean(dfmylist1$values[(which(dfmylist1$refvec == "f"))], na.rm = FALSE)
    
    dfmylist1$sem_vals = 0.0001
    dfmylist1$sem_vals[(which(dfmylist1$refvec == "a"))]<-sem(dfmylist1$values[(which(dfmylist1$refvec == "a"))])
    dfmylist1$sem_vals[(which(dfmylist1$refvec == "b"))]<-sem(dfmylist1$values[(which(dfmylist1$refvec == "b"))])
    dfmylist1$sem_vals[(which(dfmylist1$refvec == "c"))]<-sem(dfmylist1$values[(which(dfmylist1$refvec == "c"))])
    dfmylist1$sem_vals[(which(dfmylist1$refvec == "d"))]<-sem(dfmylist1$values[(which(dfmylist1$refvec == "d"))])
    dfmylist1$sem_vals[(which(dfmylist1$refvec == "e"))]<-sem(dfmylist1$values[(which(dfmylist1$refvec == "e"))])
    dfmylist1$sem_vals[(which(dfmylist1$refvec == "f"))]<-sem(dfmylist1$values[(which(dfmylist1$refvec == "f"))])
    
    sem(dfmylist1$values[(which(dfmylist1$refvec == "a"))])
    dfmylist1$UCLS = 0.0001
    dfmylist1$LCLS = 0.0001
    
    
    dfmylist1$LCLS = dfmylist1$mean_vals - dfmylist1$sem_vals
    dfmylist1$UCLS = dfmylist1$mean_vals + dfmylist1$sem_vals
    
    
    
    
    #create vector with named elements for ggplot to use to generate colors and legend
    col <- as.character(dfmylist1$color)
    col
    names(col) <- as.character(dfmylist1$AA_category)
    
    
    #plot synonomous points
    Synplot<-ggplot() +
        
        geom_point( data = dfmylist1, mapping = aes(x=refvec, y= values, colour = AA_category), size = 5,show.legend = FALSE) +
        xlab("Mutation Type") + ylab("Samples' Average Frequencies of Mutations") +
        geom_point(data = dfmylist1, mapping = aes (x = refvec, y = mean_vals, colour = AA_category),size = 10.0) +
        geom_errorbar(data = dfmylist1,aes(x = refvec, ymin= LCLS, ymax= UCLS, color = AA_category),width=.5) +
        scale_color_manual(values=col) +
        theme(legend.text=element_text(size=7),legend.title = element_text(size=10)) +
        
        
        
        
        annotate("text", x  = 1.5, y = 0.00001, label = "A -> G") +
        
        geom_vline(aes(linetype=1, colour="black"),xintercept =c(2.5)) +
        
        annotate("text", x  = 3.5, y = 0.00001, label = "T -> C") +
        
        geom_vline(aes(linetype=1, colour="black"),xintercept =c(4.5)) +
        
        geom_point(data = dfmylist1, mapping = aes (x = "e1", y = 0.0), colour = "red",size = 0.0) +
        
        geom_point(data = dfmylist1, mapping = aes (x = "f1", y = 0.0), colour = "red",size = 0.0) +
        
        annotate("text", x  = 5.5, y = 0.00001, label = "C -> T") +
        
        geom_vline(aes(linetype=1, colour="black"),xintercept =c(6.5)) +
        
        annotate("text", x  = 7.5, y = 0.00001, label = "G -> A") +
        
        scale_x_discrete(labels=c("a" = "", "b" = "", "c" = "", "d" = "","e" = "", "e1" = "","f" ="", "f1" = "")) +
        
        scale_y_log10(labels = comma) +
        theme(legend.position="none") +
        expand_limits(y = c(0.00001, 0.1)) +
        theme(plot.margin = unit(c(1,1,3.0,1), "cm")) +
        theme (panel.border = element_rect(colour = "black", fill=NA, 4),plot.title = element_text(hjust = 0.5))
    
    
    
    
    #prepare mean & error values for nonsynonomous plot
    
    dfmylist2$mean_vals = 0.0001
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "g"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "g"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "h"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "h"))],na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "i"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "i"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "j"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "j"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "k"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "k"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "l"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "l"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "m"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "m"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "n"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "n"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "o"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "o"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "p"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "p"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "q"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "q"))], na.rm = FALSE)
    dfmylist2$mean_vals[(which(dfmylist2$refvec == "r"))]<-mean(dfmylist2$values[(which(dfmylist2$refvec == "r"))], na.rm = FALSE)
    
    
    dfmylist2$sem_vals = 0.0001
    
    
    
    sem(dfmylist2$values) 
    sem(dfmylist2$values[(which(dfmylist2$refvec == "g"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "g"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "g"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "h"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "h"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "i"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "i"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "j"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "j"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "k"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "k"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "l"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "l"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "m"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "m"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "n"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "n"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "o"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "o"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "p"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "p"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "q"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "q"))])
    dfmylist2$sem_vals[(which(dfmylist2$refvec == "r"))]<-sem(dfmylist2$values[(which(dfmylist2$refvec == "r"))])
    
    dfmylist2$UCLS = 0.0001
    dfmylist2$LCLS = 0.0001
    
    
    dfmylist2$LCLS = dfmylist2$mean_vals -dfmylist2$sem_vals
    dfmylist2$UCLS = dfmylist2$mean_vals + dfmylist2$sem_vals
    
    
    
    
    
    #create vector with named elements for ggplot to use to generate colors and legend
    col <- as.character(dfmylist2$color)
    col
    names(col) <- as.character(dfmylist2$AA_category)
    
    #plot synonomous points
    NonSynplt<-ggplot() +
        
        #plot points
        geom_point( data = dfmylist2, mapping = aes(x=refvec, y= values, colour = AA_category), size = 5,show.legend = TRUE) +
        xlab("Mutation Type") + ylab("Samples' Average Frequencies of Mutations") +
        geom_point(data = dfmylist2, mapping = aes (x = refvec, y = mean_vals, colour = AA_category),size = 10) +
        geom_errorbar(data = dfmylist2,aes(x = refvec, ymin= LCLS, ymax= UCLS, color = AA_category),width=.5) +
        scale_color_manual(values=col) +
        theme(legend.text=element_text(size=7),legend.title = element_text(size=10),legend.box.background = element_rect(size = 1)) +
        
        #set up graph background
        annotate("text", x  = 2.5, y = 0.00001, label = "A -> G") +
        geom_vline(aes(linetype=1, colour="black"),xintercept =c(4.5)) +
        annotate("text", x  = 6.5, y = 0.00001, label = "T -> C") +
        geom_vline(aes(linetype=1, colour="black"),xintercept =c(8.5)) +
        annotate("text", x  = 9.5, y = 0.00001, label = "C -> T") +
        geom_vline(aes(linetype=1, colour="black"),xintercept =c(10.5)) +
        annotate("text", x  = 11.5, y = 0.00001, label = "G -> A") +
        scale_x_discrete(labels=c("g" = "", "h" = "", "i" = "", "j" = "","k" = "","l" ="","m" ="", "n"="","o" = "","p" = "","q"="","r"="")) +
        
        #scales, margins, border
        scale_y_log10(labels = comma) +
        expand_limits(y = c(0.00001, 0.1)) +
        theme(plot.margin = unit(c(1,1,3.0,1), "cm")) +
        theme (panel.border = element_rect(colour = "black", fill=NA, size=4),plot.title = element_text(hjust = 0.5))
    
    #two graphs on one page
    #library("gridExtra", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
    # require(grid)
    #require(gridExtra)
    # gs<-list(Synplot,NonSynplt)
    # lay<-matrix(c(1,1,2,2,2), nrow = 1)
    # title1=textGrob("
    #                Fig. 3: type of site vs frequency", gp=gpar(fontface="bold", fontsize = 16, cex = 1))
    
    #grid.arrange(top =title1, grobs = gs, layout_matrix = lay)
    
    #add name of virus to name of pdf 
    pdf(paste("synplot_",virusname,".pdf",sep=""), height=15,width=15)
    print(Synplot)
    dev.off()
    
    pdf(paste("nonsynplot_",virusname,".pdf",sep=""), height=15,width=15)
    print(NonSynplt)
    dev.off()
    #end plot function for figure 3; begins on line 327
}





