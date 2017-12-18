##########################################
#### FINAL PROJECT FOR BIOINFORMATICS ####
##########################################
#            Group Members:              #
#            Andrew Mahoney              #
#            Nathan O'Neil               #
##########################################
#####ggplot2 is The best for quick, clear analysis of our data #####
library(ggplot2)
library(stats)
library(seqinr)
######### my attempt at stuff #########
#files will be put in a github as part of rubrick, path will be:
#                     https://github.com/nnoneil/AM_NO_final11am

seed <- data.frame(read.csv("seedling_data_sep_DeadvAlive.csv")) 
seed
str(seed)
#'data.frame'  :	"seed" this is a Key for the dataframe!
#$ Sample      : "sample count number 
#$ Line        : "seed line, this includes crossed mutant line deliniated by '/'."
#                   lower case is homozygous mutant (pp), heterozygous/+ is (Pp), wild type gene is (PP)
#                   example [pdx3] homozygous is (pp), cross line [pdx3 rus1/+] heterozygous is (pp/Rr)
#$ Gen         : "seed line generation; parental is 1, self polinated het. is 2, mutant bearing homozygous is 3"
#$ S_Count     : "observed calculated seed count"
#$ DvA         : "alive germinating seeds vs. dead or albino arrested seeds"
#$ Pheno       : "observed recorded phenotype of seed sample"
#$ Geno        : "detected Genotype of seed sample"
#$ RootLg      : "Measured Root length of seedlings (mm)"
#$ ShootWd     : "leaf span or shoot span of seedlings (mm)"
#$ INTmg       : "Wet seed weight at plating (mg)"
#$ FINmg       : "Wet seed weight at tissue collection (mg)"
#$ DpG         : "(Days Post Germination) day seedlings are examined and data is collected"
#$ SC_Total    : "S_Count total of alive and dead seeds e.g. SAMPLE SIZE"
#$ SC_PerT     : "S_Count calculated percent in respect to total sample size"
#$ SC_STDEV_Tot: "S_Count standard deviation from respective Total 
#$ SC_STDEV_WT : "S_Count standard deviation from WT by Generation num"

head(seed)
tail(seed)
Dead <- seed$S_Count[which(seed$DvA == "DEAD")]
Alive <- seed$S_Count[which(seed$DvA == "ALIVE")]

#phenotype by seed count
qplot(S_Count, data = seed, color = DvA) #interesting! 
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

ggplot(aes(y = SC_STDEV_WT, x = DvA, fill = Pheno), data = seed) + geom_boxplot()

### plot for print!
qplot(y = S_Count, x = SC_Total, data = seed,fill = DvA, facets = .~Gen) + geom_smooth(method = lm)#!!
#analysis#
#   split by generation in this graph we see seec count split by seed count total for alive vs. Dead seedlings.
#   the resulting interpretation for this graph is that the linear increase in alive seeds is lost to an increase in dead seed count
#     at the point of mutant penatrance or genotype and phenoytype observation.
#   from this we can conclude that while total seed count is lower the induced mutant condution results in a linar increase in leathality.

ggplot(aes(y = S_Count, x = DvA, fill = Pheno), data = seed) + geom_boxplot()
#analysis#
#   this plot shows the calculated deviation of seed counts separated by their expected phenotype cross.
#   we observe a vast difference between the seed counts of single mutants to crosses generated as segregated by alive or dead seeds.
#   of note, we see that single mutants PP3 RR1 and SS4 show a large increase in seed count as compared to dead. 
##### Learning ggplot! #####
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

stop("running stop line 140")
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

stop("running line 152 stop")
##### test #####
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
##### end of code #####
stop("running stop line 203")