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
library(Hmisc)
  library(lattice)
  library(survival)
  library(Formula)
library(seqinr)
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

#### Step 1 ####
#what does the data look like? how can we compare the factors and numerators?
head(seed)
tail(seed)

#first we conduct simple comparisons by quick plot (qplot)

#phenotype by seed count
qplot(S_Count, data = seed, fill = DvA) #interesting! 
qplot(Pheno,S_Count, data = seed, facets = .~DvA)# interesting!
qplot(Pheno,S_Count, data = seed, facets = .~DvA)+ geom_smooth(method = lm)# each point is considered a seprate line, no connected data...
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
stop("running stop 1")
#### step 2 #### 
#Using ggplot to create more complex graphs to show us our data.

#using ggplot2 for 2 factor box plotting of the standard DEV from WT 
head(seed)


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
m <- ggplot(aes(S_Count, SC_Total),data = seed)
m + geom_raster(aes(fill = DvA)) + geom_smooth(method = lm)

stop("running stop 2")
#### step 3 ####
### decide what plot represents the data accuratly and answers our research question.
# plots for print!

####################################figure 1 
#what trend if any can be seen in seed count? 
qplot(y = S_Count, x = SC_Total,                                                 # x and y values to be compared. 
      data = seed,                                                               # data set or data.frame to be used 
      color = DvA ,                                                              # color the data by 2 factor column DvA "dead" or "alive" seed count.
      facets = .~Gen,                                                            # Faucets divides data by the chosen factor into rows/columns 
      xlab = "Seed Count totals by Generation",                                  # X-axis label
      ylab = "Seed Count",                                                       # Y-axis label
      main = "Generational Segrigation of alive and dead seeds by Seed Count"    # Plot title
       ) + geom_smooth(method = lm)                                              # produces a best fit line with grey area depicting condifence 
#analysis#
#   split by generation in this graph we see seec count split by seed count total for alive vs. Dead seedlings.
#   the resulting interpretation for this graph is that the linear increase in alive seeds is lost to an increase in dead seed count
#     at the point of mutant penatrance or genotype and phenoytype observation.
#   from this we can conclude that while total seed count is lower the induced mutant condution results in a linar increase in leathality.

####################################figure 2
# does the observed trend persist in segrigated seed counts by screened phenotype/genotype?
f <- ggplot(aes(y = S_Count, x = DvA, fill = Pheno),data = seed)                                   # constructed blank plot for data 
fi <- f + geom_boxplot()                                                                           # determination of how the prior data sets will be presented [boxplot]
fig <- fi + geom_smooth(method = lm)                                                               # geom argument diffrent from qplot, this smooths out the plot presentation. cleans up aes arguments for better presentation. 
fig2 <- fig + labs(x = "Genetic assesment of segrigated seeds",                                    # labs adds new labels over the prior to ggplot. 
                   y = "seed count",                                                               # y label.
                   title = "Seed Count Distribution From Mutant Screening",                        # title .
                   subtitle = "homozygous single mutants and heterozygous double mutants shown")   # subtitle text.
fig2
#above shows plot before dividing by generation. as shown this is rather misleading~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# below is the above plot after dividing by generation, while not as pritty is more acurate to the reality of the data.
Tf <- ggplot(aes(y = S_Count, x = DvA, fill = Pheno),data = seed)                                  # constructed blank plot for data 
Tfi <- pf + geom_boxplot()                                                                         # determination of how the prior data sets will be presented [boxplot]
Tfig <- pfi + facet_grid(.~Gen)                                                                    #argument, facets varible is directional (right or left) .~column or Row~. and will divide data by that factor. 
Tfig2 <- pfig + labs(x = "Genetic assesment of segrigated seeds",                                  # labs adds new labels over the prior to ggplot. 
                   y = "seed count",                                                               # y label.
                   title = "Seed Count Distribution From Mutant Screening",                        # title .
                   subtitle = "homozygous single mutants and heterozygous double mutants shown")   # subtitle text.
Tfig2

#analysis#
#   this plot shows the calculated deviation of seed counts separated by their expected phenotype cross.
#   we observe a vast difference between the seed counts of single mutants to crosses generated as segregated by alive or dead seeds.
#   of note, we see that single mutants PP3 RR1 and SS4 thrive 
#   heterozygotes (rus1/+ pdx3/+, rus1/+ sos4/+) have varying success 


stop("running stop 3")
##### Learning ggplot! #####
# below is code used to understand ggplot and its use
# generate a random data example....
df <- data.frame(f1=factor(rbinom(100, 1, 0.45), label=c("x","y")), 
                 f2=factor(rbinom(100, 1, 0.45), label=c("up","down")),
                 boxthis=rnorm(100))
df
df$f1f2 <- interaction(df$f1, df$f2)
head(df)
#?ggplot
ggplot(aes(y = boxthis, x = f1f2), data = df) + geom_boxplot()
ggplot(aes(y = boxthis, x = f2, fill = f1), data = df) + geom_boxplot()
#examples of using ggplot2 for 2 factor box plotting
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
#different possible final plot...
# genetic type ggplot
gf <- ggplot(aes(y = S_Count, x = DvA, fill = Pheno),data = seed)                                   # constructed blank plot for data 
gfi <- gf + geom_boxplot()                                                                           # determination of how the prior data sets will be presented [boxplot]
gfig <- gfi + facet_grid(.~Gen)                                                               # geom argument diffrent from qplot, this smooths out the plot presentation. cleans up aes arguments for better presentation. 
gfig2 <- gfig + labs(x = "Genetic assesment of segrigated seeds",                                    # labs adds new labels over the prior to ggplot. 
                     y = "seed count",                                                               # y label.
                     title = "Seed Count Distribution From Mutant Screening",                        # title .
                     subtitle = "homozygous single mutants and heterozygous double mutants shown")   # subtitle text.
gfig2


##### end of code #####
stop("end of code")