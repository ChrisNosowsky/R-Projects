stemplot(test$Rate) #Q14 A


median(test$Rate)#Q14 Bhe 7 mark
#C Highly concentrated around t
#D It looks like the stem plot is skewing to the right
#E 18.9 looks like an outlier that is far from the rest of the numbers


#Q16
a <- c(5.9, 7.2, 7.3, 6.3, 8.1, 6.8, 7.0, 7.6, 6.8, 6.5, 7.0, 6.4, 7.9, 9.0, 8.2, 8.7, 7.8, 9.7, 7.4, 7.7, 9.7, 7.8, 7.7, 11.6, 11.3, 11.8, 10.7)
b <-c(6.1, 5.8, 7.8, 7.1, 7.2, 9.2, 6.6, 8.3, 7.0, 8.3, 7.8, 8.1, 7.4, 8.5, 8.9, 9.8, 9.7,14.1, 12.6, 11.2)
stem.leaf.backback(a,b)


#A.B The stem plot appears to be skewing to the right
#A.C There doesn't seem to be any outliers in this dataset
#A.D We have n = 20 values. It seems that only 3 of the ones are above 10 MPa so 3/20 will give us about 15% above in our dataset

#B They both seem to be right skewed in terms of shape. Both are centered around 7 stem
#The differences are that the exercise plot seems to be more widely ranged then the data in example 1.2(q.10)
#The other difference is that the Q.16 data seems to contain outliers compared to Q.10's that doesn't contain any outliers

#C
dotchart(a)


#Q16A
x<-list()
x[[1]]<-c(5.9, 7.2, 7.3, 6.3, 8.1, 6.8, 7.0, 7.6, 6.8, 6.5, 7.0, 6.4, 7.9, 9.0, 8.2, 8.7, 7.8, 9.7, 7.4, 7.7, 9.7, 7.8, 7.7, 11.6, 11.3, 11.8, 10.7)
x[[2]]<-c(6.1, 5.8, 7.8, 7.1, 7.2, 9.2, 6.6, 8.3, 7.0, 8.3, 7.8, 8.1, 7.4, 8.5, 8.9, 9.8, 9.7, 14.1, 12.6, 11.2)

X<-stack(setNames(x, c("a","b")))

brx <- pretty(range(X$values), 
              n = nclass.Sturges(X$values),min.n = 1)

X$stem <- factor(brx[cut(unlist(x), breaks=brx, include.lowest=T, labels=F)], levels=brx[-length(brx)])
X$leaf <- as.integer(X$values %% 1 *10)


max.leaf.width <- 2*with(aggregate(leaf~ind+stem, X, length), tapply(leaf, ind, max))

fmt<-paste0("%", max.leaf.width[1],"s | %2s | %-", max.leaf.width[2],"s")

va<-with(subset(X, ind=="a"), tapply(leaf, stem, function(x) paste(rev(sort(x)), collapse=" ")))
vb<-with(subset(X, ind=="b"), tapply(leaf, stem, function(x) paste(sort(x), collapse=" ")))

va[is.na(va)]<-""
vb[is.na(vb)]<-""

cat(paste(sprintf(fmt, va, levels(X$stem), vb), collapse="\n"), "\n")

q18 <- read.table(file.choose(), header=T, sep="\t")
attach(q18)
hist.default(Number)
hist(Q18test$`Number o`,
     )

hist(HW1_Data_sets_new$Cumulati,
     ylim=c(0,100))
hist(q18)
hist(q18$V1,
     main = "Histogram of Number of Papers Published Per Author",
     breaks = ,
     xlab = "Author",
     xlim = c(0,25),
     ylim = c(0,800)
     )


hist.default(TestPiece$Number,
             breaks = TestPiece$Number)

hist(q18$Published)

hist(q18$Number,
     breaks = 17,
     freq = FALSE)

stem(Q20$Number)
install.packages("aplpack", depend = TRUE)
library(aplpack)
stem.leaf(Q20$Number)



hist(Q20$Number,
     main = "Histogram of Subdivisions",
     ylim = c(0,15),
     xlab = "Length")


hist(q24$V1,
     main = "Histogram of Spot Weld Strength",
     xlab = "lb's of Strength",
     ylab = "Percent",
     ylim = c(0,25),
     breaks = 10,
     xaxt = "n"
     )
axis(1, at=seq(4000,6000, by=200), labels=c("4000", "4200","4400","4600","4800","5000","5200","5400","5600","5800", "6000"))


hist(Q32$Value,
     probability = TRUE,
     breaks = 14)


stem(Q54$Value, scale=1)



boxplot(Q54$Value)



