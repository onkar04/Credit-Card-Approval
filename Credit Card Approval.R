setwd("C:\\Users\\Gnaneeswar\\Desktop\\Praxis\\Credit Card Default") 
getwd()

df=read.csv("credit_approvalr2.csv")             

# t test of difference in means between married and unmarried people

# Number of levels in married is 3. But only two are majority levels
unique(df$Married)

df1<-df[((df[,"Married"]=="u") | (df[,"Married"]=="y")),]
df1$Married <- factor(df1$Married)

unique(df1$Married)

# box plot of age vs married levels. We notice difference in the medians.
boxplot(df1$Age~df1$Married)

# test for significant difference in means

t.test(df1$Age~df1$Married, mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F) # significant

t.test(df1$Age~df1$Employed, mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F) # significant

t.test(df1$YearsEmployed~df1$Employed, mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F) #very significant

# check for correlation of age with other numeric predictors

cor(df1$Age,df1$Income,use="complete.obs")        #0.07
cor(df1$Age,df1$Debt,use="complete.obs")          #0.21
cor(df1$Age,df1$CreditScore,use="complete.obs")    #0.188
cor(df1$Age,df1$YearsEmployed,use="complete.obs")  #0.4

# scattter plot to vizuvalize the relation ship between age and other numeric variables

x<-df1$YearsEmployed
y<-df1$Age
#y=log(y)
z<-df1$Income

yf<-df1$Age[df$Employed=="f"]
yt<-df1$Age[df$Employed=="t"]

xf<-df1$YearsEmployed[df$Employed=="f"]
xt<-df1$YearsEmployed[df$Employed=="t"]

plot(x,y,frame = F)
abline(lm(y~x), col = "blue")

plot(z,y,frame = F)
abline(lm(y~z), col = "blue")

plot(xf,yf,frame = F)
abline(lm(yf~xf), col = "blue")

cor(xf,yf,use="complete.obs")

plot(xt,yt,frame = F)
abline(lm(yt~xt), col = "blue")

cor(xt,yt,use="complete.obs")

#symnum(cor(df1,use="complete.obs"))


# imputing missing values in age by regressing it over years employed 
Ind<-function(t)
  
{
  xx<-dim(length(t))
  xx[which(!is.na(t))]=1
  xx[which(is.na(t))]=0
  return(xx)
}

df$I <- Ind(df$Age)
lm(y~x)


for (i in 1:nrow(df))
{
  if(df$I[i]==0)
  {

    df$Age[i]=28.361+1.447*df$YearsEmployed[i]
    
  }
}


# Checking Imputation of married or not  based on age

t1<-df[df$Married=="u",]
min(t1$Age)

t2<-df[df$Married=="y",]
min(t2$Age)


# Since imputation of categorical variables by above methods involves lot of steps we go for 
# reliable imputation with KNN

# Imputation using K nearest Neighbours 

df$Married[df$Married==""]<-NA
df$BankCustomer[df$BankCustomer==""]<-NA
df$Male[df$Male==""]<-NA
df$EducationLevel[df$EducationLevel==""]<-NA
df$Ethnicity[df$Ethnicity==""]<-NA

library(VIM)

?kNN

summary(df)

df2<-kNN(df)

summary(df2)

df2<-subset(df2,select=Male:Approved)

write.csv(df2,"C:\\Users\\Gnaneeswar\\Desktop\\Praxis\\Credit Card Default\\creditfromr1.csv", row.names = FALSE)
