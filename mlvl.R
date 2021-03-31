library(MultiLCIRT)
library(xlsx)
library(psych)
library(ggplot2)
library(magrittr)

setwd("C:/Users/home_/Desktop/clan/psed/mlv")

# load and prepare data ----
# V - ind level
# U - gr level
datà_ipips_use <- read.csv2("datà_ipips_use.csv",header=T)
datà_ipips_use <- datà_ipips_use[,c(3:26)]
View(datà_ipips_use)

datà_ipips_use <- datà_ipips_use[complete.cases(datà_ipips_use),]

ys <- rowSums(Y)
describe(ys)
hist(ys)
View(ys)
Y <- datà_ipips_use[,c(14:24)] # the lowest value of responses should be 0
Y <- data.matrix(Y)

clust <- datà_ipips_use[,2]
clust <- as.factor(clust)
clust <- c(sapply(clust, as.numeric))
str(clust)

W <- datà_ipips_use[,c(4,7:13)] #urban/rural, years of experience, education lvl, class size, Self-efficacy, General-intel, Human-fixed, Math_fixed
describe(W) 
table(W$Urban1Rural0)
table(W$Education_level) # ?? maybe make a binary var? (0 = 0, 1,2,4 = 1)?
W <- cbind(W,clust)
out <- aggr_data(W)
S <- out$data_dis
W <- as.matrix(S[,c(1:8)])
str(W)

X <- datà_ipips_use[,c(3,5,6)] #gender, read, math
str(X)
describe(X)


# The final model

y10_u3_v5 <-  est_multi_poly_clust(Y_10,kU=3,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE)


######### Determine the number of classes ---- #########

### The first dimension 1,2,3,7,8,10 ### -----

# Without "cultura"

# 1 Determine the number of lower-level classes V ignoring the multilevel structure U (assuming that H = 1)


Y_10 <- Y[,c(1,2,3,7,8,10)]

v2_10d <-  est_multi_poly(Y_10, k=2,X=X,link=2, disc=0, difl=0, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 
v3_10d <-  est_multi_poly(Y_10, k=3,X=X,link=2, disc=0, difl=0, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 
v4_10d <-  est_multi_poly(Y_10, k=4,X=X,link=2, disc=0, difl=0, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 66609.46
v5_10d <-  est_multi_poly(Y_10, k=5,X=X,link=2, disc=0, difl=0, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 66486.32 ! V=5
v6_10d <-  est_multi_poly(Y_10, k=6,X=X,link=2, disc=0, difl=0, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 66518.48
v7_10d <-  est_multi_poly(Y_10, k=7,X=X,link=2, disc=0, difl=0, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 

BIC910 <- rbind(v2_10d$bic,  
                v3_10d$bic,  
                v4_10d$bic,  
                v5_10d$bic,  
                v6_10d$bic,  
                v7_10d$bic)

Number_of_classes <- c("2", "3", "4", "5", "6", "7")
bick8 <- data.frame(Number_of_classes,BIC910)
bick8

## 2. Fix the number of lower-level classes V to the value of step 1 and determine the number of higher-level classes U.

y10_u2_v5 <-  est_multi_poly_clust(Y_10,kU=2,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 66095.87 # BIC # difl=0 65742.32
y10_u3_v5 <-  est_multi_poly_clust(Y_10,kU=3,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 66025.32 ! U = 3
y10_u4_v5 <-  est_multi_poly_clust(Y_10,kU=4,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 66078.32
y10_u5_v5 <-  est_multi_poly_clust(Y_10,kU=5,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 66031.99
y10_u6_v5 <-  est_multi_poly_clust(Y_10,kU=6,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 66081.50
y10_u7_v5 <-  est_multi_poly_clust(Y_10,kU=7,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC
y10_u8_v5 <-  est_multi_poly_clust(Y_10,kU=8,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC


BIC_9 <-  rbind(y10_u2_v5$bic,  
                y10_u3_v5$bic,  
                y10_u4_v5$bic,  
                y10_u5_v5$bic,  
                y10_u6_v5$bic,  
                y10_u7_v5$bic,
                y10_u8_v5$bic)

Number_of_classes <- c("2", "3", "4", "5", "6", "7", "8")
bick8 <- data.frame(Number_of_classes,BIC_9)
bick8

###   3. Fix the number of higher-level classes U to the value of step 2 and redetermine the number of lower-level classes V. 

y10_u3_v2 <-  est_multi_poly_clust(Y_10,kU=3,kV=2,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC  
y10_u3_v3 <-  est_multi_poly_clust(Y_10,kU=3,kV=3,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC  
y10_u3_v4 <-  est_multi_poly_clust(Y_10,kU=3,kV=4,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 
y10_u3_v5 <-  est_multi_poly_clust(Y_10,kU=3,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 66025.32 ! kU=3,kV=5
y10_u3_v6 <-  est_multi_poly_clust(Y_10,kU=3,kV=6,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 
y10_u3_v7 <-  est_multi_poly_clust(Y_10,kU=3,kV=7,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC
y10_u3_v8 <-  est_multi_poly_clust(Y_10,kU=3,kV=8,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC


BIC_7 <-  rbind(y10_u3_v2$bic,  
                y10_u3_v3$bic,  
                y10_u3_v4$bic,  
                y10_u3_v5$bic,  
                y10_u3_v6$bic,  
                y10_u3_v7$bic)

Number_of_classes <- c("2", "3", "4", "5", "6", "7")
bick8 <- data.frame(Number_of_classes,BIC_7)
bick8


### The first dimension ### Interpret the results ----

# Th - estimated matrix of ability levels for each dimension and latent class
Thsy10_u3_v5 <- y10_u3_v5[["Th"]] # 4.4308135 3.2173843 2.0960990 0.6532474 6.1529330
Thsy10_u3_v5 <- as.numeric(Thsy10_u3_v5)


data7 <- data.frame(
  name7=c("1", "2", "3", "4", "5"),
  val=Thsy10_u3_v5
)


data7 %>%
  arrange(Thsy10_u3_v5) %>%
  mutate(name = factor(name7, levels=c("1", "2", "3", "4", "5"))) %>%
  ggplot( aes(x=name7, y=Thsy10_u3_v5)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="orange") +
  theme_bw() +
  xlab("")


# Pp - matrix of the posterior probabilities for each response configuration and latent class (if output=TRUE)1

Pp35_ind <- as.data.frame(y10_u3_v5[["Vind"]])
Pp35_ind <- round(Pp35_ind,2)
View(Pp35_ind)

Pp35_clust <- as.data.frame(y10_u3_v5[["Vclust"]])
Pp35_clust <- round(Pp35_clust,2)

# write.csv2(Pp35_clust,"Pp35_ind.csv")


# Covariates 2 lvl U group


U_cov <- data.frame(y10_u3_v5[["DeU"]])
U_cov_se <- data.frame(y10_u3_v5[["seDeU"]])
U_covariates <- data.frame(U_cov,U_cov_se)

write.csv2(U_covariates,"U_covariates.csv")

# 1 calculate the standard error: SE = (u ??? l)/(2??1.96)
# 2 calculate the test statistic: z = Est/SE
# 3 calculate the P value2: P = exp(???0.717??z ??? 0.416??z2).

urban <- U_cov[2,]
urban_se <- U_cov_se[2,]

urban_z_2 <- -0.29/0.441 
urban_P_2 <- exp((-0.717*urban_z_2) - 0.416*(urban_z_2*urban_z_2))

urban_z_3 <- 1.011/0.40
urban_P_3 <- exp((-0.717*urban_z_3) - 0.416*(urban_z_3*urban_z_3))

urban_z_4 <- 1.27/0.47
urban_P_4 <- exp((-0.717*urban_z_4) - 0.416*(urban_z_4*urban_z_4))

# Covariates 1 lvl V ing

V_cov <- data.frame(y10_u3_v5[["DeV"]])
V_cov_se <- data.frame(y10_u3_v5[["seDeV"]])


### The second dimension 4,5,6,9 ### -----


# 1 Determine the number of lower-level classes V ignoring the multilevel structure U (assuming that H = 1)
# difl=1 RSM

Y_2 <- Y[,c(4,5,6,9)]
describe(Y_2)

v2_2d <-  est_multi_poly(Y_2, k=2,X=X,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 
v3_2d <-  est_multi_poly(Y_2, k=3,X=X,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 
v4_2d <-  est_multi_poly(Y_2, k=4,X=X,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 
v5_2d <-  est_multi_poly(Y_2, k=5,X=X,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 42241.60    
v6_2d <-  est_multi_poly(Y_2, k=6,X=X,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 42212.04 ! KV = 6
v7_2d <-  est_multi_poly(Y_2, k=7,X=X,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE,out_se=T) # BIC 42241.63

BIC7 <- rbind(v2_2d$bic,  
                v3_2d$bic,  
                v4_2d$bic,  
                v5_2d$bic,  
                v6_2d$bic,  
                v7_2d$bic)

Number_of_classes <- c("2", "3", "4", "5", "6", "7")
bick8 <- data.frame(Number_of_classes,BIC7)
bick8

## 2. Fix the number of lower-level classes V to the value of step 1 and determine the number of higher-level classes U.

y2_u2_v6 <-  est_multi_poly_clust(Y_2,kU=2,kV=6,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 41900.97 
y2_u3_v6 <-  est_multi_poly_clust(Y_2,kU=3,kV=6,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 41857.30 ! U = 3
y2_u4_v6 <-  est_multi_poly_clust(Y_2,kU=4,kV=6,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 41879.32
y2_u5_v6 <-  est_multi_poly_clust(Y_2,kU=5,kV=6,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 


BIC_9 <-  rbind(y2_u2_v6$bic,  
                y2_u3_v6$bic,  
                y2_u4_v6$bic,  
                y2_u5_v6$bic)

Number_of_classes <- c("2", "3", "4", "5")
bick8 <- data.frame(Number_of_classes,BIC_9)
bick8

###   3. Fix the number of higher-level classes U to the value of step 2 and redetermine the number of lower-level classes V. 

y10_u3_v2 <-  est_multi_poly_clust(Y_2,kU=3,kV=2,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC  

y2_u3_v3 <-  est_multi_poly_clust(Y_2,kU=3,kV=3,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC  
y2_u3_v4 <-  est_multi_poly_clust(Y_2,kU=3,kV=4,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 
y2_u3_v5 <-  est_multi_poly_clust(Y_2,kU=3,kV=5,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 41880.45 
y2_u3_v6 <-  est_multi_poly_clust(Y_2,kU=3,kV=6,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 41857.3 ! kU=3,kV=6
y2_u3_v7 <-  est_multi_poly_clust(Y_2,kU=3,kV=7,W=W,X=X,clust=clust,link=2, disc=0, difl=1, start = 1, disp=TRUE,output=TRUE) # BIC 41899.58



### ### Interpret the results ----

# Th - estimated matrix of ability levels for each dimension and latent class
Thsy10_u3_v5 <- y10_u3_v5[["Th"]] # 4.4308135 3.2173843 2.0960990 0.6532474 6.1529330
Thsy10_u3_v5 <- as.numeric(Thsy10_u3_v5)


data7 <- data.frame(
  name7=c("1", "2", "3", "4", "5"),
  val=Thsy10_u3_v5
)

data7 %>%
  arrange(Thsy10_u3_v5) %>%
  mutate(name = factor(name7, levels=c("1", "2", "3", "4", "5"))) %>%
  ggplot( aes(x=name7, y=Thsy10_u3_v5)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="orange") +
  theme_bw() +
  xlab("")


# Pp - matrix of the posterior probabilities for each response configuration and latent class (if output=TRUE)1

Pp35_ind <- as.data.frame(y10_u3_v5[["Vind"]])
Pp35_ind <- round(Pp35_ind,2)
View(Pp35_ind)

Pp35_clust <- as.data.frame(y10_u3_v5[["Vclust"]])
Pp35_clust <- round(Pp35_clust,2)

# write.csv2(Pp35_clust,"Pp35_ind.csv")


# Covariates 2 lvl U group


U_cov <- data.frame(y10_u3_v5[["DeU"]])
U_cov_se <- data.frame(y10_u3_v5[["seDeU"]])
U_covariates <- data.frame(U_cov,U_cov_se)

write.csv2(U_covariates,"U_covariates.csv")

# 1 calculate the standard error: SE = (u ??? l)/(2??1.96)
# 2 calculate the test statistic: z = Est/SE
# 3 calculate the P value2: P = exp(???0.717??z ??? 0.416??z2).

urban <- U_cov[2,]
urban_se <- U_cov_se[2,]

urban_z_2 <- -0.29/0.441 
urban_P_2 <- exp((-0.717*urban_z_2) - 0.416*(urban_z_2*urban_z_2))

urban_z_3 <- 1.011/0.40
urban_P_3 <- exp((-0.717*urban_z_3) - 0.416*(urban_z_3*urban_z_3))

urban_z_4 <- 1.27/0.47
urban_P_4 <- exp((-0.717*urban_z_4) - 0.416*(urban_z_4*urban_z_4))

# Covariates 1 lvl V ing

V_cov <- data.frame(y10_u3_v5[["DeV"]])
V_cov_se <- data.frame(y10_u3_v5[["seDeV"]])
V_covariates <- data.frame(V_cov,V_cov_se)

write.csv2(V_covariates,"V_covariates.csv")
