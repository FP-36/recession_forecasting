install.packages("ggplot2")                                        
library("ggplot2")
data


install.packages("corrplot")    
library(corrplot)



df <- data[ -c(1) ]

corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag
  )
}

# edit from here
corrplot2(
  data = df,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)


ggplot(data, aes(x = UNRATE, fill = as.factor(USREC))) + 
  geom_histogram(position = "identity", alpha = 0.4, bins = 30)

ggplot(data, aes(x = GS10, fill = as.factor(USREC))) + 
  geom_histogram(position = "identity", alpha = 0.4, bins = 30)

ggplot(data, aes(x = CUMFNS, fill = as.factor(USREC))) + 
  geom_histogram(position = "identity", alpha = 0.4, bins = 30)

ggplot(data, aes(x = TB3MS, fill = as.factor(USREC))) + 
  geom_histogram(position = "identity", alpha = 0.4, bins = 30)

ggplot(data, aes(x = INDPRO, fill = as.factor(USREC))) + 
  geom_histogram(position = "identity", alpha = 0.4, bins = 30)

ggplot(data, aes(x = CPIAUCNS, fill = as.factor(USREC))) + 
  geom_histogram(position = "identity", alpha = 0.4, bins = 30)


install.packages("GGally")  
library(GGally)


ggparcoord(data, columns = 2:8, groupColumn=8, alphaLines = .1, scale = "uniminmax", 
           mapping=aes(color=as.factor(USREC)))+
  scale_color_discrete("USREC")


install.packages("tidyverse")
library(tidyverse)



install.packages("remotes")
remotes::install_github("jtr13/redav")
install.packages("redav")
library(redav)

plot_missing(data, percent = FALSE)

library(forcats)
x <- factor(data$USREC)  
y <- fct_recode(x, Non-Recession ='0', Recession='1')  
ggplot(data, aes(x)) + geom_bar() 
x
