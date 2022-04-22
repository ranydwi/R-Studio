fifa_results<-FIFA.2018.Statistics
colnames(fifa_results) <-c("Date","Team","Opponent","Goal.Scored","Ball.Possession","Attempts","On.Target","Off.Target","Blocked","Corners","Offsides",
                           "Free.Kicks","Saves","Pass.Accuracy",
                           "Passes","Distance.Covered.Kms","Fouls.Committed","Yellow.Card","Yellow.Red","Red",
                           "Man.of.the.Match","first_goal","round","PSO","Goals.in.PSO","Own.Goals",
                           "Own.Goals.Time")
# Taking only the numeric columns
df <- fifa_results[,c(4:20,22)]
# Check the number of rows and columns
print("no. of rows & columns") 
dim(df)
# Get the average by team for each column
# Replacing NA values with 0
df[is.na(df)] <- 0
head(df)

library(psych)
df_corr <- cor(df) # Create a correlation matrix
KMO(df_corr) # Kaiser-Meyer-Olkin factor adequacy
df1 <- df[,c("Goal.Scored","Ball.Possession","Attempts","Corners","Free.Kicks","Saves","Pass.Accuracy","Passes","Fouls.Committed","Yellow.Card","Red")]
head(df1)

library(corrplot)
df1_corr <- cor(df1) # Create a correlation matrix
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(round(df1_corr, 2), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE) # hide correlation coefficient on the principal diagonal
round(df1_corr, 2) # Correlation matrix in table form

#mulai factor analysis
library (GPArotation)
nfactors <- 4 # Based on the screeplot suggestion
nvars <- dim(_corr)[1]
factors <- fa(r = df1_corr, nfactors = nfactors, rotate = "Varimax")
factors

library(ggplot2)
# Plot Eigenvalues / Represented Variance
eigenvalues <- data.frame(factors$e.values)
colnames(eigenvalues) <- c("Values")
eigenvalues$Number <- 1:nrow(df1_corr)
eigenvalues$RepresentedVariance <- NA for (i in 1:nrow(df1_corr)) 
{eigenvalues$RepresentedVariance[i] <-
  sum(eigenvalues$Values[1:i])/sum(eigenvalues$Values) *100}
eigenvalues$RepresentedVariance_text <-
  paste(round(eigenvalues$RepresentedVariance, 0), " %")
e1 <- ggplot(eigenvalues, aes(Number, y = Values), group = 1)
e1 <- e1 + geom_bar(stat = "identity")
e1 <- e1 + geom_line(aes(y = Values), group = 2)
e1 <- e1 + xlab("Number [-]")
e1 <- e1 + ylab("Eigenvalue [-]")
e1 <- e1 + geom_hline(aes(yintercept = 1), col = "red")
e1 <- e1 + geom_text(aes(label = RepresentedVariance_text), nudge_y = 0.2)
e1 <- e1 + ggtitle("Eigenvalues and explained Variance")
e1 <- e1 + theme_bw()
e1 <- e1 + scale_x_continuous(breaks = seq(1, 10, 1))
e