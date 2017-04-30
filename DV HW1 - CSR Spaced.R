library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)


##Question 2
df <- read.csv("/Users/melaniepalmer/Documents/Data Visualization/cancer_survival_rate.csv")
df <- melt(df)
names(df) <- c("group", "x", "y")

df$col <- c(0)*length(df$y)
for(i in seq(1, length(df$y)-1)){
  if(abs(df$y[i] - df$y[i+1]) <= 1){
    df$col[i] <- df$y[i] + 1.1
  } else {
    df$col[i] <- df$y[i]
  }
}

df$col2 <- c(0)*length(df$y)
for(i in seq(1, length(df$col)-1)){
  if(abs(df$col[i] - df$col[i+1]) <= 1){
    df$col2[i] <- df$col[i] + 0.8
  } else {
    df$col2[i] <- df$col[i]
  }
}

df <- transform(df, x=factor(x, levels=c("X5.year","X10.year","X15.year","X20.year"),
                             labels=c("5 years", "10 years", "15 years", "20 years")),
                y=round(y))


theme_slopegraph <- 
  theme(axis.line = element_blank(),
        axis.text = element_text(colour="black", margin =  unit(0, "lines")),
        axis.text.x = element_text(size = rel(1.5), lineheight = 0.9,
                                   vjust = 1),
        axis.text.y = element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.length = unit(0, "lines"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.25, "lines"), 
        strip.background = element_blank(),
        strip.text.x = element_text(size = rel(1)),
        strip.text.y = element_text(size = rel(1)),
        plot.background = element_blank(),
        plot.title = element_text(size = rel(2), hjust = 0.5),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines"),
        complete=FALSE)



plot <- ggplot(df,aes(x=x,y=col2)) +
  geom_line(aes(group=group),colour="grey80") +
  geom_point(colour="white",size=8) +
  geom_text(aes(label=y),size=2.7) + 
  scale_x_discrete(position = "top") +
  scale_y_continuous(name="", breaks=subset(df, x==head(x,1))$col2, labels=subset(df, x==head(x,1))$group) + 
  theme_slopegraph + labs(title = "Cancer Survival Rate")

plot

