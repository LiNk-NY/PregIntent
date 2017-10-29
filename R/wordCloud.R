## Wordcloud
library(wordcloud)
library(ggplot2)

pregint <- read.csv("data/pregint.csv")
subGroup <- !is.na(pregint$pregFeel)
subdata <- pregint[subGroup, ]

attach(subdata)

pos <- pregFeel == "Positive"
neg <- pregFeel == "Negative"
feelsDF <- subdata[, grep("^feelsDF", names(pregint), value = TRUE)]
names(feelsDF) <- gsub("feelsDF\\.", "", names(feelsDF))

Positive <- apply(feelsDF[pos, ], 2, sum, na.rm = TRUE)
Negative <- apply(feelsDF[neg, ], 2, sum, na.rm = TRUE)

pdf(file = "feelings.pdf", height = 4, width = 4, paper = "special")
set.seed(718)
wordcloud(names(Positive), freq = Positive, rot.per = 0.35, colors = colorRampPalette(brewer.pal(8, "PuOr"))(12))
set.seed(718)
wordcloud(names(Negative), freq = Negative, rot.per = 0.35, colors = colorRampPalette(brewer.pal(8, "PuOr"))(12))
dev.off()

## BarPlot

feelingsDF <- reshape2::melt(rbind(Positive, Negative))
names(feelingsDF) <- c("pregFeel", "Feelings", "Frequency")

mybar <- ggplot(data = feelingsDF, aes(x = feelings, y = Frequency, fill = pregFeel)) +
    geom_bar(stat="identity", position = position_dodge()) +
    labs(fill = "", x = "If you found out you or your partner were pregnant right now, how would you feel?") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Positive and negative feelings toward pregnancy and emotional response")

pdf("barplot.pdf", height = 4, width = 6, paper = "special")
mybar
dev.off()
