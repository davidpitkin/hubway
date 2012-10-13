##========================================================================
##
## http://hubwaydatachallenge.org/
##
## Patrick Hausmann <patrick.hausmann@covimo.de>
##
## 13-10-2012
##========================================================================

load(file = file.path("rdata", "hubway_bike.rdata"))

# End_date later then start_date?
x$check <- (x$end_date > x$start_date) && (x$end_date != x$start_date)

table(x$check, x$same_day)

###############################################################################

Make_Plot <- function(xdf, x, y, gr, fname) {
  xtitle <- paste("Rent by", x, "and", gr, sep = " ")
  pk <- ggplot(xdf, aes_string(x=x, y=y, group = gr, color = gr)) + geom_line()
  pk <- pk + theme_bw()
  pk <- pk + ggtitle(xtitle)
  pk <- pk + theme(plot.title=element_text(family="Times", face="bold", size=10))
  pk <- pk + theme(axis.title.x = element_text(face="bold", colour="#990000", size=10),
                   axis.text.x  = element_text(angle=90, hjust=0, size=7))
  ggsave(pk, file = file.path("graphics", fname), width=11.69, height=8.27)
}

Calc_cumsum <- function(xdf, x1, x2) {
  m <- table(xdf[[x1]], xdf[[x2]])
  m <- melt(apply(m, 2, 
            FUN = function(L) cumsum(prop.table(L)* 100)))
  colnames(m) <- c(x1, x2, "Freq")
  return(m)
}

###############################################################################
#                        Only trips under 24h
###############################################################################

x <- x[x$duration <= 86400, ]

###############################################################################

p1 <- table(x$start_s, x$subscription_type)
p1 <- melt(apply(p1, 2, prop.table) * 100)
colnames(p1) <- c("start_s", "subscription_type", "Freq" )
Make_Plot(p1, x= "start_s", y= "Freq", gr="subscription_type", "p1.pdf")

# cumsum, ecdf?
p2 <- Calc_cumsum(x, "start_s", "subscription_type")
Make_Plot(p2, x= "start_s", y= "Freq", gr="subscription_type", "p2.pdf")

p3 <- table(x$start_s, x$gender)
p3 <- melt(apply(p3, 2, prop.table) * 100)
colnames(p3) <- c("start_s", "gender", "Freq" )
Make_Plot(p3, x= "start_s", y= "Freq", gr="gender", "p3.pdf")

# cumsum, ecdf?
p4 <- Calc_cumsum(x, "start_s", "gender")
Make_Plot(p4, x= "start_s", y= "Freq", gr="gender", "p4.pdf")

# cumsum, ecdf?
p5 <- Calc_cumsum(x, "start_s", "ag1")
Make_Plot(p5, x= "start_s", y= "Freq", gr="ag1", "p5.pdf")

############################################################################

p6 <- ggplot(x, aes(x=age, group= gender, color = gender)) + geom_density()
p6 <- p6 + ggtitle("Density age/gender") + theme_bw()
ggsave(p6, file = file.path("graphics", "p6.pdf"), width=11.69, height=8.27)

p7 <- ggplot(x, aes(x=wday, y=log(duration/60)))
p7 <- p7 + geom_boxplot(outlier.colour = "red", outlier.size = 1)
p7 <- p7 + facet_wrap(~subscription_type)
p7 <- p7 + ggtitle("log(duration/60) / wday") + theme_bw()
ggsave(p7, file = file.path("graphics", "p7.pdf"), width=11.69, height=8.27)

# log
p7a <- ggplot(x, aes(x=wday, y=duration/60))
p7a <- p7a + geom_boxplot(outlier.colour = "red", outlier.size = 1) 
p7a <- p7a + facet_wrap(~subscription_type)
p7a <- p7a + scale_y_log10()
p7a <- p7a + ggtitle("log(duration/60) / wday") + theme_bw()
ggsave(p7a, file = file.path("graphics", "p7a.pdf"), width=11.69, height=8.27)

p8 <- ggplot(x, aes(x=ag1, y=log(duration/60))) 
p8 <- p8 + geom_boxplot(outlier.colour = "red", outlier.size = 1)
p8 <- p8 + ggtitle("log(duration/60) / ag1") + theme_bw()
ggsave(p8, file = file.path("graphics", "p8.pdf"), width=11.69, height=8.27)

p9 <- ggplot(subset(x, subset=gender != ""), aes(x=ag1, y=log(duration/60)))
p9 <- p9 + ggtitle("log(duration/60) / ag1") + theme_bw()
p9 <- p9 + geom_boxplot(outlier.colour = "red", outlier.size = 1) + facet_wrap( ~ gender)
ggsave(p9, file = file.path("graphics", "p9.pdf"), width=11.69, height=8.27)

############################################################################

tapply(x$duration/60, list(Month = x$mon, day = x$wday), length)

pdf(file.path("graphics", "h1_scale_none.pdf"), width=11.69, height=8.27)
(w1 <- tapply(x$duration/60, list(Month = x$mon, day = x$wday), FUN=function(L) round(median(L), 1)))
heatmap.2(as.matrix(w1),
           col="bluered", scale = "none", trace = "none",
           sepcolor='white', sepwidth=0.25, cexRow=1, cexCol=1, margins = c(4, 2),
           cellnote=round(w1, 1), notecol="black", 
           key=TRUE, keysize=1,
           main = list("Median, Duration in min, scale = none / month vs. Weekday / (13.03.2011-30.11.2011)", cex=1.2))
dev.off()

pdf(file.path("graphics", "h2_scale_col.pdf"), width=11.69, height=8.27)
heatmap.2(as.matrix(w1),
           col="bluered", scale = "col", trace = "none",
           sepcolor='white', sepwidth=0.25, cexRow=1, cexCol=1, margins = c(4, 2),
           cellnote=round(w1, 1), notecol="black", 
           key=TRUE, keysize = 1,
           main = list('Median, Duration in min, scale = col / month vs. Weekday / (13.03.2011-30.11.2011)', cex=1.2))
dev.off()

pdf(file.path("graphics", "h3_scale_col_wday_ag1.pdf"), width=11.69, height=8.27)
w2 <- tapply(x$duration/60, list(s=x$wday, ag1 = x$ag1), FUN=function(L) round(median(L),1))
heatmap.2(as.matrix(w2), trace = "none", scale ="col", Rowv = FALSE, 
            hclustfun=function(c){hclust(c, method='ward')},
            xlab="ag1", ylab="Weekday", density.info="none", 
            col = brewer.pal(8,"RdYlBu"), 
            dendrogram="col",  
            sepcolor='white', sepwidth=c(0.5, 0.5),
            cellnote=round(w1,1), notecol="black", 
            margins=c(5, 5), 
            key=TRUE, keysize = 1, 
            cexRow=0.75, cexCol=0.75,
            main = list("Median, Duration in min 'Ward', scale = col / Weekday vs. Age / (13.03.2011-30.11.2011)", cex=1.2))
dev.off()

#
# Fini
#