library(car)
library(emmeans)
hfa <- read.csv("C:/Users/ernie/OneDrive/Desktop/HFA paper/HFA.csv")

cc <- read.csv("C:/Users/ernie/OneDrive/Desktop/HFA paper/HFA_co2.csv")

library(RColorBrewer)
brewer.pal(n=6,"Set1")

palette <- c("#377EB8", "#E41A1C", "#4DAF4A", "#984EA3", "#FF7F00" ,"#FFFF33")

#Analyze respiration

a <- lm(cc ~ soil * litter, data=cc)
qqPlot(resid(a))
a2 <- glm(cc~soil*litter, data=cc, family=Gamma(link=log))
AIC(a, a2)
Anova(a2)
a3 <- emmeans(a2, ~  soil | litter)
contrast(a3, method="pairwise")

a4 <- emmeans(a2, ~  litter)
contrast(a4, method="pairwise")

a5 <- emmeans(a2, ~  soil)
contrast(a3, method="pairwise")

a6 <- lm(cc ~ group, data=cc)
AIC(a6,a7)
Anova(a7)

aggregate(cc~group, data=cc, FUN=mean)



#respiration plot for Figure 1

FB1 <- aggregate(cc~litter+soil+group, data=cc, FUN =function(x) c(mean=mean(x), sd=sd(x),n=length(x)))
FB1

FB2 <- do.call(data.frame, FB1)
FB2

FB2$se <- FB2$cc.sd / sqrt(FB2$cc.n)
head(FB2)


library(ggpattern)

library(ggplot2)

setwd("C:/Users/ernie/OneDrive/Desktop/")

jpeg(filename="fig1v2.jpeg", bg="transparent", res=500, units = "in", height=6, width=8)

bar_plotFB<- ggplot(FB2, aes(x=FB2$litter, y=FB2$cc.mean, fill=FB2$soil,pattern=FB2$group)) + 
  #geom_bar(stat="identity", color="black", size=1, position="dodge") +
  geom_bar_pattern(stat="identity", position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  geom_errorbar(aes(ymin=FB2$cc.mean-FB2$se, ymax=FB2$cc.mean+FB2$se), width=0.2, size=.5, position=position_dodge(0.9)) +
  annotate('text', 2, 120, label="Litter: ~italic(P) < 0.001", size=5, parse=TRUE)+
  annotate('text', 2, 110, label="Inoculum: italic(P) < 0.001", size = 5, parse=TRUE)+
  annotate('text', 2, 100, label="Litter %*% Inoculum: italic(P) == 0.057", size=5, parse=TRUE)+
  #BW
  annotate("text", label="a", x=.61, y=88, size=3) +
  annotate("text", label="ab", x=.77, y=73, size=3) +
  annotate("text", label="a", x=.93, y=90, size=3) +
  annotate("text", label="b", x=1.09, y=69, size=3) +
  annotate("text", label="a", x=1.23, y=92, size=3) +
  annotate("text", label="a", x=1.39, y=86, size=3) +
  #PP
  annotate("text", label="a", x=1.61, y=75, size=3) +
  annotate("text", label="a", x=1.77, y=84, size=3) +
  annotate("text", label="a", x=1.93, y=82, size=3) +
  annotate("text", label="a", x=2.09, y=74, size=3) +
  annotate("text", label="a", x=2.23, y=77, size=3) +
  annotate("text", label="a", x=2.39, y=73, size=3) +
  #RM
  annotate("text", label="a", x=2.61, y=59, size=3) +
  annotate("text", label="a", x=2.77, y=65, size=3) +
  annotate("text", label="a", x=2.93, y=61, size=3) +
  annotate("text", label="a", x=3.09, y=57, size=3) +
  annotate("text", label="a", x=3.23, y=60, size=3) +
  annotate("text", label="a", x=3.39, y=61, size=3) +
  #TA
  annotate("text", label="a", x=3.61, y=113, size=3) +
  annotate("text", label="a", x=3.77, y=113, size=3) +
  annotate("text", label="a", x=3.93, y=122, size=3) +
  annotate("text", label="a", x=4.09, y=115, size=3) +
  annotate("text", label="a", x=4.23, y=115, size=3) +
  annotate("text", label="a", x=4.37, y=115, size=3) +
  #TP
  annotate("text", label="a", x=4.61, y=101, size=3) +
  annotate("text", label="a", x=4.77, y=105, size=3) +
  annotate("text", label="a", x=4.93, y=102, size=3) +
  annotate("text", label="a", x=5.09, y=106, size=3) +
  annotate("text", label="a", x=5.23, y=110, size=3) +
  annotate("text", label="a", x=5.37, y=108, size=3) +
  #WP
  annotate("text", label="ab", x=5.61, y=63, size=3) +
  annotate("text", label="ab", x=5.77, y=65, size=3) +
  annotate("text", label="ab", x=5.93, y=66, size=3) +
  annotate("text", label="b", x=6.09, y=58, size=3) +
  annotate("text", label="a", x=6.23, y=72, size=3) +
  annotate("text", label="a", x=6.37, y=68, size=3) +
  theme_classic() +
  scale_pattern_manual(values = c(home = "stripe", away= "none"),guide=FALSE) +
  scale_fill_manual(values = palette, name="Inoculum") +
  theme(axis.title=element_text(size=16)) +
  theme(text=element_text(size=16)) +
  theme(legend.text = element_text(color = "black", size = 12)) +
  #theme(axis.line.y=element_line(colour="black", size=1)) + 
  scale_y_continuous(bquote(~'Cumulative CO'[2]~'-C'~(mg~g~dry~wt~litter^-1))) +
  guides(fill = guide_legend(override.aes = list(pattern = "none")))+
  xlab('Litter Species')
plot(bar_plotFB)

dev.off()

#respiration plot for Figure S1


FB1 <- aggregate(cc~soil, data=cc, FUN =function(x) c(mean=mean(x), sd=sd(x),n=length(x)))
FB1

FB2 <- do.call(data.frame, FB1)
FB2

FB2$se <- FB2$cc.sd / sqrt(FB2$cc.n)
head(FB2)

library(ggplot2)

bar_plot_ino<- ggplot(FB2, aes(x=FB2$soil, y=FB2$cc.mean, fill=FB2$soil)) + 
  geom_bar(stat="identity", color="black", size=1, position="dodge") +
  geom_errorbar(aes(ymin=FB2$cc.mean-FB2$se, ymax=FB2$cc.mean+FB2$se), width=0.2, size=.5, position=position_dodge(0.9)) +
  annotate("text", label="a", x=1, y=82, size=5) +
  annotate("text", label="a", x=2, y=85, size=5) +
  annotate("text", label="b", x=3, y=88, size=5) +
  annotate("text", label="c", x=4, y=80, size=5) +
  annotate("text", label="c", x=5, y=88, size=5) +
  annotate("text", label="ab", x=6, y=86, size=5) +
  theme_classic() +
  scale_fill_manual(values = palette, name="Inoculum") +
  theme(axis.title=element_text(size=16)) +
  theme(text=element_text(size=16)) +
  theme(legend.text = element_text(color = "black", size = 12)) +
  #theme(axis.line.y=element_line(colour="black", size=1)) + 
  scale_y_continuous(bquote(~'Cumulative CO'[2]~'-C'~(mg~g~dry~wt~litter^-1))) +
  xlab('Inoculum')+
  coord_cartesian(ylim=c(60,90)) 
plot(bar_plot_ino)

#respiration plot for Figure S2

FB1 <- aggregate(cc~litter, data=cc, FUN =function(x) c(mean=mean(x), sd=sd(x),n=length(x)))
FB1

FB2 <- do.call(data.frame, FB1)
FB2

FB2$se <- FB2$cc.sd / sqrt(FB2$cc.n)
head(FB2)

library(ggplot2)

bar_plot_ino<- ggplot(FB2, aes(x=FB2$litter, y=FB2$cc.mean, fill=FB2$litter)) + 
  geom_bar(stat="identity", color="black", size=1, position="dodge") +
  geom_errorbar(aes(ymin=FB2$cc.mean-FB2$se, ymax=FB2$cc.mean+FB2$se), width=0.2, size=.5, position=position_dodge(0.9)) +
  #annotate('text', 2, 120, label="Litter: ~italic(P) < 0.001", size=5, parse=TRUE)+
  annotate("text", label="a", x=1, y=82, size=5) +
  annotate("text", label="a", x=2, y=82, size=5) +
  annotate("text", label="b", x=3, y=62, size=5) +
  annotate("text", label="c", x=4, y=115, size=5) +
  annotate("text", label="d", x=5, y=105, size=5) +
  annotate("text", label="e", x=6, y=65, size=5) +
  theme_classic() +
  scale_fill_manual(values=palette, name="Litter") +
  theme(axis.title=element_text(size=16)) +
  theme(text=element_text(size=16)) +
  theme(legend.text = element_text(color = "black", size = 12)) +
  #theme(axis.line.y=element_line(colour="black", size=1)) + 
  scale_y_continuous(bquote(~'Cumulative CO'[2]~'-C'~(mg~g~dry~wt~litter^-1))) +
  xlab('Litter')
plot(bar_plot_ino)


#Analyze HFA

b <- lm(HFA ~ sample , data=hfa)
qqPlot(resid(b))
Anova(b)
b2 <- emmeans(b, ~ sample)
contrast(b2, method="pairwise")

#HFA box plots

box_plot_hfa <- ggplot(hfa, aes(x=sample, y=HFA, fill=sample)) + 
  geom_hline(yintercept=0, linetype="dotted", color = "black",size=1) +
  stat_boxplot(geom = "errorbar", size=.5,width = 0.5, position=position_dodge(.75)) +
  geom_boxplot(inherit.aes = TRUE,size=.5, outlier.size=2) +
  stat_summary(fun.y=mean,geom="point",shape=23,size=3,color="black",fill="white")+
  annotate('text', x=5, y=17, label="Inoculum: ~italic(P) == 0.309", size=6, parse=TRUE)+
  annotate("text", label="???", x=2, y=13, size=8) +

  labs(list(x ="")) +
  theme_classic() +
  scale_fill_manual(values=palette) +
  theme(axis.title=element_text(size=20)) +
  theme(text=element_text(size=18)) +
  theme(legend.position='none') +
  xlab("Inoculum") +
  theme(axis.line.x=element_line(colour="black", size=1)) + 
  theme(axis.line.y=element_line(colour="black", size=1)) + 
  theme(legend.title=element_blank()) +
  ylab("Home Field Advantage (HFA)") 
plot(box_plot_hfa)

#Analyze FB

c <- lm(FB ~ sample , data=hfa)
qqPlot(resid(c))
Anova(c)
c2 <- emmeans(c, ~ sample)
contrast(c2, method="pairwise")

#FB plot

box_plot_fb <- ggplot(hfa, aes(x=sample, y=FB, fill=sample)) + 
  geom_hline(yintercept=0, linetype="dotted", color = "black",size=1) +
  stat_boxplot(geom = "errorbar", size=.5,width = 0.5, position=position_dodge(.75)) +
  geom_boxplot(inherit.aes = TRUE,size=.5, outlier.size=2) +
  stat_summary(fun.y=mean,geom="point",shape=23,size=3,color="black",fill="white")+
  annotate('text', x=1.7, y=6, label="Inoculum: ~italic(P) < 0.001", size=6, parse=TRUE)+
  annotate('text', x=3, y=7.5, label="*", size=10)+
  annotate('text', x=4, y=-2, label="***", size=10)+
  annotate('text', x=5, y=6, label="*", size=10)+
  labs(list(x ="")) +
  theme_classic() +
  scale_fill_manual(values=palette) +
  theme(axis.title=element_text(size=20)) +
  theme(text=element_text(size=20)) +
  theme(legend.position='none') +
  xlab("Inoculum") +
  theme(axis.line.x=element_line(colour="black", size=1)) + 
  theme(axis.line.y=element_line(colour="black", size=1)) + 
  theme(legend.title=element_blank()) +
  ylab("Functional Breadth (FB)") 

plot(box_plot_fb)

#Analyze QI

d <- lm(QI ~ sample , data=hfa)
qqPlot(resid(d))
Anova(d)
d2 <- emmeans(d, ~ sample)
contrast(d2, method="pairwise")

#FB plot

box_plot_qi <- ggplot(hfa, aes(x=sample, y=QI, fill=sample)) + 
  geom_hline(yintercept=0, linetype="dotted", color = "black",size=1) +
  stat_boxplot(geom = "errorbar", size=.5,width = 0.5, position=position_dodge(.75)) +
  geom_boxplot(inherit.aes = TRUE,size=.5, outlier.size=2) +
  stat_summary(fun.y=mean,geom="point",shape=23,size=3,color="black",fill="white")+
  annotate('text', x=1.5, y=25, label="Litter: ~italic(P) < 0.001", size=6, parse=TRUE)+
  annotate('text', x=1, y=9, label="***", size=10)+
  annotate('text', x=2, y=1, label="***", size=10)+
  annotate('text', x=3, y=-17, label="***", size=10)+
  annotate('text', x=4, y=35, label="***", size=10)+
  annotate('text', x=5, y=27, label="***", size=10)+
  annotate('text', x=6, y=-10, label="***", size=10)+
  labs(list(x ="")) +
  theme_classic() +
  scale_fill_manual(values=palette) +
  theme(axis.title=element_text(size=20)) +
  theme(text=element_text(size=20)) +
  theme(legend.position='none') +
  xlab("Litter") +
  theme(axis.line.x=element_line(colour="black", size=1)) + 
  theme(axis.line.y=element_line(colour="black", size=1)) + 
  theme(legend.title=element_blank()) +
  ylab("Quality Index (QI)") 
plot(box_plot_qi)

library(cowplot)

jpeg(filename="fig2v3.jpeg", bg="transparent", res=500, units = "in", height=15, width=7) 

f3 <- plot_grid(box_plot_hfa, box_plot_fb, box_plot_qi, ncol = 1, labels=c('A', 'B','C'),align="hv", label_size=20)

f3

dev.off()

#HFA and FB plot

multi_ord4 <- aggregate(HFA ~ sample, hfa,FUN= function(x) c(mean=mean(x, na.rm=T), sd=sd(x, na.rm=T), n=length(x)))
multi_ord4

multi_ord5 <- aggregate(FB ~ sample, hfa,FUN= function(x) c(mean=mean(x, na.rm=T), sd=sd(x, na.rm=T), n=length(x)))
multi_ord5

multi_ord4 <- do.call(data.frame, multi_ord4)
multi_ord4

multi_ord5 <- do.call(data.frame, multi_ord5)
multi_ord5

multi_ord6 <- cbind(multi_ord4, multi_ord5[,c(2:4)])
multi_ord6

multi_ord6$HFAse <- multi_ord6$HFA.sd / sqrt(multi_ord6$HFA.n)
multi_ord6

multi_ord6$FBse <- multi_ord6$FB.sd / sqrt(multi_ord6$FB.n)
multi_ord6

multi_ord6$FB.mean2 <- multi_ord6$FB.mean^2

Model<-lm(HFA.mean~FB.mean+FB.mean2, data=multi_ord6)
#HFAdv is home-field advantage
summary(Model)

jpeg(filename="fig3.jpeg", bg="transparent", res=500, units = "in", height=6, width=8) 

ord_2 <- ggplot(multi_ord6, aes(x=FB.mean, y=HFA.mean,fill=sample)) + 
  geom_hline(yintercept=0, linetype="dotted", color = "black",size=1) +
  geom_vline(xintercept=0, linetype="dotted", color = "black",size=1) +
  geom_errorbar(aes(ymin=multi_ord6$HFA.mean-multi_ord6$HFAse, ymax=multi_ord6$HFA.mean+multi_ord6$HFAse), width=0.01, size=.75, position=position_dodge(0.9)) +
  geom_errorbarh(aes(xmin=multi_ord6$FB.mean-multi_ord6$FBse, xmax=multi_ord6$FB.mean+multi_ord6$FBse), height=0.01, size=.75) +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="black",linetype="dashed") +
  geom_point(data=multi_ord6, aes(x=FB.mean,y=HFA.mean,fill=sample),size=5,shape=21) + # add the point markers
  annotate("text", label="R^2 == 0.895", x=-5, y=10, size=6, parse=TRUE) +
  #annotate("text", -5, 8, label="~italic(P) == 0.034", parse=TRUE, size=6, fontface="bold")+
  scale_fill_manual(values=palette) +
  xlab("Functional Breadth") +
  ylab("Home Field Advantage") +
  theme_classic() +
  theme(legend.position = c(0.9, 0.78))+
  theme(axis.text=element_text(size=15)) +
  theme(text=element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill='Inoculum') +
  theme(legend.text=element_text(size=12)) 
ord_2

dev.off()

#Correlation analyses

all <- read.csv("C:/Users/ernie/OneDrive/Desktop/hfa_all.csv",row.names=1)

otu <- read.csv("C:/Users/ernie/OneDrive/Desktop/hfa_otu.csv",row.names=1)

otu2 <- data.frame(t(otu))

rowSums(otu2)

library(vegan)

bac<- data.frame(rrarefy(otu2, 6418))

dist <- vegdist(bac, method="bray")

cap <- cmdscale(dist, k=2, eig=TRUE)

cap1 <- as.data.frame(cap$points)

var1 <- round(cap$eig[1]/sum(cap$eig)*100,1)
var2 <- round(cap$eig[2]/sum(cap$eig)*100,1)

cap1 <- cbind(rownames(cap1), data.frame(cap1, row.names=NULL))

names(cap1)[names(cap1) == "rownames(cap1)"] <- "soil"

cap2 <- cap1[order(cap1$soil),]

all <- cbind(rownames(all), data.frame(all, row.names=NULL))

names(all)[names(all) == "rownames(all)"] <- "soil"

all2 <- all[order(all$soil),]

all2$Bac_PCoA1 <- cap2$V1



div1 <- data.frame(diversity(bac, index = "shannon"))

names(div1)[names(div1) == "diversity.bac..index....shannon.."] <- "shannon"

div1 <- cbind(rownames(div1), data.frame(div1, row.names=NULL))

names(div1)[names(div1) == "rownames(div1)"] <- "soil"
div1<- div1[order(div1$soil),]

all2$Bac_shannon <- div1$shannon

div2 <- data.frame(specnumber(bac))

div2 <- cbind(rownames(div2), data.frame(div2, row.names=NULL))

names(div2)[names(div2) == "rownames(div2)"] <- "soil"
div2<- div2[order(div2$soil),]

names(div2)[names(div2) == "specnumber.bac."] <- "richness"


otu_fung <- read.csv("C:/Users/ernie/OneDrive/Desktop/hfa_otu2.csv",row.names=1)

otu2_fung <- data.frame(t(otu_fung))

library(vegan)

rowSums(otu2_fung)

fung<- data.frame(rrarefy(otu2_fung, 3161))

dist2 <- vegdist(fung, method="bray")

cap_fung <- cmdscale(dist2, k=2, eig=TRUE)

cap1_fung <- as.data.frame(cap_fung$points)

var1 <- round(cap_fung$eig[1]/sum(cap_fung$eig)*100,1)
var2 <- round(cap_fung$eig[2]/sum(cap_fung$eig)*100,1)

cap1_fung <- cbind(rownames(cap1_fung), data.frame(cap1_fung, row.names=NULL))

names(cap1_fung)[names(cap1_fung) == "rownames(cap1_fung)"] <- "soil"

cap2_fung <- cap1_fung[order(cap1_fung$soil),]

all2 <- all2[order(all2$soil),]

all2$Fung_PCoA1 <- cap2_fung$V1

div1_fung <- data.frame(diversity(fung, index = "shannon"))

names(div1_fung)[names(div1_fung) == "diversity.fung..index....shannon.."] <- "shannon"

div1_fung <- cbind(rownames(div1_fung), data.frame(div1_fung, row.names=NULL))

names(div1_fung)[names(div1_fung) == "rownames(div1_fung)"] <- "soil"
div1_fung<- div1_fung[order(div1_fung$soil),]

all2$Fung_shannon <- div1_fung$shannon

div2_fung <- data.frame(specnumber(fung))

div2_fung <- cbind(rownames(div2_fung), data.frame(div2_fung, row.names=NULL))

names(div2_fung)[names(div2_fung) == "rownames(div2_fung)"] <- "soil"
div2_fung<- div2_fung[order(div2_fung$soil),]

names(div2_fung)[names(div2_fung) == "specnumber.fung."] <- "richness"



library(corrplot)
library(RColorBrewer)
scalebluered <- colorRampPalette(brewer.pal(8, "RdBu"))(8)

Corr <- cor(all2[,c(2:23)], use="complete.obs", method="pearson")

res1 <- cor.mtest(all2[,c(2:23)], conf.level = .95)

#jpeg(filename="correlogram.jpeg", bg="transparent", res=500, units = "in", height=5, width=5.5) 

#corrplot(Corr[1:49,1:3, drop=FALSE], method="color", col=scalebluered, addCoef.col = "black",  type="lower", tl.cex=1, tl.col="black", tl.srt=45,sig.level=0.05,p.mat = res1$p, insig = "blank",cl.pos="n")
corrplot(Corr, method="color", col=scalebluered, addCoef.col = "black",  type="lower", tl.cex=1, tl.col="black", tl.srt=45,sig.level=0.05,p.mat = res1$p, insig = "blank",cl.pos="n")
colorlegend(xlim=c(22,23), ylim=c(12,22), scalebluered,c(seq(-1,1,.25)), align="l", vertical=TRUE, addlabels=TRUE)

#dev.off()

cor.test(all2$HFA, all2$SIR)

reg1 <- ggplot(all2, aes(x=HFA, y=SIR)) + 
  geom_smooth(method='lm', colour="black",se=FALSE,linetype="dashed") +
  geom_point(size=4, aes(color=soil)) +   
  scale_color_manual(values=palette, name="Inoculum") +
  scale_y_continuous(bquote(~'SIR'~~ (mu*'g'~'CO'[2]~'-C'~~gdw^-1~~h^-1)))+
  xlab("Home Field Advantage")+
  theme_classic() +
  theme(text=element_text(size=18)) +
  theme(axis.text=element_text(size=18)) +
  theme(legend.position=c(.15,.75)) +
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  theme(axis.line.x=element_line(colour="black", size=.5)) + 
  theme(axis.line.y=element_line(colour="black", size=.5)) +
  annotate("text", label="r == 0.813", x=3, y=1.8, size=6, parse=TRUE) 
reg1

cor.test(all2$HFA, all2$Verrucomicrobia)

reg2 <- ggplot(all2, aes(x=HFA, y=Verrucomicrobia)) + 
  geom_smooth(method='lm', colour="black",se=FALSE,linetype="dashed") +
  geom_point(size=4, aes(color=soil)) +   
  scale_color_manual(values=palette, name="Inoculum") +
  ylab("Verrucomicrobia")+
  xlab("Home Field Advantage")+
  theme_classic() +
  theme(text=element_text(size=18)) +
  theme(axis.text=element_text(size=18)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  theme(axis.line.x=element_line(colour="black", size=.5)) + 
  theme(axis.line.y=element_line(colour="black", size=.5)) +
  annotate("text", label="r == 0.857", x=-.5, y=.105, size=6, parse=TRUE) 
reg2

cor.test(all2$FB, all2$CatEveness)

reg3 <- ggplot(all2, aes(x=FB, y=CatEveness)) + 
  geom_smooth(method='lm', colour="black",se=FALSE,linetype="dashed") +
  geom_point(size=4, aes(color=soil)) +   
  scale_color_manual(values=palette, name="Inoculum") +
  ylab("Catabolic Evenness")+
  xlab("Functional Breadth")+
  theme_classic() +
  theme(text=element_text(size=18)) +
  theme(axis.text=element_text(size=18)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  theme(axis.line.x=element_line(colour="black", size=.5)) + 
  theme(axis.line.y=element_line(colour="black", size=.5)) +
  annotate("text", label="r == 0.818", x=-4.5, y=1.52, size=6, parse=TRUE) 
reg3

cor.test(all2$FB, all2$rk)

reg4 <- ggplot(all2, aes(x=FB, y=rk)) + 
  geom_smooth(method='lm', colour="black",se=FALSE,linetype="dashed") +
  geom_point(size=4, aes(color=soil)) +   
  scale_color_manual(values=palette, name="Inoculum") +
  ylab("Bacterial r:K")+
  xlab("Functional Breadth")+
  theme_classic() +
  theme(text=element_text(size=18)) +
  theme(axis.text=element_text(size=18)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  theme(axis.line.x=element_line(colour="black", size=.5)) + 
  theme(axis.line.y=element_line(colour="black", size=.5)) +
annotate("text", label="r == -0.938", x=1, y=4, size=6, parse=TRUE) 
reg4

jpeg(filename="fig4.jpeg", bg="transparent", res=500, units = "in", height=9, width=11) 

f3 <- plot_grid(reg1, reg3, reg2, reg4, ncol = 2, labels=c('A', 'B', 'C', 'D'),align="hv", label_size=20)

f3

dev.off()
