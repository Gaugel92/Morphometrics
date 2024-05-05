library(ggplot2)
library(tidyverse)
library(cowplot)
library(multcompView)
library(MASS)
library(caret)
library(vegan)
library(mvnormtest)
#1) first create species as a factor in the order of the species IDs of the dataset
species <- factor(c("C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. mollis mollis",
                    "C. mollis ignifer",
                    "C. mollis mollis",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. mollis ignifer",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus ab. ticino",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. mollis mollis",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. biguttulus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus",
                    "C. brunneus"
                    
                    
                    
                    
                    
                    
))


#2) read the data (in a txt file)
TM<-read.table("alps_final.txt",stringsAsFactors = TRUE, header=TRUE, sep="\t")


#3) for PCA, include only measurements, no meta data, so columns between 5 and 17
TM1<-TM[5:17]

#4) PCAs###
t.pca <- prcomp(TM1, center=TRUE, scale.=TRUE)

# add the species to the pca object
t.pca$species<-species

#5) do anova and a post hoc tukey test to a) check if there are any differences 
#in any groups and b) between which groups are differences

###Tukey test and anova: from first 5 axes 
par(mfrow=c(1,1))
for (i in 1:5){
  scores <- t.pca$x[ , i]   
  loadings <- t.pca$rotation
  tukey_data <- data.frame(scores, t.pca$species)
  
  anova <- aov(scores~t.pca$species, data=tukey_data)
  
  
  head(tukey_data)
  summary(anova)
  
  TUKEY<-TukeyHSD(anova, conf.level=.95) 
  library(multcompView)
  plot(TukeyHSD(anova, conf.level=.95), las = 2)
  
  
  generate_label_df <- function(TUKEY, variable){
    
    # Extract labels and factor levels from Tukey post-hoc 
    Tukey.levels <- TUKEY[[variable]][,4]
    Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
    
    #I need to put the labels in the same order as in the boxplot :
    Tukey.labels$treatment=rownames(Tukey.labels)
    Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
    return(Tukey.labels)
  }
  
  # Apply the function on my dataset
  LABELS <- generate_label_df(TUKEY , "t.pca$species")
  
  #make boxplot with the tukey range groups
  library(ggplot2)
  library(dplyr)
  
  
  df3 <- tukey_data %>% 
    left_join(LABELS, by = c("t.pca.species" = "treatment"))
  head(df3)
  
  
  name <- colnames(t.pca$x)[i] # name of the PCA
  p <- df3 %>% 
    ggplot(aes(x = t.pca.species, y = scores)) +
    geom_boxplot(aes(fill = Letters),alpha=0.5)+
    #geom_violin()+
    geom_point(alpha=0.5)+
    xlab("species")+
    guides(fill=guide_legend(title="groups"))+
    ggtitle(name)+
    theme(axis.text.x = element_text(face = "italic", size=7)) 
  dir <- "C:/Users/sarah/OneDrive/Desktop/Uni/Uni Hohenheim/Master Landscape Ecology/Master thesis/Masks/figures/PCAs"
  plot(p)
  ggsave(paste0(dir,"/",name,".png"),
         device = "png",
         dpi = 300, width = 20, height = 10, units = "cm")
  
}



#6) make data frame
df1 <- data.frame(t.pca$x) 
df1$species<-species
t.pca$sdev^2###print numbers of PCs

#optional: make loadings to see which traits affect which axes of the PCA
PCAloadings1<-data.frame(Variables=rownames(t.pca$rotation),t.pca$rotation)


###########plotting the PCA###############
pt1<-df1 %>% 
  ggplot(aes(PC2, PC1, color = species), alpha = 0.8)+
  geom_point(size = 3, alpha = 0.7)+
  scale_color_manual(values = c("C. brunneus"="#FFD700","C. biguttulus"="#800080","C. mollis mollis"="#056608","C. brunneus ab. ticino"="#B4641E","C. mollis ignifer"="#81D170"))+
  scale_fill_manual(values = c("C. brunneus"="#FFD700","C. biguttulus"="#800080","C. mollis mollis"="#056608","C. brunneus ab. ticino"="#B4641E","C. mollis ignifer"="#81D170"))+
  labs(y="PC1: 6.4%" , x = "PC2: 2.9%")+
  stat_ellipse(geom="polygon",aes(fill = species),alpha=0.1,linetype=1, lwd=1.2)+
  guides(color = guide_legend(override.aes = list(linetype = c(0,0,0,0,0) )))+
  theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5))))+
  theme(legend.background=element_blank())#+


####optional: showing loadings
  
#geom_segment(data=PCAloadings1, aes(x=0, y=0, xend=(PC2*10), 
                           #        yend=(PC1*10)), arrow=arrow(length=unit(1, "picas")),
           #  color="black")+ 
  #annotate("text", x=(PCAloadings1$PC2*10), y=(PCAloadings1$PC1*10), 
        #   label=PCAloadings1$Variables)+
# theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5))))




#############density plot for traditional morphometrics
ydens <-
  axis_canvas(pt1, axis = "y", coord_flip = TRUE) + 
  geom_density(data = df1, aes(x = PC1, fill = species, colour = species), alpha = 0.2) +
  coord_flip()+
  scale_color_manual(values = c("C. brunneus"="#FFD700","C. biguttulus"="#800080","C. mollis mollis"="#056608","C. brunneus ab. ticino"="#B4641E","C. mollis ignifer"="#81D170"))+
  scale_fill_manual(values = c("C. brunneus"="#FFD700","C. biguttulus"="#800080","C. mollis mollis"="#056608","C. brunneus ab. ticino"="#B4641E","C. mollis ignifer"="#81D170"))
pt1 %>%
  #insert_xaxis_grob(xdens, grid::unit(1, "in"), position = "top") %>%
  insert_yaxis_grob(ydens, grid::unit(1, "in"), position = "right") %>%
  ggdraw()




#LDA
#multicollinearity tests

#1 make a correlation matrix of the data
as.dist(cor(TM1))

###2look at variance inflation factor (VIF); if >3.5, drop variable
faraway::vif(TM1)


####3 drop all multicollinear variables, keep only 5 uncorrelated variables
TM2<-TM[12:16]
as.dist(cor(TM2))
faraway::vif(TM2)

###now vif factor is only between 1 and 3.4, which is acceptable for me


#####4 assumptions check: homogeneity test (homogeneity of variance using euclidean distances 
#and formation of 5 groups)

t.hle<-decostand(as.matrix(TM2), "hellinger")
gr<-cutree(hclust(vegdist(t.hle, "euc"), "ward.D"), 5)
table(gr)
t.pars<-as.matrix(TM2)
t.pars.d<-dist(t.pars)
t.MHV<-betadisper(t.pars.d,TM$species)
anova(t.MHV)


####we can accept the homogeneity assumption, as p > 0.05


####5 normality
TM.transformed.pars2<-as.matrix(TM2)
TM.transformed.pars.d2<-dist(TM.transformed.pars2)
TM.transformed.MHV2<-betadisper(TM.transformed.pars.d2, TM$species)

par(mfrow=c(1,ncol(TM.transformed.pars2)))
for(j in 1:ncol(TM.transformed.pars2)){
  hist(TM.transformed.pars2[,j])
}
par(mar=c(1,1,1,1))


mshapiro.test(t(TM.transformed.pars2))


#####not normally distributed data according to shapiro test, 
#but large sample size justifies a linear model here



####linearity check (optional)
#library(psych)
#pairs.panels(TM2, gap=0, bg=c("red","blue","green", "yellow", "purple")[TM$species], pch=21)

####some non-linear relationships exist 




##create a new pca but with only non multicollinear data
t.pca2 <- prcomp(TM2, center=TRUE, scale.=TRUE)
df2 <- data.frame(t.pca2$x) 
df2$species<-species


#####LDA

# LDA model
# NOTE: when creating training and test data sets, the results can vary as the selection of those is random. 
#   1.) split data into training dataset and test dataset
trainIndex <- createDataPartition(df2$species, 
                                  p = 0.8, 
                                  list = FALSE) 

train <- df2[trainIndex, ] 
test <- df2[-trainIndex, ] 

# 2.) Fit an lda Model using training data. the numbers are prior probabilities, i.e. 
#proportions of each species in the whole dataset
model <- lda(species ~ ., data = train, prior = c(0.173,0.297,0.273,0.148,0.109))



####3.) predict both the training data and the test data
predict.train<-predict(model, train)
predicted <- predict(model, newdata = test)  


#####4.) test how well LDA performs in class accuracy
confusionMatrix(predicted$class, test$species) 


####gives an estimate on the mean error (model accuracy)
mean(predict.train$class == train$species)
mean(predicted$class == test$species)

#the test dataset had a slightly higher error (65% vs. 61% accuracy)

####5.) confusion matrix for training error
lda_prediction<-predict(model)
conf<-table(list(predicted=predict.train$class,observed=train$species))

#6.) precision (positive predicted value)
diag(conf)/rowSums(conf)

#7.) sensitivity
diag(conf)/colSums(conf)

confusionMatrix(conf)

#####8.) test error
lda_model2<-lda(species~., data=train, CV=T)
conf2<-table(list(predicted=lda_model2$class,observed=train$species))



####9.) use all data to show it on the plot
lda.pred<-predict(model, df2)
data.lda.tm <- data.frame(lda.pred$x[,1], lda.pred$x[,2], df2$species)


#10.) calculate LD values
(model$svd^2 / sum(model$svd^2))*100
#11.) add loadings of the PCA here
LDAloadings_TM<-data.frame(Variables=rownames(model$scaling),model$scaling)


#####12.) plotting of LDA
library(Momocs)

t.lda<-LDA(t.pca2$x, species, retain=0.95)
dfs<-data.frame(t.lda$mod.pred)

lda.plot <- dfs%>% 
  ggplot(aes(x = x.LD1, y = x.LD2)) +
  geom_point(aes(color = species, shape = species), size = 3) +
  scale_shape_manual(values = c("C. brunneus" = 17, "C. biguttulus" = 17, "C. mollis mollis" = 17, "C. brunneus ab. ticino" = 17, "C. mollis ignifer" = 17))+
  scale_color_manual(values = c("C. brunneus" = "#FFD700", "C. biguttulus" = "#800080", "C. mollis mollis" = "#056608", "C. brunneus ab. ticino" = "#B4641E", "C. mollis ignifer" = "#81D170")) +
  scale_fill_manual(values = c("C. brunneus"="#FFD700","C. biguttulus"="#800080","C. mollis mollis"="#056608","C. brunneus ab. ticino"="#B4641E","C. mollis ignifer"="#81D170"))+
  labs(y = "LD2: 6.51%", x = "LD1: 91.9%") +
  stat_ellipse(aes(fill = species, color = species), geom = "polygon", alpha = 0.1, linetype = 1, lwd = 1.2) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0, 0, 0, 0)))) +
  theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5)))) +
  theme(legend.background = element_blank())+ 
  geom_segment(data=LDAloadings_TM, aes(x=0, y=0, xend=(LD1*2), 
                                         yend=(LD2*2)), arrow=arrow(length=unit(1, "picas")),
            color="black")+
 annotate("text", x=(LDAloadings_TM$LD1*2), y=(LDAloadings_TM$LD2*2), 
          label=LDAloadings_TM$Variables)+
 theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5))))



#13.) heat map for visualizing  the class errors
plot_CV(t.lda, rm0=FALSE, fill=TRUE)
