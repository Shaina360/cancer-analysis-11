library("caTools")
library("ggplot2")
library("corrplot")
cancer <- read.csv(choose.files(), header=TRUE)
cancer
head(cancer)
str(cancer)
dim(cancer)
glimpse(cancer)
class(cancer)
cancer <- cancer[-33]
summary(cancer)

# no of women effected by cancer
cancer %>% count(diagnosis)

#percentage of women effected in begnin and malignant stage
cancer %>% count(diagnosis)%>%group_by(diagnosis)%>%
  summarise(perc_dx = round((n /569)*100,2))

# data visualization 
# frequency of cancer diagnosis 
diagnosis.table <- table(cancer$diagnosis)
colors<- terrain.colors(2)

#diagnosis.prop.table that calculates proportions of tabular data
#pie chart
diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
pielables <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")
pie(diagnosis.prop.table,lables = pielables, clockwise = TRUE,col = colors,
    border = "gainsboro",radius = 0.8,cex = 0.8, main = "frequency of cancer diagnosis")
legend(1, .4, legend = diagnosis.prop.df[,1],cex = 0.7, fill = colors)


 # correlation plot
# calculation collineraity(hcl for cluster)
c <- cor(cancer[3:31])
corrplot(c, order = "hclust", tl.cex=0.7)

# comparing the radious , area concavity of begnin and malignant stage
ggplot(cancer, aes(x=diagnosis, y=radius_mean,fill="pink"))+geom_boxplot(fill="yellow")+ggtitle("radius_mean of begnin")
gplot(cancer, aes(x=diagnosis, y=area_mean,fill="pink"))=geom_boxplot()+ggtitle("area of brgnin vs malignant")
#

#barplot
ggplot(cancer,aes(x=diagnosis, fill = texture_mean,))+geom_bar()+ggtitle("women effected  in begnin")

# women affected at higher level based on mean from the analysis of boxplot
sel_data<- cancer[cancer$radius_mean]>10&cancer$radius_mean<cancer$compactness_mean>10
 
#dwnsity plot based on texture and mean
ggplot(cancer, aes(x=texture_mean,fill=as.factor(diagnosis)))+geom_density()ggtitle("texture mean")


#histogram
gglpot(cancer, aes(x= concavity_mean,fill=diagnosis))+geom_histogram(binwidth =5)+ggtitle("concavity mean")
gglpot(cancer, aes(x= texture_se))+facet_wrap(~diagnosis)+ggtitle
gglpot(cancer, aes(x= perimeter_mean))+geom_histogram(binwith =15)+ facet_waro(~diagnosis)+ggtitle("perimeter mean for beginin and malignant")                    
