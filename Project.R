ls()
getwd() ##Show working directory
setwd("/Users/josephwhiting/Desktop/College/Intro to Business Analytics /Project") ##Setting working directory 
library(readr)
library(ggplot2) #loading ggplot

##Loading data set locally form computer, and changing the the variables to numeric and storing it as Project_Dataset
Project_Dataset <- read_csv("Project_Dataset_2.csv", 
                                col_types = cols(Quarters = col_number(), 
                                VOO = col_number(), VGT = col_number(), 
                                VXUS = col_number(), GDP = col_number(), 
                                Unemployment = col_number()))
##Loading data set from Github repository##
Project_Dataset <- read_csv("https://raw.githubusercontent.com/joewhiting/PROJECT-I-ECON-494-F20/main/Project_Dataset_2.csv", 
                            col_types = cols(Quarters = col_number(), 
                                             VOO = col_number(), VGT = col_number(), 
                                             VXUS = col_number(), GDP = col_number(), 
                                             Unemployment = col_number()))
View(Project_Dataset) #view the data set that I am using
summary(Project_Dataset) #looking at summary of variables in the data set 
Project_Dataset_2<- read_csv("Project_Dataset_1.csv")
View(Project_Dataset_2) #view the data set that I am using

###############
##Line Plots##
##############

##Creating Visual of line graph for variable VOO, X-axis being quarters and Y-axis the percentage change from previous quarter
##Added header for graph and renamed the axis, and storing it as VOO_Percent_Change
VOO_Percent_Change<-ggplot(Project_Dataset) +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = VOO), color = "red")+
  xlab('Quarters') + ylab('Returns % change') +
  ggtitle("Vanguard 500 Index Fund ETF") +
  theme(plot.title = element_text(hjust = 0.5)) 
VOO_Percent_Change ##show graph

##Creating Visual of line graph for variable VGT, X-axis being quarters and Y-axis the percentage change from previous quarter
##Added header for graph and renamed the axis, and storing it as VGT_Percent_Change
VGT_Percent_Change<-ggplot(Project_Dataset) +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = VGT), color = "blue")+
  xlab('Quarters') + ylab('Returns % change') +
  ggtitle("Vanguard Information Technology Index") +
  theme(plot.title = element_text(hjust = 0.5)) 
VGT_Percent_Change##Show graph

##Creating Visual of line graph for variable VXUS, X-axis being quarters and Y-axis the percentage change from previous quarter
##Added header for graph and renamed the axis, and storing it as VXUS_Percent_Change
VXUS_Percent_Change<-ggplot(Project_Dataset) +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = VXUS), color = "green")+
  xlab('Quarters') + ylab('Returns % change') +
  ggtitle("Vanguard Total International Stock Index") +
  theme(plot.title = element_text(hjust = 0.5)) 
VXUS_Percent_Change ##Show graph

##Creating Visual of line graph for variable GDP, X-axis being quarters and Y-axis the percentage change from previous quarter
##Added header for graph and renamed the axis, and storing it as GDP_Percent_Change
GDP_Percent_Change<-ggplot(Project_Dataset) +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = GDP), color = "yellow")+
  xlab('Quarters') + ylab('Returns % change') +
  ggtitle("Gross Domestic Product") +
  theme(plot.title = element_text(hjust = 0.5)) 
GDP_Percent_Change ##Show graph

##Creating Visual of line graph for variable Unemployment, X-axis being quarters and Y-axis the percentage change from previous quarter
##Added header for graph and renamed the axis, and storing it as Unemployment_Percent_Change
Unemployment_Percent_Change<-ggplot(Project_Dataset) +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = Unemployment), color = "black")+
  xlab('Quarters') + ylab('Returns % change') +
  ggtitle("Unemployment") +
  theme(plot.title = element_text(hjust = 0.5)) 
Unemployment_Percent_Change ##Show graph

##creating a visual line graph to show how the ETF's performed against each other over the 34 observations, X-axis being quarters and Y-axis the percentage change from previous quarter
##Added header for graph and renamed the axis, stored graph as ETFs_Percent_Change
EFTs_Percent_Change<-ggplot(Project_Dataset) +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = VOO), color = "red") +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = VGT), color = "blue") +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = VXUS), color = "green") +
  labs(x ="Quarters", y= "Returns % Change") +
  ggtitle("ETF's") +
  theme(plot.title = element_text(hjust = 0.5))
EFTs_Percent_Change ##Show graph 

##Creating visual line graph of all variables percentage change, X-axis being quarters and Y-axis the percentage change from previous quarter
##Added header for graph and renamed the axis, stored graph as Dataset_Percent_Change
Dataset_Percent_Change<-ggplot(Project_Dataset) +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = VOO), color = "red") +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = VGT), color = "blue") +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = VXUS), color = "green") +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = GDP), color = "yellow") +
  geom_line(data = Project_Dataset, aes(x = Quarters, y = Unemployment), color = "black") +
  labs(x ="Quarters", y= "Returns % Change") +
  ggtitle("All Variables") +
  theme(plot.title = element_text(hjust = 0.5))
Dataset_Percent_Change ##Show graph

#################
##Scatter Plots##
#################

##Creating scatter plot of VOO variable against VGT variable, added a smooth element to show relationship
##Added title 
VOO_VGT_Relationship<-ggplot(Project_Dataset, aes(VOO, VGT)) + geom_point()+ geom_smooth() +
  ggtitle("VOO vs VGT") +
  theme(plot.title = element_text(hjust = 0.5)) 
VOO_VGT_Relationship ##Show graph

##Creating scatter plot of VOO variable against VXUS variable, added a smooth element to show relationship
##Added title 
VOO_VXUS_Relationship<-ggplot(Project_Dataset, aes(VOO, VXUS)) + geom_point()+ geom_smooth() +
  ggtitle("VOO vs VXUS") +
  theme(plot.title = element_text(hjust = 0.5)) 
VOO_VXUS_Relationship ##Show graph

##Creating scatter plot of VOO variable against GDP variable, added a smooth element to show relationship
##Added title 
VOO_GDP_Relationship<-ggplot(Project_Dataset, aes(VOO, GDP)) + geom_point()+ geom_smooth() +
  ggtitle("VOO vs GDP") +
  theme(plot.title = element_text(hjust = 0.5)) 
VOO_GDP_Relationship ##Show graph 

##Creating scatter plot of VOO variable against Unemployment variable, added a smooth element to show relationship
##Added title 
VOO_Unemployment_Relationship<-ggplot(Project_Dataset, aes(VOO, Unemployment)) + geom_point()+ geom_smooth() +
  ggtitle("VOO vs Unemployment") +
  theme(plot.title = element_text(hjust = 0.5)) 
VOO_Unemployment_Relationship ##Show graph

##Creating scatter plot of VOO variable against VOO variable, added a smooth element to show relationship
##Added title 
##Used to show that the relationships are a valid visual 
VOO_VOO_Relationship<-ggplot(Project_Dataset, aes(VOO, VOO)) + geom_point()+ geom_smooth() +
  ggtitle("VOO vs VOO") +
  theme(plot.title = element_text(hjust = 0.5)) 
VOO_VOO_Relationship ##Show graph

##Creating scatter plot of VXUS variable against GDP variable, added a smooth element to show relationship
##Added title 
VXUS_GDP_Relationship<-ggplot(Project_Dataset, aes(VXUS, GDP)) + geom_point()+ geom_smooth() +
  ggtitle("VXUS vs GDP") +
  theme(plot.title = element_text(hjust = 0.5)) 
VXUS_GDP_Relationship ##Show graph

##Creating scatter plot of VXUS variable against Unemployment variable, added a smooth element to show relationship
##Added title 
VXUS_Unemployment_Relationship<-ggplot(Project_Dataset, aes(VXUS, Unemployment)) + geom_point()+ geom_smooth() +
  ggtitle("VXUS vs Unemployment") +
  theme(plot.title = element_text(hjust = 0.5)) 
VXUS_Unemployment_Relationship ##Show graph

#################################
##Looking at President variable##
#################################
mean(Project_Dataset$VOO[Project_Dataset$President=="Donald Trump"]) ##what is the mean return for VOO while Trump was President 
mean(Project_Dataset$VOO[Project_Dataset$President!="Donald Trump"]) ##what is the mean return for VOO while Obama was President 

mean(Project_Dataset$VGT[Project_Dataset$President=="Donald Trump"]) ##what is the mean return for VGT while Trump was President
mean(Project_Dataset$VGT[Project_Dataset$President!="Donald Trump"]) ##what is the mean return for VGT while Obama was President 

mean(Project_Dataset$VXUS[Project_Dataset$President=="Donald Trump"]) ##what is the mean return for VXUS while Trump was President 
mean(Project_Dataset$VXUS[Project_Dataset$President!="Donald Trump"]) ##what is the mean return for VXUS while Obama was President 

mean(Project_Dataset$GDP[Project_Dataset$President=="Donald Trump"]) ##what is the mean percentage change for GDP while Trump was President 
mean(Project_Dataset$GDP[Project_Dataset$President!="Donald Trump"]) ##what is the mean percentage change for GDP while Obama was President

mean(Project_Dataset$Unemployment[Project_Dataset$President=="Donald Trump"]) 
mean(Project_Dataset$Unemployment[Project_Dataset$President!="Donald Trump"]) 
##what is the mean percentage change for Unemployment while Trump was President 
##what is the mean percentage change for Unemployment while Obama was President 

####################################
##Histograms created, but not used##
####################################

##Creating histogram to show distribution of returns of VOO, added title, and vertical line at mean
VOO_Distribution<-ggplot(Project_Dataset, aes(VOO)) + 
  geom_histogram(fill = "red") +
  ggtitle("VOO Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = mean(Project_Dataset$VOO))
VOO_Distribution ##Show Histogram 

##Creating histogram to show distribution of returns of VGT, added title, and vertical line at mean
VGT_Distribution<-ggplot(Project_Dataset, aes(VGT)) + 
  geom_histogram(fill = "blue") +
  ggtitle("VGT Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = mean(Project_Dataset$VGT))
VGT_Distribution ##Show histogram

##Creating histogram to show distribution of returns of VXUS, added title, and vertical line at mean
VXUS_Distribution<-ggplot(Project_Dataset, aes(VXUS)) + 
  geom_histogram(fill = "green") +
  ggtitle("VXUS Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = mean(Project_Dataset$VXUS))
VXUS_Distribution ##Show histogram

##Creating histogram to show distribution of returns of GDP, added title, and vertical line at mean
GDP_Distribution<-ggplot(Project_Dataset, aes(GDP)) + 
  geom_histogram(fill = "yellow") +
  ggtitle("GDP Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = mean(Project_Dataset$GDP))
GDP_Distribution ##Show histogram

##Creating histogram to show distribution of returns of Unemployment, added title, and vertical line at mean
Unemployment_Distribution<-ggplot(Project_Dataset, aes(Unemployment)) + 
  geom_histogram(fill = "black") +
  ggtitle("Unemployment Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = mean(Project_Dataset$Unemployment))
Unemployment_Distribution ##Show histogram
