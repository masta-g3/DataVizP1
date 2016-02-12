library(gdata)
library(ggplot2)
library(plyr)
## Set your own working directory!!!
setwd("../../../Downloads")
survey <- read.xls("Survey+Response.xlsx", stringsAsFactors = F)
#for windows user 
#survey <- read.xls("Survey+Response.xlsx", stringsAsFactors = F,perl="C:\\Perl64\\bin\\perl.exe")
########################
#### DATA CLEANING ####
#######################

## Add ID variable.
survey <- cbind(1:nrow(survey), survey)

## Drop useless columns and rename the good ones.
survey <- survey[,colSums(is.na(survey))<nrow(survey)]
names(survey)
newNames <- c("id", "Waitlist", "Program", "Tools", "R - Data Manipulation", "Gender", "Prof. Editor", "R - Graphic Skills",
              "R - Adv. Multivariate Analysis Skills", "R - Reproducibility Skills", "Matlab Skills", "Github Skills")
names(survey) <- newNames

## Get list of all available "tools".
toolsList <- as.character(survey$Tools)
toolsList <- strsplit(toolsList, ",")
toolsList <- unique(trim(unlist(toolsList)))
toolsList
lg <- length(toolsList)

## Make binary variables for each tool.
test <- data.frame(survey$id, survey$Tools, stringsAsFactors = F)
names(test) <- c("id", "tools")
out <- data.frame(matrix(ncol=lg, nrow=0))
names(out) <- toolsList

make_row = function(x) {
  m <- c(rep(NA,lg))
  names(m) <- toolsList
  v = as.character(unique(trim(unlist(strsplit(x[1,2], ",")))))
  m[v] = "Yes"
  m
}

outFile <- ddply(.data=test, .variables=.(id), .fun=make_row)

surveyNew <- merge(survey, outFile, by="id")
surveyNew <- surveyNew[!(names(surveyNew) %in% c("Tools"))]

## Clean program options.
sort(unique(surveyNew$Program))

surveyNew[surveyNew$Program == "Ms in ds",]$Program <- "IDSE (master)"
surveyNew[surveyNew$Program == "MSDS",]$Program <- "IDSE (master)"
surveyNew[surveyNew$Program == "Data Science",]$Program <- "IDSE (master)"
surveyNew[surveyNew$Program == "QMSS (master)",]$Program <- "QMSS"
surveyNew[surveyNew$Program == "Data Science Certification",]$Program <- "IDSE (certificate)"
surveyNew[surveyNew$Program == "PhD Biomedical Informatics",]$Program <- "Ph.D."

#ggplot(surveyNew, aes(Program, fill=Program)) + geom_bar() +
#  geom_text(aes(label = format(..count.., digits=2, drop0trailing=TRUE), y= ..count.. ), stat= "count", vjust = -.5) +
#  theme(legend.position = "bottom")

## Clean text editor.
sort(unique(surveyNew$`Prof. Editor`))
surveyNew[surveyNew$`Prof. Editor` == "Any (20 years C++/Java experience)",]$`Prof. Editor` <- "Any"
surveyNew[grepl("sublime", surveyNew$`Prof. Editor`, ignore.case=T),]$`Prof. Editor` <- "Sublime"
surveyNew[surveyNew$`Prof. Editor` == "I used jupyter last semester",]$`Prof. Editor` <- "Jupiter"
surveyNew[surveyNew$`Prof. Editor` == "textwrangler",]$`Prof. Editor` <- "textWrangler"
surveyNew[surveyNew$`Prof. Editor` == "Text Wrangler",]$`Prof. Editor` <- "textWrangler"
surveyNew[surveyNew$`Prof. Editor` == "python",]$`Prof. Editor` <- "iPython"
surveyNew[surveyNew$`Prof. Editor` == "ipynb",]$`Prof. Editor` <- "iPython"
surveyNew[surveyNew$`Prof. Editor` == "Webstorm, pycharm",]$`Prof. Editor` <- "iPython"
surveyNew[surveyNew$`Prof. Editor` == "notepad++",]$`Prof. Editor` <- "Notepad++"
surveyNew[surveyNew$`Prof. Editor` == "xcode",]$`Prof. Editor` <- "XCode"
surveyNew[surveyNew$`Prof. Editor` == "vi/vim",]$`Prof. Editor` <- "Vi/Vim"
surveyNew[surveyNew$`Prof. Editor` == "haven't used any",]$`Prof. Editor` <- "None"


## Clean gender.
sort(unique(surveyNew$Gender))
surveyNew[surveyNew$Gender == "",]$Gender <- "he/him"
surveyNew[surveyNew$Gender == "doesn't matter",]$Gender <- "she/her"
surveyNew[surveyNew$Gender == "he/him",]$Gender <- "Male"
surveyNew[surveyNew$Gender == "she/her",]$Gender <- "Female"


#bar chart for Prof Editor
ggplot(surveyNew, aes(`Prof. Editor`, fill=`Prof. Editor`)) + geom_bar() +
  geom_text(aes(label = format(..count.., digits=2, drop0trailing=TRUE), y= ..count.. ), stat= "count", vjust = -.5) +
  theme(legend.position = "bottom")
#waitlist statstic
waitlist=rbind(table(surveyNew$Waitlist))
waitlist
#pie chart for waitlist
waitlist <- table(surveyNew$Waitlist)
lbls <- paste(names(waitlist), "\n", waitlist, sep="")
pie(waitlist, col= c("blue","red"),labels = lbls, main="Waitlist")
#Program statstic
program <- rbind(table(surveyNew$Program))
program
bp_program=barplot(as.matrix(program),main="Students by Program",ylab="numbers",cex.names=0.7,cex.main=1.4,beside = TRUE, col=rainbow(7))
text(x=bp_program, y=program, labels=round(program,0), pos=3, xpd=NA)

#Gender Statistic
gender <-rbind(table(surveyNew$Gender))
gender
#pie chart for Gender
gender <- table(surveyNew$Gender)
lbls <- paste(names(gender), "\n", gender, sep="")
pie(gender, col= c("red","blue"),labels = lbls, main="Gender")



#create matrix for skills with proficiency
skills <- cbind (table(surveyNew$`R - Data Manipulation`),
                        table(surveyNew$`R - Graphic Skills`),
                        table(surveyNew$`R - Adv. Multivariate Analysis Skills`),
                        table(surveyNew$`R - Reproducibility Skills`),
                        table(surveyNew$`Matlab Skills`), 
                        table(surveyNew$`Github Skills`))
#Skills Statistic with proficiency
colnames(skills) <- c("R - Data Manipulation","R - Graphic","R - Adv.Multivariate","R - Reproducibility","Matlab Skills","Github Skills")
skills
#bar chart for skills with proficiency
colours <- c("red", "orange", "blue", "yellow")
bp_skills=barplot(as.matrix(skills), main="skills statistic", ylab = "Numbers",cex.names=0.8, cex.lab = 0.8, cex.main = 1.4, beside=TRUE, col=colours)
legend("topright", c("A little","Confident","Expert","None"), cex=0.8, bty="n", fill=colours)
text(x=bp_skills, y=skills, labels=round(skills,0), pos=3, xpd=NA)

#change "yes" response to 1 and none response to 0
tool_exp <- surveyNew[,12:31]

tool_exp[tool_exp[,]== "Yes"] <- "1"
tool_exp[is.na(tool_exp[,])] <- 0
#transform charactor structure to numeric
tool_exp=as.matrix(sapply(tool_exp, as.numeric))
#statistic table for tools
#colSums() calcuates total students that have experience for each tool
tool_exp <- rbind(colSums(tool_exp)) 
tool_exp
tool_exp_name=names(surveyNew[,12:31])
bp_tool_exp=barplot(as.matrix(tool_exp),main="Experience with Tools Statistic",
                    horiz=TRUE,legend=TRUE,xlab="numbers",names.arg=tool_exp_name, 
                    las=1,cex.names=0.8,xlim=c(0,100),cex.main=1.4,beside = TRUE, col=rainbow(20))
#adjust the margins
par(mar=c(5.1, 13 ,4.1 ,2.1)) 
#adjust font size and numbers positon
text(x=tool_exp, y=bp_tool_exp,cex=0.8,labels=round(tool_exp,0), pos=4, xpd=NA)


