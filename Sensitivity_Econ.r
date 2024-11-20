#Procedures to decompose an adjacency list into a weighted graph
packages = c("splitstackshape", "stringr", "igraph", "spdep", "magrittr", "htmlwidgets", "htmltools", "plotly")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

a<-read.csv("https://raw.githubusercontent.com/democratizing-data-science/solo/refs/heads/main/Econscopus.csv")
table(a$Authors=="")
table(a$Authors!="[No author id available]")
a <- a[a$Authors!="",]

#Keeping only column with author relationships
authors<-as.data.frame(a[,1])#"Author.s..ID"])
colnames(authors)<-"AU"

#package for first split
# install.packages("splitstackshape")
 authors$AU <- str_remove_all(authors$AU, "[^[\\a-zA-Z ]]")

#As can be seen in the file the separator of interest is :
a1<-cSplit(authors, splitCols = "AU", sep = ";", direction = "wide", drop = TRUE) #retain the matrix form version of the adjacency list input
#solo authors
a1 <- as.matrix(a1)
a$author_number <- ncol(a1)-rowSums(is.na(a1))
a$author_binary <- ifelse(a$author_number>1, 0, 1)
summary(lm(Cited.by~author_binary, data=a))

#Load without selfcitation
b <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/solo/refs/heads/main/econallPHUDCFILY.csv")
a$no_self <- b$Total[match(a$ISSN, b$ISSN)]
a$no_self1 <- b$Total[match(a$Title, b$Document.Title)]
head(a[,c("no_self1","cit","author_binary")],100)

a <- a[!is.na(a$no_self1),]
table(is.na(a$no_self1), a$author_binary) 
summary(lm(Cited.by~author_binary, data=a))
summary(lm(no_self1~author_binary, data=a))

a$inflation <- a$Cited.by -  a$no_self1
a <- a[a$inflation>=0,]

summary(lm(Cited.by~author_binary+author_number, data=a))
summary(lm(no_self1~author_binary+author_number, data=a))

a$Open.Access <- ifelse(a$Open.Access=="", 0, 1)
summary(lm(Cited.by~author_binary+Open.Access, data=a))
