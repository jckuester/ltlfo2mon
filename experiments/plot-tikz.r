library(reshape)
library(tools)
library(Hmisc)
library(ggplot2)
library(scales)
library(gridExtra)

toPdf <- TRUE
#x11()
if(toPdf) {
  require(tikzDevice)
  options (tikzLatexPackages = c(
             getOption ("tikzLatexPackages"),
             "\\usepackage{amsmath}",
             "\\newcommand{\\ltlG}{\\mathsf{G}}",
             "\\newcommand{\\ltlF}{\\mathsf{F}}",
             "\\newcommand{\\ltlU}{\\mathsf{U}}",
             "\\newcommand{\\ltlW}{\\mathsf{W}}",
             "\\newcommand{\\ltlX}{\\mathsf{X}}"))
  output_file_name <- 'experiments_tikz'
  isStandalone <- TRUE
  tikz(paste(output_file_name,'.tex',sep = ""), standAlone = isStandalone, width=12, height=8)
}

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

# read files
flist_progression <- list.files(path="results/", pattern = "progression.dat$",full.names = TRUE)
flist_SAbased <- list.files(path="results/", pattern = "sa.dat$",full.names = TRUE)
formula_file <- read.table("results/formulae.dat", header = FALSE, sep = "\t",  comment.char = "#", col.names=c("placeholder", "sa-cache", "formula"))
numfiles <- length(flist_progression)  

max100 <- vector()
max1000 <- vector()
max10000 <- vector()
min100 <- vector()
min1000 <- vector()
min10000 <- vector()
avg100 <- vector()
avg1000 <- vector()
avg10000 <- vector()

i <-1  
for (i in c(1:numfiles)){  
  # Load data
  data_progression <- read.table(flist_progression[i], header = FALSE, sep = "\t",  comment.char = "#", col.names=c("index","event-size","size"))
  data_SAbased <- read.table(flist_SAbased[i], header = FALSE, sep = "\t",  comment.char = "#", col.names=c("index","event-size","size","num-submons","max-runs", "max-run-length","max-obl-length"))

  diff <- data_progression$size - data_SAbased$size 
  max100[i] <- max(diff[1:100]) - formula_file$sa.cache[i]
  max1000[i] <- max(diff[1:1000]) - formula_file$sa.cache[i]
  max10000[i] <- max(diff[1:10000])- formula_file$sa.cache[i]
  min100[i] <- min(diff[1:100])- formula_file$sa.cache[i]
  min1000[i] <- min(diff[1:1000])- formula_file$sa.cache[i]
  min10000[i] <- min(diff[1:10000])- formula_file$sa.cache[i]
  avg100[i] <- mean(diff[1:100])- formula_file$sa.cache[i]
  avg1000[i] <- mean(diff[1:1000])- formula_file$sa.cache[i]
  avg10000[i] <- mean(diff[1:10000])- formula_file$sa.cache[i]
  
  # trace length
  trace_len <- length(data_SAbased$size)
}

formula <- c(formula_file,formula_file,formula_file)

grp <- c(rep(100,numfiles),rep(1000,numfiles),rep(10000,numfiles))
avg <- c(avg100,avg1000,avg10000)
min <- c(min100,min1000,min10000)
max <- c(max100,max1000,max10000)
c <- data.frame(formula,grp,avg,min,max)

formula_factor <- factor(formula, levels=formula_file$formula)

# label data for geom_text
data2.labels <- data.frame(
  formula = c(1:length(formula_file$formula))+0.38,
  value = c(-5,500,13,300,-5,5,7.4,150),
  label = formula_file$formula)

ggplot(c,aes(x=factor(formula,levels=formula_file$formula),y=avg),width=0.1)+
  geom_errorbar(aes(x=factor(formula,levels=formula_file$formula),group=grp,ymin=min,ymax=max), size=1.6,width=0.5,position=position_dodge(width=.4),color="dark gray")+
  geom_point(position=position_dodge(width=.4),size = 3.5,width=1,stat="identity",aes(shape=factor(grp),label=factor(formula)))+
  scale_x_discrete(labels=factor(formula_file$placeholder))+
  scale_y_continuous(trans = 'asinh',breaks=c(-1000,-100,-50,-10,-1,0,1,10,50,100,1000,10000))+ 
  geom_hline(yintercept = 0,linetype = "dashed",colour="gray")+
  xlab("Formulae")+
  ylab("$\\Delta$ of monitor size")+
  scale_shape_manual("Trace length",values=c(2,4,19))+
  geom_text(data = data2.labels, aes(x = formula, y = value, label = label),size=6)+
  coord_flip()+
  theme_bw()+ 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position=c(.1, .25)) +
  theme(axis.text.x = element_text(size = rel(1.8), angle = 00))+
  theme(axis.text.y = element_text(size = rel(1.8), angle = 00))+
  theme(legend.text = element_text(size=rel(1.5)))+
  opts(legend.key = theme_blank())+
  theme(legend.title = element_text(size=18, face="bold"))

if(toPdf) {
  dev.off()  # This is only needed if you use pdf/postscript in interactive mode
  texi2pdf(paste(output_file_name,'.tex',sep = ""),clean=TRUE,texi2dvi = getOption("pdflatex"))
  system(paste(getOption('pdfviewer'),paste(output_file_name, '.pdf',sep = "")))
}
