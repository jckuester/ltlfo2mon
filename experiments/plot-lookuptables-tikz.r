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
  output_file_name <- 'experiments_lkt_tikz'
  isStandalone <- FALSE
  tikz(paste(output_file_name,'.tex',sep = ""), standAlone = isStandalone, width=12, height=8)
}

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

lookuptables_sa <- read.table("results/lookuptables_sa.dat", header = FALSE, sep = "\t",  comment.char = "#")
lookuptables_opt <- read.table("results/lookuptables_opt.dat", header = FALSE, sep = "\t",  comment.char = "#")


formula_file <- read.table("results/formulae.dat", header = FALSE, sep = "\t",  comment.char = "#", col.names=c("placeholder", "formula"))

formula <- c(formula_file,formula_file)
grp <- c(rep(1,8),rep(2,8))
lkt_size <- c(lookuptables_sa$V1,lookuptables_opt$V1)
c <- data.frame(formula,grp,lkt_size)


ggplot(c,aes(x=factor(formula,levels=formula_file$formula),y=lkt_size),width=0.1)+
# geom_errorbar(aes(x=factor(formula,levels=formula_file$formula),group=grp,ymin=min,ymax=max), size=1.6,width=0.5,position=position_dodge(width=.4),color="dark gray")+
# geom_boxplot(aes(x=factor(formula,levels=formula_file$formula),group=grp,ymin=min,ymax=max, lower = avg - 10, middle = avg, upper = avg + 10), size=0.05,width=0.1,position=position_dodge(width=5.1),color="dark gray")+
  geom_bar(position=position_dodge(),size = 1,width=0.5 ,stat="identity",colour="black", aes(fill=factor(grp),label=factor(formula)))+
  scale_fill_grey(start = 0, end = .9, labels=c("Precomputed SAs", "Precomputed product automata")) +
  scale_x_discrete(labels=factor(formula_file$placeholder))+
# scale_y_log10(breaks=c(0,1,10,50,100,1000,10000)) +
  scale_y_continuous(trans = 'asinh',breaks=c(0, 1,10,50,100,1000,10000), expand = c(0,0), limits = c(0,3000))+ 
  geom_hline(yintercept = 0,linetype = "dashed",colour="gray")+
  geom_text(aes(x = c(0.85,1.85,2.85,3.85,4.85,5.85,6.85,7.852,1.15,2.15,3.15,4.15,5.15,6.15,7.12,8.15), label = lkt_size, y = lkt_size + c(3,6,8,8,8,25,200,8,1.5,5,10,10,10,25,30,9)), size = 5) +	
  #xlab("Formulae")+
# ylab("$\\Delta$ of monitor size")+
  scale_shape_manual("Trace length",values=c(2,4,19))+
  #geom_text(data = data2.labels, aes(x = formula, y = value, label = label),size=6)+
# coord_flip()+
  theme_bw()+ 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position=c(.2, .8)) +
  theme(axis.text.x = element_text(size = rel(1.8), angle = 00))+
  theme(axis.text.y = element_text(size = rel(1.8), angle = 00))+
  theme(legend.text = element_text(size=rel(1.5)))+
  theme(legend.key = element_blank())+
  theme(legend.title = element_blank())

if(toPdf) {
  dev.off()  # This is only needed if you use pdf/postscript in interactive mode
  texi2pdf(paste(output_file_name,'.tex',sep = ""),clean=TRUE,texi2dvi = getOption("pdflatex"))
  system(paste(getOption('pdfviewer'),paste(output_file_name, '.pdf',sep = "")))
}
