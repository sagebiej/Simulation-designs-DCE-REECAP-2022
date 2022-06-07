## This script sets the core parameters for a simulation and allows the user to test several designs without much recoding.
rm(list=ls())

set.seed(4894949)

designs_all <- list()

# location of design
designfile <- "R/Designs/befficientdesign.ngd"


# betaparameters

basc = -1.2
basc2 = -1.4
baction = 0.1
badvisory = 0.4 
bpartner = 0.3
bcomp = 0.02

replications <-500    # number of replications of the design in the simulation. You can increase this number later on and see how the standard errors go down
no_sim <-500          # how often do you want to repeat the simulation. Once the code runs, go for 100 or more






#source("R/simulationcore.R")


## and now for all designs

for (designpath in list.files("R/Designs/",full.names = T)) {
  
designfile<-  designpath

source("R/simulationcore.R")
  
}

summa <- lapply(designs_all, "[[", "summary")    # extract summary list element from designs_all list


comparestats=data.frame(beff=summa$`R/Designs//befficientdesign.ngd`[c("mean","sd")],     # make dataframe from summary list
             eff=summa$`R/Designs//efficientdesign.ngd`[c("mean","sd")],
             orth=summa$`R/Designs//orthodesign.ngd`[c("mean","sd")], 
             bad=summa$`R/Designs//verybaddesign.ngd`[c("mean","sd")]) %>% 
  relocate(contains("mean"))



comparestats%>% 
  kable(digits = 2, format = "rst")






library("ggplot2")



plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    #geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.5) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
}


meandata <- lapply(designs_all, "[[", "results")


s<-as.data.frame(meandata) %>% 
  select(!matches("pval")) %>%
  rename_with(~ sub("R.Designs..", "", .x), everything()) %>%   #be careful, in some RStudio versions, there is only one "."
  rename_with(~ sub("ngd.b.", "", .x), everything()) %>%
  rename_with( ~ paste0(.,".",stringr::str_extract(.,"(^....)" )), everything() ) %>%   # rename attributes for reshape part 1
 rename_with( ~ stringr::str_replace(.,pattern = "[a-z]+\\.",""), everything() )  %>% 
  reshape(varying =1:24, sep = "."  , direction = "long" ,timevar = "design", idvar = "run" )
  


plot_multi_histogram(s,"asc","design")

plot_multi_histogram(s,"asc2","design") 

plot_multi_histogram(s,"action","design") 

plot_multi_histogram(s,"advisory","design") 

plot_multi_histogram(s,"partner","design")

plot_multi_histogram(s,"comp","design") 
 


