
library("readr")
library("psych")
library("dplyr")          
library("evd")           
library("apollo")
library("tidyr")
library("kableExtra")
library("tidylog")


simulate_choices <- function(data=database) {
  
  data <-  data %>% 
    group_by(RID) %>% 
    mutate(
      e.1 = rgumbel(setpp,loc=0, scale=1) ,
      e.2 = rgumbel(setpp,loc=0, scale=1) ,
      e.3 = rgumbel(setpp,loc=0, scale=1) ,
      U.1 = V.1 + e.1 ,
      U.2 = V.2 + e.2 ,
      U.3 = V.3 + e.3 
    )   %>% 
    as.data.frame()
  
  data$pref1 <- max.col(data[,c("U.1" , "U.2" , "U.3" )])
  
  return(data)
  
} 


design <- read_delim(designfile,delim = "\t",
                     escape_double = FALSE,
                     trim_ws = TRUE  , 
                     col_select = c(-Design, -starts_with("...")) ,
                     name_repair = "universal") %>% 
  filter(!is.na(Choice.situation))



nsets<-nrow(design)        
nblocks<-max(design$Block)
setpp <- nsets/nblocks      # Choice Sets per respondent; in this 'no blocks' design everyone sees all 24 sets
respondents <- replications*nblocks


database<- design %>%
  arrange(Block,Choice.situation) %>% 
  slice(rep(row_number(), replications)) %>%    ## replicate design according to number of replications
  mutate(RID = rep(1:respondents, each=setpp)) %>%  # create Respondent ID.
  relocate(RID,`Choice.situation`) %>% 
  mutate(
    V.1 = basc + baction*alt1.b + badvisory * alt1.c + bpartner * alt1.d + bcomp * alt1.p , #Utility of alternative 1
    V.2 = basc2 + baction*alt2.b + badvisory * alt2.c + bpartner * alt2.d + bcomp * alt2.p ,  #Utility of alternative 2
    V.3 = 0 ) %>% # utility of opt out, set to zero
  as.data.frame()







models <-list()  # create a list to store all models
results <-data.frame()  # create a list to store all results

for (run in 1:no_sim) {         #start loop
  
  database <- simulate_choices() 
  
  
  
  ##Start estimate models
  
  apollo_initialise()
  
  modelOutput_settings = list(printPVal=T)
  
  ### Set core controls
  apollo_control = list(
    modelName  ="Simulated Data",
    modelDescr ="Simple MNL model",
    indivID    ="RID"
  )
  
  
  apollo_beta=c(b_asc = 0,
                b_asc2 =0,
                b_action = 0,
                b_advisory = 0,      
                b_partner = 0,    
                b_comp = 0 )
  
  ### keine Parameter fix halten
  apollo_fixed = c()
  
  ### validieren
  apollo_inputs = apollo_validateInputs()
  
  apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
    ### Function initialisation: do not change the following three commands
    ### Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    ### Create list of probabilities P
    P = list()
    
    ### List of utilities (later integrated in mnl_settings below)
    V = list()
    V[['alt1']] = b_asc + b_action*alt1.b + b_advisory * alt1.c + b_partner * alt1.d + b_comp * alt1.p
    V[['alt2']] = b_asc2 + b_action*alt2.b + b_advisory * alt2.c + b_partner * alt2.d + b_comp * alt2.p  
    V[['alt3']] = 0
    
    ### Define settings for MNL model component
    mnl_settings = list(
      alternatives  = c(alt1=1, alt2=2, alt3=3) ,
      avail         = 1, # all alternatives are available in every choice
      choiceVar     = pref1,
      V             = V  # tell function to use list vector defined above
      
    )
    
    ### Compute probabilities using MNL model
    P[['model']] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P = apollo_panelProd(P, apollo_inputs, functionality)
    
    ### Average across inter-individual draws - nur bei Mixed Logit!
    ### P = apollo_avgInterDraws(P, apollo_inputs, functionality)
    
    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  
  models[[run]] = apollo_estimate(apollo_beta, apollo_fixed,
                                  apollo_probabilities, apollo_inputs, 
                                  estimate_settings=list(hessianRoutine="maxLik"))
  
  paras <- length(models[[run]]$estimate)*2
  
  results[run,1:paras]<-apollo_modelOutput(models[[run]],modelOutput_settings = list(printPVal=T))[,c(1,7)]
  
  if (run==1) {
    names(results) <- c(rownames(apollo_modelOutput(models[[run]])) ,paste0("pval_",rownames(apollo_modelOutput(models[[run]]))))
  }
  
}


summary <- describe(results, fast = TRUE)

power <- table(results$pval_b_action<0.05 &results$pval_b_advisory<0.05 & results$pval_b_partner<0.05 & results$pval_b_comp<0.05)


print(kable(summary,digits = 2, format = "rst"))


print(power)

designs_all[[designfile]] <- list(models=models,results=results,summary=summary,power=power)
