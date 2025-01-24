#!/usr/bin/env Rscript
######################

invisible(lapply(c("lm.beta","e1071","caret","readxl","stringr","tidyr","plyr","dplyr","tidyverse","corrplot","lattice","stringi","parameters","lme4","lmerTest","ggplot2","effectsize","jsonlite"), require, character.only=TRUE))
ROOT<-"/Users/rjirsara/Box Sync/Research/proj24-RiskCalculator"
TODAY<-format(Sys.time(), "%Y%m%d")
theme_set(theme_bw(base_size = 16))

capitalize <- function(x) {
  ifelse(grepl("MRI", x), x, paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x)))))
}

cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

######
### Pairwise Correation Matrix
######

for (FILE in grep("Preproc6_totalprob_future",list.files(ROOT, pattern="XGB.*predict", recursive=TRUE, full.names=TRUE),value=TRUE)){
    DF<-read.csv(FILE); print(paste0("Loaded File: ", FILE))
    TARGET<-unlist(strsplit(basename(dirname(FILE)),"_"))[3]
    FEATURES<-gsub("df-","",unlist(strsplit(basename(FILE),"_"))[1])
    DF[,'featset']<-FEATURES; DF[,'target']<-TARGET
    DF[,'result']<-unlist(strsplit(basename(dirname(FILE)),"_"))[1]
    if (!exists("PREDICT")){
        PREDICT<-DF
    } else {
        PREDICT<-rbind(PREDICT,DF)
    }
}; row.names(PREDICT)<-NULL; PREDICT$featset<-capitalize(PREDICT$featset)
PREDICT<-as.data.frame(pivot_wider(PREDICT, names_from = featset, values_from = predictions))
DF<-read.csv(paste0(ROOT,"/datasets/Preproc4_Primary/n24838x969_multimodal.csv"))
DF<-DF[which(DF$eventname=="baseline_year_1_arm_1"),]
INTERCEPT<-DF[,c("src_subject_id","TARGET_bpm_totalprob_r")]
names(INTERCEPT)<-c("subject", "Baseline Symptoms")
MERGED<-merge(PREDICT, INTERCEPT, by=c("subject"))
CORMAT<-MERGED[,c(
    "targets",
    "Baseline Symptoms",
    "Multimodal",
    "Environmental",
    "Family",
    "Demographics",
    "Adversity", 
    "Behavior", 
    "sMRI", 
    "dMRI",
    "Cognition",
    "fMRI"
)]; names(CORMAT)[1]<-c("Future Symptoms")
r.mat<-as.matrix(cor(CORMAT,use="complete.obs", method = "pearson")) ; p.mat <- cor.mtest(CORMAT)
col <- colorRampPalette(c("#8f1010","#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA","#1c4670"))
col <- colorRampPalette(c("#1c4670","#4477AA","#77AADD","#FFFFFF","#EE9988","#BB4444","#8f1010"))
png(filename = paste0(ROOT, "/Analysis/Visualize/CorMat_FUTURE_",{TODAY},".png"), width = 7.5, height = 7.5, units = "in", res = 300)
corrplot(
  r.mat, 
  method = "color", 
  type = "lower", 
  col = col(100), 
  addCoef.col = "black", 
  tl.col = "black", 
  tl.srt = 45, 
  number.digits = 2,
  p.mat = p.mat, 
  sig.level = 0.99, 
  insig = "blank", 
  diag = TRUE, 
  lwd = 9, 
  addgrid.col = "black"
)
dev.off()

######
### Lineplot of Variance Explained
######

# Define the response variable and potential predictors
STEPWISE<-CORMAT; response <- "Future_Symptoms"
names(STEPWISE)<-gsub(" ", "_",names(STEPWISE))
STEPWISE$Multimodal<-NULL
predictors <- setdiff(names(STEPWISE),response)

# Perform stepwise selection
FINAL<-step(lm(Future_Symptoms ~ ., data = STEPWISE), direction = "both")

# Initialize variables
formula_base <- paste(response, "~ 1")
current_predictors <- character(0)   
remaining_predictors <- predictors   
r_squared_values <- numeric(0)      
steps <- numeric(0)                

# Stepwise selection process
for (step in seq_along(predictors)) {
  # Track the best R-squared and corresponding predictor
  best_r_squared <- -Inf
  best_predictor <- NULL
  
  # Test adding each remaining predictor
  for (predictor in remaining_predictors) {
    # Create the model formula by adding the predictor
    formula <- as.formula(paste(response, "~", paste(c(current_predictors, predictor), collapse = " + ")))
    
    # Fit the model and calculate R-squared
    model <- lm(formula, data = STEPWISE)
    r_squared <- summary(model)$r.squared
    
    # Update the best predictor if R-squared improves
    if (r_squared > best_r_squared) {
      best_r_squared <- r_squared
      best_predictor <- predictor
    }
  }
  
  # Update the model and trackers with the best predictor
  current_predictors <- c(current_predictors, best_predictor)
  remaining_predictors <- setdiff(remaining_predictors, best_predictor)
  r_squared_values <- c(r_squared_values, best_r_squared)
  steps <- c(steps, step)
}

# Combine results into a data frame for plotting
RESULTS <- data.frame(
  Step = steps,
  Predictor = predictors,
  R_squared = r_squared_values
)
RESULTS[which(RESULTS$Predictor %in% names(coef(FINAL))[-1]),"INCL"]<-"YES"
RESULTS[is.na(RESULTS$INCL),"INCL"]<-"NO"

RESULTS$Predictor <- ifelse(
  RESULTS$Predictor == "Baseline_Symptoms", 
  "Baseline Symptoms", 
  gsub("^(.*)", "+\\1", gsub("_", " ", RESULTS$Predictor))
); RESULTS$Predictor<-factor(RESULTS$Predictor,levels=unique(RESULTS$Predictor))
RESULTS$PERCENT<-round(RESULTS$R_squared*100,digits=2)
theme_set(theme_bw(base_size = 16))
F1<-ggplot(RESULTS) + 
    geom_smooth(mapping = aes(x = as.numeric(Predictor), y = PERCENT), 
              method = "gam", 
              formula = y ~ s(x, k = 10), 
              se = FALSE, 
              color="#000000",
              linewidth = 2, 
              alpha = 0.5) + 
    geom_point(mapping = aes(x = Predictor, y = PERCENT), color = "#ffffff", size = 10, shape = 18) + 
    geom_point(mapping = aes(x = Predictor, y = PERCENT, color=INCL), size = 6, shape = 18) + 
    scale_y_continuous(
        breaks = seq(min(RESULTS$PERCENT), max(RESULTS$PERCENT), length.out = 5),
        labels = function(x) format(x, digits = 3, nsmall = 1)) +
    scale_x_discrete(labels = levels(RESULTS$Predictor)) +  
    scale_color_manual(values = c("#b81c1c", "#288c16")) +
    theme(legend.position = "none") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(strip.background = element_rect(colour = "black", fill = "black")) +
    theme(strip.text = element_text(color = "white", face = "bold")) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5)) +
    theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) + 
    theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
    ylab("Percent of Variance Explained in Future Symptoms") +
    xlab("Incremental Addition of New Variables") 
ggsave(plot=F1,filename=paste0(ROOT,"/Analysis/Visualize/F_FUTURE",{TODAY},".png"), device="png", width=15, height=8.5, units='in')
round(summary(FINAL)$adj*100,digits=2)

########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######
####           ⚡     ⚡    ⚡   ⚡   ⚡   ⚡  ⚡  ⚡  ⚡    ⚡  ⚡  ⚡  ⚡   ⚡   ⚡   ⚡    ⚡     ⚡         ####
########⚡⚡⚡⚡⚡⚡#################################⚡⚡⚡⚡⚡⚡################################⚡⚡⚡⚡⚡⚡#######