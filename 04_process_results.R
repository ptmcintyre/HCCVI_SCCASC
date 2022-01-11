
# this script processes the raw outputs of the veg variable importance script,
# generating some visualizations and saving a csv file for other scripts to reference.


library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

# open rfe data
d <- list.files(here("type_specific_modeling/variable_selection/rfe_results"), full.names=T)
d <- d[grepl(".rds", d)]
#d<-d[c(1,14 )]
## VARIABLE RANKS ###############################

culls <- lapply(d, function(x) {
      x <- readRDS(x)
      if(length(x)==2) return(x[[1]])})
culls <- do.call("rbind", culls)
veg <- basename(d)
rownames(culls) <- 1:nrow(culls)

vars <- paste0("bio", 1:19)
culls <- apply(culls, 1, function(x) match(vars, x))
culls <- t(culls)
colnames(culls) <- vars

# low rank number means low importance
ranks <- apply(culls, 2, mean)
culls <- culls[,order(ranks)]

culls <- as.data.frame(culls)
culls$veg <- sub(".rds", "", veg)
culls$veg <- sub("rfe_", "", culls$veg)
culls$veg <- gsub("_", " ", culls$veg)

# sort veg types by similarity of variable preferences

###pjm- row names not returngin from corrplot, troubleshooting
#vegorder <- as.integer(rownames(corrplot::corrplot(cor(t(culls[,-20])), order="FPC"))) #not working, brokne to 2 parts, PJM
veg.cor<-corrplot::corrplot(cor(t(culls[,-20])), order="FPC") #pjm part 1, run corrplot
vegorder <- as.integer(rownames(veg.cor$corr)) #grab row order from $corr

#varorder <- rownames(corrplot::corrplot(cor(culls[,-20]), order="FPC"))#not working, brokne to 2 parts, PJM
var.cor<-corrplot::corrplot(cor(culls[,-20]), order="FPC")
varorder <- rownames(var.cor$corr)


veglevels <- as.character(culls$veg[vegorder])
culls$veg <- factor(culls$veg, levels=culls$veg[vegorder])

tculls <- culls %>%
      gather(var, rank, -veg) %>%
      mutate(variable = ecoclim::translate(as.character(var), "words"),
             type = veg) %>%
      dplyr::select(type, rank, var, variable) %>%
      arrange(type, desc(rank))

write.csv(tculls, 
          here("type_specific_modeling/variable_selection/variable_importance.csv"),
          row.names=F)



## DOTPLOTS #################################

ggsave(here("type_specific_modeling/variable_selection/charts/rank_scatter.png"),
       ggplot(tculls, aes(rank, var)) + geom_point(alpha=.2, size=6),
       width=8, height=6)

ggsave(here("type_specific_modeling/variable_selection/charts/selected_dotplot.png"),
       ggplot(tculls[tculls$rank>13,], aes(variable, type, color=grepl("temp", variable))) + 
             geom_point(size=6) +
             theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)),
       width=12, height=9)



## HEATMAPS OF VARIABLE IMPORTANCE ################################

ggsave(here("type_specific_modeling/variable_selection/charts/importance_heatmap.png"),
       ggplot(tculls, aes(variable, type, fill=rank)) + 
             geom_raster() +
             theme(axis.text.x=element_text(angle=45, hjust=1)) +
             scale_fill_gradient(high="darkgreen", low="white"),
       width=12, height=9)


hmd <- culls
rownames(hmd) <- hmd$veg
hmd <- hmd[,names(hmd)!="veg"]
hmd <- as.matrix(hmd)
colnames(hmd) <- ecoclim::translate(as.character(colnames(hmd)), "words")
hmd <- hmd[,order(apply(hmd, 2, mean))]
hmd <- hmd[order(apply(hmd[,14:19], 1, mean)),]
#hmd[hmd<10] <- NA
png(here("type_specific_modeling/variable_selection/charts/heatmap_clust.png"),
    width=1500, height=1000)
heatmap(hmd, 
        Colv=NA, Rowv=NA, margins=c(50,50),
        col=colorRampPalette(c("white", "darkgreen"))(100))
dev.off()

hmd[hmd<14] <- 0
hmd[hmd>=14] <- 1
png(here("type_specific_modeling/variable_selection/charts/heatmap_clust_binary.png"),
    width=1000, height=700)
heatmap(hmd, margins=c(20,20),
        col=colorRampPalette(c("white", "darkgreen"))(100))
dev.off()



## AUC CURVES ####################

s <- lapply(d, function(x) readRDS(x)[[2]])
s <- do.call("rbind", s)
rownames(s) <- 1:nrow(s)

r <- s %>%
      group_by(veg, cull) %>%
      summarize(auc = mean(auc),
                var_culled = variable[decrease == min(decrease)][1]) %>%
      mutate(nvars = 20 - cull)
rm <- r %>%
      group_by(cull) %>%
      summarize(median_auc = median(auc),
                auc = mean(auc))
ggsave(here("type_specific_modeling/variable_selection/charts/auc_curves.png"),
       ggplot(r, aes(20-cull, auc, color=veg)) + 
             geom_point() + 
             geom_line() +
             geom_line(data=rm, aes(20-cull, auc), color="black", size=1) +
             geom_line(data=rm, aes(20-cull, median_auc), color="black", size=1, linetype=2) +
             scale_x_continuous(breaks=0:20) +
             labs(title = "RFE AUC for CONUS types") +
             theme(legend.position="none"),
       width=8, height=6)
# these curves suggest 6 as a parsimonious number of variables


############## FINAL AUC VALUES #################################

f <- s[s$cull == (20 - 6),] # cull 14 is when there were 6 vars left in the RFE
f <- f %>%
      group_by(veg) %>%
      summarize(auc = mean(auc),
                nvars = length(unique(variable)))

# save final auc values
write.csv(f, 
          here("type_specific_modeling/variable_selection/auc_6_vars.csv"),
          row.names=F)

ggsave(here("type_specific_modeling/variable_selection/charts/final_auc_by_type.png"),
       ggplot(f, aes(auc, veg)) +
             geom_point() +
             xlim(0.5, 1) +
             labs(title=paste0("Model performance by veg type\n(calculated on class-balanced presence-absence\ndata using spatial block cross-validation and\n",
                               "equal-distance subsampling of evaluation points)"),
                  x=paste0("Mean AUC across ", max(s$rep), " modeling runs"),
                  y=NULL) +
             theme(plot.title=element_text(hjust=1)),
       width=8, height=6)
