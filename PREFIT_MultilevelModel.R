# Multilevel ANCOVA

library(lmerTest)
library(sjPlot)
library(emmeans)

db <- read.csv2("full_db.csv")

# Select variables
db <- db %>%
  select(ID_2, Sex_cat, Age, Wgt1, Wgt2, Hgt1, Hgt2, City_cat, SocioEconIndex_cat, HG, SLJ, P4X10M, Sol) %>%
  mutate(Weight = (Wgt1 + Wgt2)/2,
         Height = (Hgt1 + Hgt2)/2,
         SocioEconIndex_cat = factor(SocioEconIndex_cat, ordered = T, levels = c("1","2","3","4","5","6","7","8","9","10")))

str(db)

# Boxplots

png("boxplots.png", width = 13, height = 8, units = 'in', res = 600)
par(mfrow=c(2,2))
# HG
boxplot(HG ~ City_cat,
        data=db, 
        main="Handgrip test", 
        xlab="Cities", 
        ylab="Handgrip (kg)",
        cex.axis = 0.6)

# SLJ
boxplot(SLJ ~ City_cat,
        data=db, 
        main="Standing long jump test", 
        xlab="Cities", 
        ylab="Standing long jump (cm)",
        cex.axis = 0.6)

# P4X10M
boxplot(P4X10M ~ City_cat,
        data=db, 
        main="4x10 shuttle run test", 
        xlab="Cities", 
        ylab="4x10 shuttle run test (s)",
        cex.axis = 0.6)

# Sol
boxplot(P4X10M ~ City_cat,
        data=db, 
        main="One-leg stance test", 
        xlab="Cities", 
        ylab="One-leg stance test (s)",
        cex.axis = 0.6)
dev.off()



# HG
mod_HG_noran <- lm(HG ~ Age + Sex_cat, data = db)
mod_HG_ran <- lmer(HG ~ Age + Sex_cat + SocioEconIndex_cat + (1 | City_cat), data = db, REML=FALSE)
anova(mod_HG_ran, mod_HG_noran)
#step_res <- step(mod_HG)
#final <- get_model(step_res)
#anova(final)

# SLJ
mod_SLJ_noran <- lm(SLJ ~ Age + Sex_cat, data = db)
mod_SLJ_ran <- lmer(SLJ ~ Age + Sex_cat + (1 | City_cat), data = db, REML = F)
anova(mod_SLJ_ran, mod_SLJ_noran)

# P4X10M
mod_P4X10M_noran <- lm(P4X10M ~ Age + Sex_cat, data = db)
mod_P4X10M_ran <- lmer(P4X10M ~ Age + Sex_cat + (1 | City_cat), data = db, REML = F)
anova(mod_P4X10M_ran, mod_P4X10M_noran)


# Sol
mod_Sol_noran <- lm(Sol ~ Age + Sex_cat, data = db)
mod_Sol_ran <- lmer(Sol ~ Age + Sex_cat + (1 | City_cat), data = db, REML = F)
anova(mod_Sol_ran, mod_Sol_noran)


# Random Effects
city_effect_HG <- ranef(mod_HG)$City_cat
city_effect_HG <- ranef(mod_SLJ)$City_cat
city_effect_P4X10M <- ranef(mod_P4X10M)$City_cat
city_effect_Sol <- ranef(mod_Sol)$City_cat


# Plt Random Effects
HG_ran <- ggCaterpillar(ranef(mod_HG_ran, condVar=TRUE), QQ=FALSE)
#ggCaterpillar(ranef(mod_HG, condVar=TRUE), QQ=FALSE)

SLJ_ran <- ggCaterpillar(ranef(mod_SLJ_ran, condVar=TRUE), QQ=FALSE)
P4X10M_ran <- ggCaterpillar(ranef(mod_P4X10M_ran, condVar=TRUE), QQ=FALSE)
Sol_ran <- ggCaterpillar(ranef(mod_Sol_ran, condVar=TRUE), QQ=FALSE)

ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=1.2), colour="blue") 
    return(p)
  }
  
  lapply(re, f)
}

png("Random_effects.png", width = 13, height = 8, units = 'in', res = 600)
par(mfrow=c(2,2))
# Plt Random Effects
ggCaterpillar(ranef(mod_HG_ran, condVar=TRUE), QQ=FALSE)
#ggCaterpillar(ranef(mod_HG, condVar=TRUE), QQ=FALSE)

ggCaterpillar(ranef(mod_SLJ_ran, condVar=TRUE), QQ=FALSE)
ggCaterpillar(ranef(mod_P4X10M_ran, condVar=TRUE), QQ=FALSE)
ggCaterpillar(ranef(mod_Sol_ran, condVar=TRUE), QQ=FALSE)

dev.off()


HG_ran <- plot_model(mod_HG_ran, type = "re", sort.est = "sort.all", grid = F ) + ggtitle("Handgrip") + 
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
SLJ_ran <- plot_model(mod_SLJ_ran, type = "re", sort.est = "sort.all", grid = F ) + ggtitle("Standing long jump") + 
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
P4X10M_ran <- plot_model(mod_P4X10M_ran, type = "re", sort.est = "sort.all", grid = F ) + ggtitle("4x10m shuttle run test") + 
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
Sol_ran <- plot_model(mod_Sol_ran, type = "re", sort.est = "sort.all", grid = F ) + ggtitle("One-leg stance test") + 
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

ggarrange(HG_ran, 
          SLJ_ran,
          P4X10M_ran,
          Sol_ran,
          ncol = 2, nrow = 2)
ggsave("Random_effects.png", height=5, width=10, units='in', dpi=600)


