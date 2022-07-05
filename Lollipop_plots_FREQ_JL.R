library(ggplot2)
library(dplyr)
library(forcats)
library(cowplot)

add_counts <- function(df) {
  adult_count = round(df$adult_perc * df$total)
  child_count = round(df$child_perc * df$total)
  adult_freq = adult_count / sum(adult_count)
  child_freq = child_count / sum(child_count)
  af = df$total / sum(df$total)
  data.frame(df, af = af, adult_count = adult_count, child_count = child_count, adult_freq = adult_freq, child_freq = child_freq)
}

setwd("~/Documents/Imperial/Papers/carriage_age/analysis")

####lollipop plots
mydata1=read.table("lollypop_plots_INPUT_FREQ_NL_serotype.txt", header = TRUE, sep = "\t", na.strings = "NA")
mydata2=read.table("lollypop_plots_INPUT_FREQ_NL_GPSC.txt", header = TRUE, sep = "\t", na.strings = "NA")
mydata3=read.table("final_lollypop_plots_INPUT_FREQ_OR_Maela_serotype.txt", header = TRUE, sep = "\t", na.strings = "NA")
mydata4=read.table("final_lollypop_plots_INPUT_FREQ_OR_Maela_GPSC.txt", header = TRUE, sep = "\t", na.strings = "NA")

mydata1 = add_counts(mydata1)
mydata2 = add_counts(mydata2)
mydata3 = add_counts(mydata3)
mydata4 = add_counts(mydata4)

# geometric mean: not used
gm_mean = function(x){
  x[is.na(x)] <- 0.001
  exp(sum(log(x[x > 0])) / length(x))
}

# Serotype data
serotype_data <- merge(mydata1, mydata3, by.x = "serotype", by.y = "serotype", all = TRUE)

serotype_freqs <- serotype_data[,c("af.x", "af.y")]
serotype_freqs[is.na(serotype_freqs)] <- 0.00001
serotype_mean_af <- apply(serotype_freqs, 1, mean)
# Hack to get sort shared > NL > Maela
serotype_freqs <- serotype_data[,c("af.x", "af.y")]
serotype_mean_af[is.na(serotype_freqs)[,1]] <- serotype_mean_af[is.na(serotype_freqs)[,1]] - 100
serotype_mean_af[is.na(serotype_freqs)[,2]] <- serotype_mean_af[is.na(serotype_freqs)[,2]] - 10
#serotype_total <- rowSums(serotype_data[,c("total.x", "total.y")], na.rm=TRUE)
serotype_data <- data.frame(serotype_data, total = serotype_mean_af)

# GPSC data
mydata4$GPSC <-  as.factor(as.character(mydata4$GPSC))
gpsc_data <- merge(mydata2, mydata4, by.x = "GPSC", by.y = "GPSC", all = TRUE)

gpsc_freqs <- gpsc_data[,c("af.x", "af.y")]
gpsc_freqs[is.na(gpsc_freqs)] <- 0.00001
gpsc_mean_af <- apply(gpsc_freqs, 1, mean)
# Hack to get sort shared > NL > Maela
gpsc_freqs <- gpsc_data[,c("af.x", "af.y")]
gpsc_mean_af[is.na(gpsc_freqs)[,1]] <-  gpsc_mean_af[is.na(gpsc_freqs)[,1]] - 100
gpsc_mean_af[is.na(gpsc_freqs)[,2]] <-  gpsc_mean_af[is.na(gpsc_freqs)[,2]] - 10
gpsc_data <- data.frame(gpsc_data, total = gpsc_mean_af)

plot1=ggplot(serotype_data, aes(x=reorder(serotype, total))) +
  geom_segment( aes(xend=serotype, y=adult_freq.x, yend=child_freq.x), color="grey") +
  geom_point( aes(y=adult_freq.x), color=rgb(230/255,159/255,0,0.8), size=3 ) +
  geom_point( aes(y=child_freq.x), color=rgb(86/255,180/255,233/255,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    panel.border = element_blank()
  ) +
  ylim(0, 0.25) +
  xlab("Serotype") +
  ylab("Frequency in subgroup") +
  ggtitle("Serotypes in Dutch cohort")

plot2=ggplot(serotype_data, aes(x=reorder(serotype, total))) +
  geom_segment( aes(xend=serotype, y=adult_freq.y, yend=child_freq.y), color="grey") +
  geom_point( aes(y=adult_freq.y), color=rgb(230/255,159/255,0,0.8), size=3 ) +
  geom_point( aes(y=child_freq.y), color=rgb(86/255,180/255,233/255,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    panel.border = element_blank()
  ) +
  ylim(0, 0.25) +
  xlab("") +
  ylab("Frequency in subgroup") +
  ggtitle("Serotypes in Maela cohort")

plot3=ggplot(serotype_data, aes(x=reorder(serotype, total))) +
  geom_segment( aes(xend=serotype, y=child_freq.x/adult_freq.x, yend=child_freq.y/adult_freq.y), color="grey") +
  geom_point( aes(y=child_freq.x/adult_freq.x), color=rgb(0,158/255,115/255,0.8), size=3 ) +
  geom_point( aes(y=child_freq.y/adult_freq.y), color=rgb(213/255,94/255,0/255,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    panel.border = element_blank()
  ) +
  xlab("") +
  ylab("Odds-ratio of prevalence in children") +
  scale_y_continuous(breaks = c(0.1, 1, 10), limits = c(0.1, 10), labels = c(0.1, 1, 10), trans='log') +
  ggtitle("Serotype adult/child frequency (both cohorts)")

plot4=ggplot(gpsc_data, aes(x=reorder(GPSC, total))) +
  geom_segment( aes(xend=GPSC, y=adult_freq.x, yend=child_freq.x), color="grey") +
  geom_point( aes(y=adult_freq.x), color=rgb(230/255,159/255,0,0.8), size=3 ) +
  geom_point( aes(y=child_freq.x), color=rgb(86/255,180/255,233/255,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    panel.border = element_blank()
  ) +
  ylim(0, 0.25) +
  xlab("GPSC") +
  ylab("Frequency in subgroup") +
  ggtitle("GPSCs in Dutch cohort")

plot5=ggplot(gpsc_data, aes(x=reorder(GPSC, total))) +
  geom_segment( aes(xend=GPSC, y=adult_freq.y, yend=child_freq.y), color="grey") +
  geom_point( aes(y=adult_freq.y), color=rgb(230/255,159/255,0,0.8), size=3 ) +
  geom_point( aes(y=child_freq.y), color=rgb(86/255,180/255,233/255,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    panel.border = element_blank()
  ) +
  ylim(0, 0.25) +
  xlab("") +
  ylab("Frequency in subgroup") +
  ggtitle("GPSCs in Maela cohort")

plot6=ggplot(gpsc_data, aes(x=reorder(GPSC, total))) +
  geom_segment( aes(xend=GPSC, y=child_freq.x/adult_freq.x, yend=child_freq.y/adult_freq.y), color="grey") +
  geom_point( aes(y=child_freq.x/adult_freq.x), color=rgb(0,158/255,115/255,0.8), size=3 ) +
  geom_point( aes(y=child_freq.y/adult_freq.y), color=rgb(213/255,94/255,0/255,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    panel.border = element_blank()
  ) +
  xlab("") +
  ylab("Odds-ratio of prevalence in children") +
  scale_y_continuous(breaks = c(0.1, 1, 10), limits = c(0.1, 10), labels = c(0.1, 1, 10), trans='log') +
  ggtitle("GPSC adult/child frequency (both cohorts)")

pdf(file = "lollypop_plot_xaxis.pdf", width = 16, height = 10)
plot_grid(plot1, plot2, plot3, plot4, plot5, plot6,
          labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 12)
dev.off()

## old plots

mydata1$serotype=factor(mydata1$serotype, levels = mydata1$serotype)
plot1=ggplot(mydata1, aes(x=reorder(serotype, total))) +
  geom_segment( aes(xend=serotype, y=adult_count, yend=child_count), color="grey") +
  geom_point( aes(y=adult_count), color=rgb(0.2,0.7,0.1,0.8), size=3 ) +
  geom_point( aes(y=child_count), color=rgb(0.7,0.2,0.1,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    panel.border = element_blank()
  ) +
  xlab("Serotype") +
  ylab("Count") +
  ggtitle("Serotypes in Dutch cohort")
plot(plot1)

plot2=ggplot(mydata3, aes(x=reorder(serotype, total))) +
  geom_segment( aes(xend=serotype, y=adult_count, yend=child_count), color="grey") +
  geom_point( aes(y=adult_count), color=rgb(0.2,0.7,0.1,0.8), size=3 ) +
  geom_point( aes(y=child_count), color=rgb(0.7,0.2,0.1,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    panel.border = element_blank()
  ) +
  xlab("") +
  ylab("Count") +
  ggtitle("Serotypes in Maela cohort")
plot(plot2)



mydata2$GPSC=as.factor(mydata2$GPSC)
plot2=ggplot(mydata2, aes(x=GPSC, y=adult_perc)) +
  geom_segment( aes(x=GPSC, xend=GPSC, y=adult_perc, yend=child_perc), color="grey") +
  geom_point( aes(x=GPSC, y=adult_perc), color=rgb(0.2,0.7,0.1,0.8), size=3 ) +
  geom_point( aes(x=GPSC, y=child_perc), color=rgb(0.7,0.2,0.1,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  ylab("Frequency of occurence in adult and child") +
  ggtitle("Sequence clusters in Dutch cohort")
plot(plot2)

mydata3$serotype=as.factor(mydata3$serotype)
plot3=ggplot(mydata3, aes(x=serotype, y=adult_perc)) +
  geom_segment( aes(x=serotype, xend=serotype, y=adult_perc, yend=child_perc), color="grey") +
  geom_point( aes(x=serotype, y=adult_perc), color=rgb(0.2,0.7,0.1,0.8), size=3 ) +
  geom_point( aes(x=serotype, y=child_perc), color=rgb(0.7,0.2,0.1,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  ylab("Frequency of occurence in adult and child") +
  ggtitle("Serotypes in Maela cohort")
plot(plot3)



mydata4$GPSC=as.factor(mydata4$GPSC)
plot4=ggplot(mydata4, aes(x=GPSC, y=adult_perc)) +
  geom_segment( aes(x=GPSC, xend=GPSC, y=adult_perc, yend=child_perc), color="grey") +
  geom_point( aes(x=GPSC, y=adult_perc), color=rgb(0.2,0.7,0.1,0.8), size=3 ) +
  geom_point( aes(x=GPSC, y=child_perc), color=rgb(0.7,0.2,0.1,0.8), size=3 ) +
  coord_flip() +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  ylab("Frequency of occurence in adult and child") +
  ggtitle("Sequence clusters in Maela cohort")
plot(plot4)

##cowplot - allows labelling
plot_grid(plot1, plot2, plot3, plot4, labels="AUTO")
