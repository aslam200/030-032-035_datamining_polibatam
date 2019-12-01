library(dplyr)
library(tidyverse)
library(rayshader)
library(ggbeeswarm)
library(forcats)
library(jpeg)
library(grid)

ramen_ratings = readr::read_csv("https://github.com/aslam200/030-032-035_datamining_polibatam/ramen_ratings.csv")

ramen_ratings %>%
  arrange(style,desc(stars)) %>%
  mutate(stylenum = 1) %>%
  filter(!is.na(style)) -> ramen_arranged

for(i in 1:(nrow(ramen_arranged)-1)) {
  if(ramen_arranged[i,"style"] == ramen_arranged[i+1,"style"]) {
    ramen_arranged[i+1,"stylenum"] = ramen_arranged[i,"stylenum"] + 1
  }
}

img = readJPEG("ramen.jpg")
g = rasterGrob(img, interpolate=TRUE)

ramen = ramen_arranged %>%
  filter(style %in% c("Bowl","Cup","Pack","Tray")) %>%
  arrange(stars) %>%
  mutate(style = forcats::fct_infreq(style)) %>%
  ggplot() +
  geom_beeswarm(aes(x=style,y=stylenum,color=stars),size=0.7) +
  scale_color_viridis_c("Stars") +
  scale_y_continuous("",breaks = c()) +
  scale_x_discrete("Ramen Style") +
  ggtitle("Ramen ratings (select styles)") +
  theme(axis.text.x = element_text(size=18),
        axis.title.x = element_text(size=18),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        panel.background = element_blank(),
        title = element_text(size=18)) +
  guides(color = guide_colorbar(nbin=1000, frame.colour=c("black")))

ramenimage = ramen_arranged %>%
  filter(style %in% c("Bowl","Cup","Pack","Tray")) %>%
  arrange(stars) %>%
  mutate(style = forcats::fct_infreq(style)) %>%
  ggplot() +
  geom_beeswarm(aes(x=style,y=stylenum,color=stars),size=0.7) +
  scale_color_viridis_c("Stars") +
  scale_y_continuous("",breaks = c()) +
  scale_x_discrete("Ramen Style") +
  annotation_custom(g, xmin=2, xmax=4, ymin=800, ymax=1800) +
  ggtitle("Ramen ratings (select styles)") +
  theme(axis.text.x = element_text(size=18),
        axis.title.x = element_text(size=18),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        panel.background = element_blank(),
        title = element_text(size=18)) +
  guides(color = guide_colorbar(nbin = 1000, frame.colour = c("black"))) 

plot_gg(list(ramenimage,ramen), pointcontract = 0.6, width = 8, height = 6, multicore = FALSE,
        scale = 300, sunangle = 315-90, windowsize = c(800,800))


phivec = 20 + 70 * 1/(1 + exp(seq(-5, 12, length.out = 1080/2)))
phivecfull = c(phivec, rev(phivec))
thetavec = seq(0,360,length.out = 1081)[-1081]
zoomvec = 0.5 + 0.3 * 1/(1 + exp(seq(-4, 11, length.out = 1080/2)))
zoomvecfull = c(zoomvec, rev(zoomvec))
render_camera(zoom=0.5,phi=30,theta=140)

for(i in seq(1,1080,1)) {
  render_camera(zoom = zoomvecfull[i], phi = phivecfull[i],theta = thetavec[i])
  render_snapshot(glue::glue("ramen{i}"))
}
