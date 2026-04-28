if (FALSE) {
remotes::install_gitlab(repo = "AEF/GFO/GrasProg/grasprog", 
                        host = "cau-git.rz.uni-kiel.de", 
                        dependencies = TRUE)
}

library(grasprog)
library(tidyverse)
library(vroom)
library(patchwork)

library(vegperiod)

vegperiod::read.DWDstations()

help(grasprog)

inrea_weather <- read_csv2("inrea/meteo.csv") |>  
  mutate(Date = dmy(date),
         Year = year(Date),
         JT = yday(Date)) |>
  select(Date, Year, JT, TM=TMC, RR, RG, QH=ETPP) |>
  mutate(across(TM:QH, as.numeric))

skimr::skim(inrea_weather)

pdf("inrea_grasprog_weather.pdf", paper = "a4")
inrea_weather |> ggplot(aes(Date, RG)) + geom_point() +
inrea_weather |> filter(RG < 10000) |> ggplot(aes(Date, RG)) + geom_point() +
inrea_weather |> ggplot(aes(Date, TM)) + geom_point() +
inrea_weather |> ggplot(aes(Date, RR)) + geom_point() +
inrea_weather |> ggplot(aes(Date, QH)) + geom_point() +
  plot_layout(ncol = 2)
dev.off()

inrea_weather |> filter(RG > 10000) |> as.data.frame()

inrea_weather <- inrea_weather |>
  mutate(RG_org = RG,
         RG = if_else(RG > 10000, NA_real_, RG)) |> 
  fill(RG, .direction = "updown")

inrea_weather |> filter(RG_org > 10000)

inrea_weather |> group_by() |> summarise(min(Date), max(Date))
inrea_weather |> count(year(Date))

inrea_output  <- read_csv2("inrea/grasprog_output.csv") |> 
  mutate(doy = row_number()) 

inrea_param   <- read_csv2("inrea/parameters_values.csv")

inrea_output |> filter(yield != 0)
# rowname   AGB yield   LAI
# 1 181      100  2877.  4.36
# 2 237      100   883.  2.61
# 3 303      166.  795.  2.58

# Inrea cutting days
inrea_cuts <- as.Date(paste(rep(1993:2000, each=3), 
                            c(181, 237, 303)), format="%Y %j")

inrea_cuts

inifile <- system.file("extdata", "foproq_gras_original.ini",
                       package = "grasprog")
paramsini <- parseini(inifile)
 
nfk <- 120 # mm

all <- matrix()
years <- inrea_cuts |> year() |> unique() |> as.numeric()

for (y in years) {
  print(y)
   inrea_cuts_y <- inrea_cuts[year(inrea_cuts) == y]
   conds <- list()
   for (i in 1:length(inrea_cuts_y)) 
     conds[[i]]   <- list(paste0("JT >= ", yday(inrea_cuts_y[i])))
     names(conds) <- paste0("cut", 1:length(inrea_cuts_y))
     # simulate each year with rs and w0 paramter for each cutting day
     newparams    <- c(rs_dm(inrea_cuts_y), w0_dm(inrea_cuts_y))
     # print(newparams |> as_tibble())
     out <- foproq(paramsini, wetter = inrea_weather,
                   y, nfk, conds, params = newparams)
   all <- plyr::rbind.fill.matrix(all, out)
}

inrea_sim <- as_tibble(all)
colnames(inrea_sim) <- colnames(all)
inrea_sim <- inrea_sim |>
  mutate(Date = as.Date(Date, origin = "1970-1-1")) |>
  filter(!is.na(Date))

inrea_sim |> summarise(min(Date,na.rm=TRUE), max(Date,na.rm=TRUE))

openxlsx::write.xlsx(inrea_sim, "inrea_grasprog_output.xlsx",
                     firstRow = TRUE, overwrite = TRUE)

inrea_plot <- inrea_sim |> 
  mutate(Year = year(Date), 
         growths = if_else(cut == 0, NA, cut)) |>
  fill(growths, .direction = "updown") |>
  ggplot(aes(Date, W, group = growths)) +
  geom_line() +
  facet_wrap(~Year, scale = "free_x")

inrea_plot

pdf("inrea_grasprog.pdf", paper = "a4")
inrea_plot
dev.off()
