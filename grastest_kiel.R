if (FALSE) {
remotes::install_gitlab(repo = "AEF/GFO/GrasProg/grasprog", 
                        host = "cau-git.rz.uni-kiel.de", 
                        dependencies = TRUE, force = TRUE)
}

library(grasprog)
library(tidyverse)
library(vroom)
library(patchwork)
library(vegperiod)

vegperiod::read.DWDstations()

vegperiod::read.DWDstations() |> filter(str_detect(name, "Kiel"))
#     id       from         to elev     lat    long            name              state
# 1 2564 1974-01-01 2026-04-20   28 54.3776 10.1424   Kiel-Holtenau Schleswig-Holstein
# 2 2565 1940-01-01 1988-10-15   17 54.3378 10.0929 Kiel-Kronshagen Schleswig-Holstein
# 3 2961 1998-01-01 2026-04-20    0 54.4996 10.2737 Leuchtturm Kiel Schleswig-Holstein


weather <- dwd_weather(c(2564))  
  
weatherfile <- system.file("extdata", "dwd_wetter_kiel.csv", package = "grasprog")
weather <- readr::read_csv(weatherfile)

cut_dates <- cut_dates_standard(2020)
grasprog(weather = weather, cut_dates, nfk = 80)

out <- grasprog(weather = weather, cut_dates = cut_dates, nfk = 80)
head(out)



pdf("grasprog_weather.pdf", paper = "a4")
weather |> ggplot(aes(Date, RG)) + geom_point() +
weather |> ggplot(aes(Date, RG)) + geom_point() +
weather |> ggplot(aes(Date, TM)) + geom_point() +
weather |> ggplot(aes(Date, RR)) + geom_point() +
weather |> ggplot(aes(Date, QH)) + geom_point() +
  plot_layout(ncol = 2)
dev.off()



inifile <- system.file("extdata", "foproq_gras_original.ini",
                       package = "grasprog")
paramsini <- parseini(inifile)
 
nfk <- 120 # mm

all <- matrix()
years <- cut_dates |> year() |> unique() |> as.numeric()

for (y in years) {
  print(y)
   cuts_y <- cut_dates[year(cut_dates) == y]
   conds <- list()
   for (i in 1:length(cuts_y)) 
     conds[[i]]   <- list(paste0("JT >= ", yday(cuts_y[i])))
     names(conds) <- paste0("cut", 1:length(cuts_y))
     # simulate each year with rs and w0 paramter for each cutting day
     newparams    <- c(rs_dm(cuts_y), w0_dm(cuts_y))
     # print(newparams |> as_tibble())
     out <- foproq(paramsini, wetter = weather,
                   y, nfk, conds, params = newparams)
   all <- plyr::rbind.fill.matrix(all, out)
}

test_sim <- as_tibble(all)
colnames(test_sim) <- colnames(all)
test_sim <- test_sim |>
  mutate(Date = as.Date(Date, origin = "1970-1-1")) |>
  filter(!is.na(Date))

test_sim |> summarise(min(Date,na.rm=TRUE), max(Date,na.rm=TRUE))

openxlsx::write.xlsx(test_sim, "grasprog_output.xlsx",
                     firstRow = TRUE, overwrite = TRUE)

test_plot <- test_sim |> 
  mutate(Year = year(Date), 
         growths = if_else(cut == 0, NA, cut)) |>
  fill(growths, .direction = "updown") |>
  ggplot(aes(Date, W, group = growths)) +
  geom_line() +
  facet_wrap(~Year, scale = "free_x")

test_plot

pdf("grasprog.pdf", paper = "a4")
test_plot
dev.off()
