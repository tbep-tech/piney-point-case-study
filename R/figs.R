# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(ggrepel)
library(sf)
library(here)
library(patchwork)
library(NADA)
library(scales)
library(grid)
library(gridExtra)

source(here('R/funcs.R'))

# water quality change ------------------------------------------------------------------------

load(url('https://github.com/tbep-tech/piney-point-manu/raw/refs/heads/main/data/rswqdat.RData'))
load(url('https://github.com/tbep-tech/piney-point-manu/raw/refs/heads/main/data/bswqdat.RData'))
load(url('https://github.com/tbep-tech/piney-point-manu/raw/refs/heads/main/data/ppseg.RData'))
load(url('https://github.com/tbep-tech/piney-point-manu/raw/refs/heads/main/data/rsstatloc.RData'))
load(url('https://github.com/tbep-tech/piney-point-manu/raw/refs/heads/main/data/bsstatloc.RData'))
load(url('https://github.com/tbep-tech/piney-point-manu/raw/refs/heads/main/data/parms.RData'))

p1 <- wqplo_fun(rswqdat, bswqdat, ppseg, vr = 'tn', logtr = TRUE, ttl = '(a) Total Nitrogen', ylb = 'mg/L (log-scale)')
p2 <- wqplo_fun(rswqdat, bswqdat, ppseg, vr = 'chla', logtr = TRUE, ttl = '(b) Chlorophyll-a', 
                ylb = expression(paste(mu, 'g/L (log-scale)')), addrect = T)
p3 <- wqplo_fun(rswqdat, bswqdat, ppseg, vr = 'secchi', logtr = FALSE, ttl = '(c) Secchi', ylb = 'meters')

p <- (p1 + p2 + p3 + plot_layout(ncol = 3)) / wrap_elements(grid::textGrob('Week of', gp = grid::gpar(fontsize=12))) + 
  plot_layout(ncol = 1, guides = 'collect', height = c(1, 0.07)) & 
  theme(legend.position = 'top')

png(here("figs", "wqchange.png"), width = 8.5, height = 3.5, units = "in", res = 400)
print(p)
dev.off()

# reported fish kills -------------------------------------------------------------------------

load(file = here('data/fishdat.RData'))

weeklv <- seq.Date(from = as.Date('2021-01-03'), to = as.Date('2021-12-31'), by = 'days') %>% 
  lubridate::floor_date(unit = 'week') %>% 
  unique

toplo1 <- fishdat |> 
  mutate(
    yr = year(week)
  ) |> 
  summarise(
    cnt = sum(cnt), 
    .by = c(yr, County)
  ) |> 
  complete(
    yr = seq(min(yr), max(yr), 1), 
    County, 
    fill = list(cnt = 0)
  )

toplo2 <- fishdat |> 
  filter(year(week) == 2021) |> 
  complete(
    week = weeklv,
    County, 
    fill = list(cnt = 0)
  )

p1 <- ggplot(toplo1, aes(x = yr, y = cnt, fill = County)) + 
  geom_bar(stat = 'identity', colour = 'darkgrey') + 
  labs(
    x = NULL,
    y = 'No. of fish kill reports', 
    subtitle = '(a) By year'
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(breaks = seq(min(toplo1$yr), max(toplo1$yr), 1), expand = c(0.01, 0.01)) +
  scale_fill_brewer(palette = 'Pastel1', drop = T) + 
  theme_minimal() + 
  theme(
    axis.ticks.x = element_line(), 
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.caption = element_text(size = 10)
  )

p2 <- ggplot(toplo2, aes(x = week, y = cnt, fill = County)) + 
  geom_bar(stat = 'identity', colour = 'darkgrey') + 
  labs(
    x = NULL, 
    y = 'No. of fish kill reports', 
    subtitle = '(b) 2021 week of'
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_date(breaks = weeklv, date_labels = '%b %d', limits = range(weeklv), expand = c(0.01, 0.01)) +
  scale_fill_brewer(palette = 'Pastel1', drop = T) + 
  theme_minimal() + 
  theme(
    axis.ticks.x = element_line(), 
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.caption = element_text(size = 10)
  )

p <- p1 + p2 + guide_area() +  
  plot_layout(ncol = 1, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 0.1))

png(here('figs/fishkill.png'), height = 4.5, width = 8, units = 'in', res = 300)
print(p)
dev.off()

# seagrass change -----------------------------------------------------------------------------

allmngests <- rdataload('https://github.com/tbep-tech/seagrass-analysis/raw/refs/heads/main/data/allmngests.RData')
chgdat20202022 <- rdataload('https://github.com/tbep-tech/seagrass-analysis/raw/refs/heads/main/data/chgdat20202022.RData')
data(file = 'sgmanagement', package = 'tbeptools')

ars <- list(
  west = c(12, 16:18, 30), 
  east = c(6:10)
) |> 
  enframe('side', 'number') |> 
  unnest(number)

toplo <- allmngests |> 
  filter(
    Areas %in% ars$number
  ) |> 
  filter(
    yr %in% c(2020, 2022)
  ) |> 
  ungroup() |> 
  summarise(
    Acres = sum(Acres), 
    .by = c(yr, Areas)
  ) |> 
  pivot_wider(values_from = Acres, names_from = yr) |> 
  mutate(
    diffacres = `2022` - `2020`
  )
tomap1 <- inner_join(sgmanagement, toplo, by = c('areas' = 'Areas'))

maxv <- max(abs(tomap1$diffacres))

ppt <- tibble(lat = 27.6293999448385, lon = -82.52936862182267) |> 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# colors
colgrn <- c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", 
            "#238B45", "#006D2C", "#00441B")
colred <- c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", 
            "#CB181D", "#A50F15", "#67000D")               
colfun <- leaflet::colorNumeric(
  palette = c(rev(colred), colgrn),
  domain = c(-1 * maxv, maxv)
)

# text labels
totxt <- st_centroid(tomap1) |> 
  mutate(
    lab = round(diffacres, 0)
  )

# bbox
dat_ext <- tomap1 %>% 
  sf::st_as_sfc() %>% 
  sf::st_buffer(dist = units::set_units(2, kilometer)) %>%
  sf::st_transform(crs = 4326) %>% 
  sf::st_bbox()

tls <- maptiles::get_tiles(dat_ext, provider = 'CartoDB.PositronNoLabels', zoom = 10)

thm <- ggplot2::theme(
  panel.grid = ggplot2::element_blank(), 
  axis.title = ggplot2::element_blank(), 
  axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.7)), 
  axis.text.x = ggplot2::element_text(size = ggplot2::rel(0.7), angle = 30, hjust = 1),
  axis.ticks = ggplot2::element_line(colour = 'grey'),
  panel.background = ggplot2::element_rect(fill = NA, color = 'black'), 
  legend.position = 'top', 
  legend.title.position = 'top',
  legend.key.width = unit(1, "cm"),
  legend.key.height = unit(0.25, "cm"),
  legend.title = element_text(hjust = 0.5)
) 

m1 <- ggplot2::ggplot() + 
  tidyterra::geom_spatraster_rgb(data = tls, maxcell = 1e8) +
  ggplot2::geom_sf(data = tomap1, aes(fill = tomap1$diffacres), color = 'black', inherit.aes = F, alpha = 0.7) +
  ggplot2::geom_sf_label(data = totxt, ggplot2::aes(label = lab), size = 3, alpha = 0.8, inherit.aes = F) +
  ggplot2::geom_sf(data = ppt, color = 'black', size = 3, shape = 17, inherit.aes = F) +
  ggplot2::geom_sf_text(data = ppt, aes(label = 'Piney Point'), size = 3, hjust = 0, vjust = 0.2, nudge_x = 0.015, inherit.aes = F, ) +
  scale_fill_gradientn(
    colors = c(rev(colred), colgrn),
    values = scales::rescale(c(seq(-maxv, 0, length.out = length(colred)),
                               seq(0, maxv, length.out = length(colgrn)))),
    limits = c(-maxv, maxv),
    na.value = "grey50"
  ) +
  thm + 
  ggspatial::annotation_scale(location = 'bl', unit_category = 'metric', height = unit(0.2, "cm"), text_cex = 0.5) +
  ggspatial::annotation_north_arrow(location = 'tl', height = unit(1, 'cm'), width = unit(1, 'cm')) +
  labs(
    fill = '(a) Change in acres'
  )

dat_ext <- dat_ext %>% 
  sf::st_as_sfc(dat_ext) %>% 
  sf::st_transform(crs = 4326) %>% 
  sf::st_bbox()

# set coordinates because vector not clipped
m1 <- m1 +
  ggplot2::coord_sf(xlim = dat_ext[c(1, 3)], ylim = dat_ext[c(2, 4)], expand = FALSE, crs = 4326)

tomap2 <- chgdat20202022[sgmanagement[sgmanagement$areas %in% ars$number, ], ] |> 
  mutate(
    var = factor(var, levels = c("lost", "gained"))
  )

m2 <- ggplot2::ggplot() + 
  tidyterra::geom_spatraster_rgb(data = tls, maxcell = 1e8) +
  ggplot2::geom_sf(data = tomap2, aes(fill = var), color = NA, inherit.aes = F, alpha = 0.7) +
  ggplot2::geom_sf(data = tomap1, fill = NA, color = 'black', inherit.aes = F, alpha = 0.7) +
  ggplot2::geom_sf(data = ppt, color = 'black', size = 3, shape = 17, inherit.aes = F) +
  ggplot2::geom_sf_text(data = ppt, aes(label = 'Piney Point'), size = 3, hjust = 0, vjust = 0.2, nudge_x = 0.015, inherit.aes = F, ) +
  scale_fill_manual(values = c('lost' = "red", gained = "green")) +
  thm + 
  theme(
    axis.text.y = ggplot2::element_blank()
  ) +
  labs(
    fill = '(b) Areas lost or gained'
  ) +
  ggplot2::coord_sf(xlim = dat_ext[c(1, 3)], ylim = dat_ext[c(2, 4)], expand = FALSE, crs = 4326)


m <- m1 + m2 + plot_layout(ncol = 2)

png(here("figs", "seagrasschange.png"), width = 7.5, height = 5, units = "in", res = 400)
print(m)
dev.off()

# site closure from emails --------------------------------------------------------------------

load(file = here::here('data/emailpars.RData'))

lbs <- tibble(
  dts = as.Date(c('2022-09-28', '2022-11-11', '2023-04-01', '2023-08-30', '2023-09-19', '2024-09-26', '2024-10-10')),
  labs = c('Hurricane\nIan', 'Hurricane\nNicole', 'Injection\nonline', 'Hurricane\nIdalia', 'OGS-S closed', 'Hurricane\nHelene', 'Hurricane\nMilton')
) |> 
  crossing(
    type = factor(c('mgallcur', 'instorcur'), levels = c('mgallcur', 'instorcur'), labels = c('NGS-S million gallons', 'Site rainfall\n capacity (inches)'))
  ) |> 
  filter(type == 'NGS-S million gallons')

toplo <- emailpars |> 
  pivot_longer(cols = c(instorcur, mgallcur), names_to = "type", values_to = "value") |> 
  mutate(
    type = factor(type, levels = c("mgallcur", "instorcur"), labels = c('NGS-S million gallons', 'Site rainfall\n capacity (inches)'))
  )

p <- ggplot() +
  geom_vline(xintercept = as.numeric(lbs$dts), color = 'rosybrown3') +
  geom_line(data = toplo, aes(x = date, y = value)) +
  geom_point(data = toplo, aes(x = date, y = value), size = 0.5) +
  geom_text_repel(data = lbs[!grepl('Milton|Helene|Idalia', lbs$labs), ], aes(x = dts, label = labs), y = 175, color = 'tomato1', box.padding = 0.5,
                  min.segment.length = 0, point.padding = 0, force = 10) +
  geom_text_repel(data = lbs[grepl('Milton|Helene|Idalia', lbs$labs), ], aes(x = dts, label = labs), y = 275, color = 'tomato1', box.padding = 0.5,
                  min.segment.length = 0, point.padding = 0, force = 10) +
  facet_wrap(~ type, scales = "free_y", strip.position = 'left', ncol = 1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
  theme_minimal() +
  theme(
    strip.placement = "outside", 
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(angle = 0, size = 12),
    axis.text.x = element_text(size = 12),
    panel.spacing = unit(0, "lines"),
  ) +
  labs(
    x = NULL,
    y = NULL
  )

png(here::here("figs", "siteclose.png"), width = 9, height = 4, units = "in", res = 400)
print(p)
dev.off()

