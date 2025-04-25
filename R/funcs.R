# try simple load, download if fail
rdataload <- function(x){
  
  fl <- basename(x)
  obj <- gsub('\\.RData$', '', fl)
  flurl <- x
  
  # try simple load
  ld <- try(load(url(flurl)), silent = T)
  
  # return x if load worked
  if(!inherits(ld, 'try-error')){
    out <- get(obj)
  }
  
  # download x if load failed
  if(inherits(ld, 'try-error')){
    
    fl <- paste(tempdir(), fl, sep = '/')
    download.file(flurl, destfile = fl, quiet = T)
    load(file = fl)
    out <- get(obj)
    suppressMessages(file.remove(fl))
    
  }
  
  return(out)
  
}

# water quality plot fun, adapted from piney-point-manu repo, only area 1
wqplo_fun <- function(rswqdat, bswqdat, ppseg, vr, logtr = TRUE, rmfacet = FALSE, ttl, ylb, addrect = F){
  
  # segments
  ppseg <- ppseg %>% 
    rename(area = Name) %>% 
    group_by(area) %>% 
    summarise() %>% 
    st_make_valid() %>% 
    filter(area %in% 'Area 1') |> 
    mutate(
      area = factor(area)
    )
  
  cols <- c("#E16A86")

  rswqdat <- rswqdat %>% 
    filter(date < as.Date('2021-10-01')) %>% 
    filter(var %in% c('tn', 'chla', 'secchi')) %>% 
    filter(!qual %in% c('S', 'U')) %>% # remove secchi on bottom, nondetect for chla, tn
    filter(!(var == 'secchi' & val >= 9.5)) # outlier secchi
  
  nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')
  
  ##
  # wq data
  
  # unique weeks
  wklv <- rswqdat %>%
    pull(date) %>% 
    range
  wklv <- seq.Date(wklv[1], wklv[2],by = 'days') %>% 
    floor_date(unit = 'week') %>% 
    unique %>% 
    crossing(date = ., area = c('Area 1')) %>% 
    mutate(
      fillcl = factor(area, levels = unique(area), labels = cols), 
      fillcl = as.character(fillcl)
    )
  
  # monitoring data
  rswqtmp <- rswqdat %>% 
    filter(var == vr) %>% 
    filter(!station %in% nonbay) %>% 
    inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
    st_intersection(ppseg) %>% 
    st_set_geometry(NULL) %>% 
    select(-qual, -bswqstation, -nrmrng, -source, -source_lng, -uni, -lbunis) %>% 
    mutate(
      date = floor_date(date, unit = 'week'), 
      mo = month(date), 
      fillcl = factor(area, levels = levels(area), labels = cols), 
      fillcl = as.character(fillcl)
    ) %>% 
    left_join(wklv, ., by = c('date', 'area', 'fillcl'))
  
  # baseline data
  bswqtmp <- bswqdat %>% 
    select(-source, -uni) %>% 
    filter(var == vr) %>%
    filter(!(var == 'secchi' & grepl('S', qual))) %>% # remove secchi on bottom
    filter(yr > 2005) %>% 
    filter(!is.na(val)) %>% 
    inner_join(bsstatloc, ., by = 'station') %>% 
    st_intersection(ppseg) %>% 
    st_set_geometry(NULL) %>% 
    mutate(cens = grepl('U', qual)) %>% 
    group_by(mo, var, area) %>% 
    summarise(   
      avev = ifelse(
        any(cens), mean(NADA::cenfit(val, cens), na.rm = T),
        mean(val, na.rm = T)
      ),
      stdv = ifelse(
        any(cens), sd(NADA::cenfit(val, cens), na.rm = T),
        sd(val, na.rm = T)
      ),
      .groups = 'drop'
    ) %>%
    left_join(parms, by = 'var') %>% 
    mutate(
      avev = round(avev, sigdig), 
      stdv = round(stdv, sigdig), 
      minv = avev - stdv, 
      minv = pmax(0, minv),
      maxv = avev + stdv,
      lbunis = gsub('^.*\\s(\\(.*\\))$', '\\1', lbs), 
      lbunis = gsub('pH', '', lbunis), 
      datestr= paste0('2021-', mo, '-01'), 
      datestr = ymd(datestr), 
      dateend = ceiling_date(datestr, unit = 'month')
    )

  p1 <- ggplot() + 
    geom_rect(data = bswqtmp, aes(xmin = datestr, xmax = dateend, ymin = minv, ymax = maxv, group = mo, fill = 'Monthly baseline (mean +/- 1 sd)'), alpha = 0.2) +
    geom_jitter(data = rswqtmp, aes(x = date, y = val, group = date, color = area), alpha = 0.7, size = 0.5, width = 2) + 
    scale_fill_manual(NULL, values = 'blue') +
    scale_color_manual(values = cols, guide = F) + 
    scale_linetype_manual(values = 'dashed') + 
    scale_x_date(breaks = unique(rswqtmp$date), date_labels = '%b %d', expand = c(0.05, 0.05)) +
    labs(
      y = ylb, 
      title = ttl
    ) + 
    coord_cartesian(xlim = range(rswqtmp$date)) +
    theme_minimal(base_size = 11) + 
    theme(
      legend.position = 'top', 
      strip.background = element_blank(), 
      axis.title.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 4.5, angle = 45, hjust = 1)
    )
  
  if(logtr)
    p1 <- p1 + 
    scale_y_log10()

  if(addrect){
    
    scales <- ggplot_build(p1)$layout$panel_params[[1]]
    date_range <- as.Date(scales$x.range, origin = "1970-01-01")
    
    # Function to convert date to relative position (0-1)
    date_to_norm <- function(dates, date_range) {
      as.numeric(dates - date_range[1]) / as.numeric(diff(date_range))
    }
    
    # Calculate normalized positions for your rectangle
    x_min1 <- date_to_norm(as.Date("2021-03-24"), date_range)
    x_max1 <- date_to_norm(as.Date("2021-04-29"), date_range)
    x_min2 <- date_to_norm(as.Date("2021-06-02"), date_range)
    x_max2 <- date_to_norm(as.Date("2021-07-22"), date_range)
    
    p1 <- p1 + 
      annotation_custom(
        grob = roundrectGrob(
          x = (x_min1 + x_max1)/2,  # x center
          y = 0.5,                # y center
          width = x_max1 - x_min1,  # width in normalized units
          height = 1,           # height in normalized units
          r = unit(0.1, "snpc"),  # corner radius as a unit object
          gp = gpar(fill = cols, alpha = 0.2, col = NA)
        ),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      ) +
      annotation_custom(
        grob = roundrectGrob(
          x = (x_min2 + x_max2)/2,  # x center
          y = 0.5,                # y center
          width = x_max2 - x_min2,  # width in normalized units
          height = 1,           # height in normalized units
          r = unit(0.1, "snpc"),  # corner radius as a unit object
          gp = gpar(fill = cols, alpha = 0.2, col = NA)
        ),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      ) + 
      geom_text(
        aes(x = as.Date("2021-03-24"), y = 0.5, label = "diatoms"),
        color = cols, size = 2.5, hjust = -0.05
      ) +
      geom_text(
        aes(x = as.Date("2021-06-02"), y = 0.5, label = "red tide"),
        color = cols, size = 2.5, hjust = -0.05
      )
    
  }
  
  out <- p1
  
  return(out)
  
}
