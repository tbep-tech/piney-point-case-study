library(tidyverse)
library(ggrepel)

# site closure from emails --------------------------------------------------------------------

load(file = here::here('data/email_bodies.RData'))

email_df <- data.frame(
  subject = sapply(email_bodies, function(x) x$subject),
  date = sapply(email_bodies, function(x) x$date),
  body = sapply(email_bodies, function(x) ifelse(length(x$body) == 0, NA, x$body[[1]])),
  stringsAsFactors = FALSE
)

gallons_pattern <- "^.*Approximately(.*)are currently held within the NGS-South compartment.*$"
capacity_pattern <- "^.*The current storage capacity for additional rainfall at the site is approximately(.*) This capacity.*$"
transfer_pattern <- "^.*to date(.*)have been transferred.*$"
inject_pattern <- "[^.!?]*\\bUIC\\b[^.!?]*[.!?]"

emailproc <- email_df |> 
  dplyr::filter(grepl('^Piney Point Update –.*', subject)) |> 
  dplyr::filter(!is.na(body)) |> 
  mutate(
    instorcur = gsub(capacity_pattern, '\\1', body),
    mgallcur = gsub(gallons_pattern, '\\1', body),
    mgallcur = gsub('\\D+', '', mgallcur), 
    # trancur = gsub(transfer_pattern, '\\1', body), 
    # injccur = regexpr(inject_pattern, body, perl = TRUE),
    # injccur = ifelse(injccur != -1, trimws(regmatches(body, injccur)[[1]]), NA)
    .by = c('subject', 'date', 'body')
  ) |> 
  mutate(
    mgallcur = as.numeric(gsub('\\D+', '', mgallcur)), 
    instorcur = gsub("[^0-9.]", "", instorcur),
    instorcur = as.numeric(gsub('\\.$', '', instorcur)), 
    date = as.Date(strptime(date, format="%a, %d %b %Y %H:%M:%S %z"))
  ) 

lbs <- tibble(
  dts = as.Date(c('2022-09-28', '2022-11-11', '2023-04-01', '2023-08-30', '2024-09-26', '2024-10-10')),
  labs = c('Hurricane\nIan', 'Hurricane\nNicole', 'Injection\nonline', 'Hurricane\nIdalia', 'Hurricane\nHelene', 'Hurricane\nMilton')
) |> 
  crossing(
    type = factor(c('mgallcur', 'instorcur'), levels = c('mgallcur', 'instorcur'), labels = c('NGS-S million gallons', 'Site rainfall\n capacity (inches)'))
  ) |> 
  filter(type == 'NGS-S million gallons')

toplo <- emailproc |> 
  select(date, instorcur, mgallcur) |> 
  bind_rows(
    tibble(
      date = as.Date(c('2024-08-09', '2024-10-25')),
      instorcur = c(90, 76),
      mgallcur = c(160.2, 193)
    )
  ) |>
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
