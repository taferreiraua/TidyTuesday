# packages
pacman::p_load(tidyverse, ggview, ggtext, rnaturalearth, sf, geomtextpath)


# dados
df = tidytuesdayR::tt_load(week=13, 2023)
transitions = df$transitions
timezones = df$timezones
timezone_countries = df$timezone_countries
countries = df$countries


# fontes e texto 
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Raleway","raleway")
sysfonts::font_add_google("Open Sans","opensans")
sysfonts::font_add_google("Quicksand","quicksand")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)


title = paste0('<span style="font-family:raleway;font-size:55pt;">Living between<br></span>',
               '<span style="font-family:raleway;font-size:85pt;">YESTERDAY </span>',
               '<span style="font-family:raleway;font-size:55pt;">and</span>',
               '<span style="font-family:raleway;font-size:85pt;"> TOMORROW</span>')

# https://www.ef.com.br/blog/language/fatos-fusos-horarios/
subtitle = paste0('<span style="font-family:raleway;font-size:27pt;">In the 1880s, an international convention decided that the zero point for measuring time<br>',
                  'zones would be in London, thus creating an imaginary vertical line that would cut the<br>',
                  'Earth from north to south. It was also created exactly on the opposite side of the 0º meridian,<br>',
                  'the 180º meridian, which cuts across the Pacific Ocean in almost its entire length. From <br>',
                  'then on you can literally time travel across the Pacific, crossing the International Date Line.<br></span>')
caption = paste0(
  "<span style='font-family:opensans;font-size:19pt;'>Data from the <i>Internet Assigned Numbers Authority tz database</i>.<br></span>",
  "<span style='font-family:fb;font-size:16pt;'>&#xf099;</span>",
  "<span style='font-family:opensans;font-size:17pt;'> @taferreiraua </span>",
  "<span style='font-family:fb;font-size:16pt;'>&#xf09b;</span>",
  "<span style='font-family:opensans;font-size:17pt;'> taferreiraua</span>",
  "<span style='font-family:opensans;font-size:17pt;'> **#TidyTuesday**</span>")



# mapa
# https://www.r-bloggers.com/2022/10/pacific-island-choropleth-map-by-ellis2013nz/
mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE)) |>
  as_tibble()
mp2 <- mp1 |>
  mutate(long = long + 360,
         group = group + max(mp1$group) + 1)
mp <- rbind(mp1, mp2) |>
  filter(long > 90  & long <360 & lat <50 & lat > -60) |>
  mutate(color = case_when(region=='Samoa'~'c1', 
                           region=='Tonga'~'c2',
                           region=='Wallis and Futuna'~'c3',
                           region=='Niue'~'c4',
                           TRUE ~ 'grey'))


# International Date Line
glines <- ne_download(type = "geographic_lines", 
                      category = "physical", 
                      returnclass = "sf") |>
  st_shift_longitude() |>
  filter(name == "International Date Line")


# zonas do pacifico
tz = timezones |> 
  filter(grepl('Pacific', zone)) |>
  mutate(zone = case_when(grepl('Pacific', zone)~str_remove(zone, 'Pacific/')),
         comments = case_when(is.na(comments)~zone, TRUE~comments),
         labels = case_when(zone=='Apia'~'SAMOA', 
                            zone=='Tongatapu'~'TONGA',
                            zone=='Wallis'~'WALLIS ISLAND'))

time = transitions |>
  filter(zone%in%c('Pacific/Tongatapu', 'Pacific/Niue', 'Pacific/Wallis', 'Pacific/Samoa')) |>
  mutate(datetime = lubridate::as_datetime(begin),
         year = format(datetime, '%Y'),
         offset = abs(offset/3600)) |>
  arrange(desc(year), zone) |>
  group_by(zone) |>
  slice_head(n=1)


# diferença de tempo
Niue = time[1,]$offset
Samoa = time[2,]$offset
Tonga = time[3,]$offset
Wallis = time[4,]$offset

Diff_Samoa = paste0(Niue + Samoa, 'h')
Diff_Tonga = paste0(Niue + Tonga, 'h')
Diff_Wallis = paste0(Niue + Wallis, 'h')


# mapa das setas
map_segments = data.frame(
  x=c(188.5, 184, 185.1),
  xend=c(190.1, 189.97, 189.92),
  y=c(-14.2, -13.4, -21.1),
  yend=c(-18.81, -18.93, -19.1),
  label=c(Diff_Samoa, Diff_Wallis, Diff_Tonga)
)


# plot
ggplot() +
  geom_polygon(data=mp, aes(x=long, y=lat, group=group, fill=color)) +
  geom_sf(data = glines, colour = '#FFEE32', linetype = 1, alpha = 0.3) +
  geom_richtext(data=tz, 
            aes(x=longitude+360, y=latitude+.3, label=labels), 
            size=7,
            color='white',
            fill=NA, 
            label.color=NA,
            fontface='bold',
            family='raleway') +
  geom_richtext(aes(x=189.8, y=-12.4, label='<i><b>International Date Line'), 
            size=5.5,
            color = '#FFEE32', 
            alpha = 0.6, 
            fill=NA, 
            label.color=NA) +
  geom_textsegment(inherit.aes = FALSE,
                   data = map_segments,
                   mapping=aes(x=x, xend=xend, y=y, yend=yend, label=label), 
                   color="white", size=6, hjust=0.25, fontface='bold',
                   linecolor="white", linetype='dashed', linewidth=.4, family='raleway') +
  geom_curve(aes(x=185.3, xend=184.4, y=-14.9, yend=-15.5),
             arrow = arrow(length = unit(0.08, "inch")), 
             size = 0.5,
             curvature = 0.3,
             color='white') +
  geom_richtext(aes(x=184.4, y=-15.75, label='Time difference<br>compared to <b>Niue.'),
                size=5.5,
                color='white',
                lineheight=.5,
                fill=NA, 
                label.color=NA,
                family='raleway') +
  geom_text(aes(x=190.5, y=-19.2, label='NIUE'),
            color='white',
            size=7,
            fontface='bold',
            family='raleway') +
  coord_sf(xlim = c(182, 193),  ylim = c(-22, -12)) +
  theme_minimal() +
  scale_fill_manual(values=c('#76C878', '#70E000', '#90A955', '#D9ED92', 'grey70')) +
  labs(title=title, subtitle=subtitle, caption=caption) +
  theme(panel.background = element_rect(fill='#1E6091'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='#F1F2F3', color='#F1F2F3'),
        plot.title = element_markdown(hjust=.5, lineheight=3.2),
        plot.margin = margin(t=40, l=20, r=22.5, b=20),
        plot.subtitle = element_markdown(hjust=.5),
        plot.caption = element_markdown(hjust=.5, lineheight=1.4, margin=margin(t=10)),
        axis.title=element_blank(),
        axis.text = element_blank(),
        legend.position = 'none')

ggview(units='px', height=3500, width=3300)
ggsave(units='px', height=3500, width=3300, filename='27-03-2023-Time-Zones.png')
