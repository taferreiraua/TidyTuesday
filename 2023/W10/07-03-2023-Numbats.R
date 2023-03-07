#packages
pacman::p_load(tidyverse,
               ggview,
               ggtext,
               ggimage)


# dados
df = tidytuesdayR::tt_load(2023, week = 10)$numbats


# elementos textuais
c(sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf'),
  sysfonts::font_add_google("Montserrat","montserrat"),
  sysfonts::font_add_google("Open Sans","opensans"),
  showtext::showtext_auto(),
  showtext::showtext_opts(dpi=150))

title=paste0("<span style='font-family:montserrat;font-size:55pt;color:white;'><br>**Numbats sightings recorded since 2016**</span>")
subtitle = paste0("<span style='font-family:montserrat;font-size:29pt;color:white;'>The **numbat** once populated much of southern semi-arid and arid Australia, extending from the<br></span>",
                  "<span style='font-family:montserrat;font-size:29pt;color:white;'>west coast to northwest Victoria and western New South Wales. By the 1970s, numbats had<br></span>",
                  "<span style='font-family:montserrat;font-size:29pt;color:white;'>disappeared from most of their range (99%), surviving only in small areas of southwest Australia.<br></span>",
                  "<span style='font-family:montserrat;font-size:29pt;color:white;'>They’re now considered endangered and it’s estimated that today there are less than 1,000<br></span>",
                  "<span style='font-family:montserrat;font-size:29pt;color:white;'>mature individuals left. *Font: World Wildlife Fund*.</span>")
caption <- paste0("<span style='font-family:opensans;'> **Data: Atlas of Living Australia**<br></span>",
                  "<span style='font-family:fb;'>&#xf099;</span>",
                  "<span style='font-family:opensans;'> @taferreiraua </span>",
                  "<span style='font-family:fb;'>&#xf09b;</span>",
                  "<span style='font-family:opensans;'> taferreiraua </span>",
                  "<span style='font-family:opensans;color:#71998D'>**#TidyTuesday**</span>")


# manipulação de dados
numbats = df |>
  select(eventDate) |>
  mutate(year = as.numeric(format(eventDate, '%Y'))) |>
  filter(!is.na(eventDate), year>2015, year<2023) |>
  group_by(year) |>
  mutate(n = n()) |>
  rowwise() |>
  mutate(seq = list(seq(0, n, 5))) |>
  unnest(cols = c(seq)) |>
  ungroup() |>
  distinct(year, seq, n)


# dotted segments
map_seg = data.frame(x=unique(numbats$year),
                     xend=unique(numbats$year),
                     y=rep(67, 7),
                     yend=c(52,27,52,57,22,57,22))


# numbat image
img = 'https://www.nrmstrategy.com.au/sites/default/files/Numbat2.png'


# plot
ggplot(numbats) +
  geom_image(aes(x=year, y=seq, image=img), size=.07) +
  geom_segment(aes(x=2016, xend=2022, y=70, yend=70), color='white', linewidth=.2) +
  geom_text(aes(x=year, y=68.4, label=n), family='montserrat', color='white', size=7.25) +
  geom_segment(aes(x=year, xend=year, y=70, yend=69.5), color='white', linewidth=.2) +
  geom_text(aes(x=2016, y=71, label='Number of sightings recorded per year:'), color='white', family='montserrat', fontface='bold', size=6.15, hjust=0) +
  geom_segment(data=map_seg, aes(x=x, xend=xend, y=y, yend=yend), color='white', linetype="dotted", linewidth=.75) +
  scale_x_continuous(limits=c(2015,2023), breaks=seq(2016, 2022, 1)) +
  scale_y_continuous(limits=c(0,71)) +
  labs(title=title, subtitle=subtitle, caption=caption) +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#111111', color='#111111'),
        plot.title = element_markdown(hjust=.5),
        plot.subtitle = element_markdown(hjust=.5),
        plot.caption = element_markdown(color='white', size=22, hjust=.97, margin=margin(b=10)),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family='montserrat', size=25.5, color='white', margin=margin(t=-15, b=35)))


ggview(units='px', height=4200, width=3700)
ggsave(units='px', height=4200, width=3700, filename='07-03-2023-Numbats.png')
