#packages
pacman::p_load(tidyverse,
               ggtext,
               ggview)



# dados
df = tidytuesdayR::tt_load(week=12, 2023)$languages



# fontes e elementos textuais
c(sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf'),
  sysfonts::font_add_google("Raleway","raleway"),
  sysfonts::font_add_google("Open Sans","opensans"),
  showtext::showtext_auto(),
  showtext::showtext_opts(dpi=150))

title = "<span style='font-size:32pt;font-family:raleway;'>**R** is the 8th most used programming language today</span>"

subtitle = "<span style='font-size:19pt;font-family:raleway;'>R is a programming language developed by the New Zealander statistician Ross Ihaka and the<br>Canadian statistician Robert Gentleman in 1993. It is very **popular in statistical computing and<br>graphics**, for performing data analysis and developing statistical software.</span>"

caption = paste0("<span style='font-family:opensans;font-size:13pt;'>**Source:** *Programming Language DataBase*<br>",
                 "<span style='font-family:fb;font-size:10pt;'>&#xf099;</span>",
                 "<span style='font-family:opensans;font-size:10pt;'> @taferreiraua </span>",
                 "<span style='font-family:fb;font-size:10pt;'> &#xf09b;</span>",
                 "<span style='font-family:opensans;font-size:10pt;'> taferreiraua </span>",
                 "<span style='font-family:raleway;font-size:12pt;color:#33415C'> **#TidyTuesday**</span>")

labels = paste0("<span style='font-size:17pt;'><b>", lang$title, "</b><br></span>",
                "<span style='font-size:16pt;'>", scales::comma(lang$number_of_users), "</span>")



# manipulação de dados
lang = df |>
  filter(type=='pl') |>
  select(title, number_of_users) |>
  mutate(title = case_when(title=='Arduino Programming Language'~'Arduino', TRUE ~ title),
         color = case_when(title=='R'~'black', TRUE~'grey')) |>
  arrange(desc(number_of_users)) |>
  head(10)




# plot
ggplot(lang) +
  geom_col(aes(y=reorder(title, number_of_users), x=number_of_users + 60000, fill=color)) +
  geom_richtext(aes(y=reorder(title, number_of_users), x=number_of_users + 50000, label=labels, color=color),
                hjust=1, fill=NA, label.colour=NA, lineheight=.9) +
  scale_fill_manual(values=c('#33415C', '#C0C0C0')) +
  scale_color_manual(values=c('white', 'black')) +
  labs(title=title, subtitle=subtitle, caption=caption) +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#EBEBEB', color='#EBEBEB'),
        plot.margin = margin(t=20, b=10),
        plot.title = element_markdown(hjust=.5),
        plot.subtitle = element_markdown(hjust=.5),
        plot.caption = element_markdown(size=10, hjust=.97),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank())


ggview(units='px', height=2000, width=2000)
ggsave(units='px', height=2000, width=2000, filename='21-03-2023-Lang.png')

