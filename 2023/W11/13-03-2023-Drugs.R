# remotes::install_github("hrbrmstr/ggchicklet")
# packages
pacman::p_load(tidyverse,
               ggview,
               ggtext)



# dados
df = tidytuesdayR::tt_load(week=11, 2023)$drugs



# manipulação de dados
med = df |> 
  filter(grepl('HIV', therapeutic_area), !is.na(decision_date)) |> 
  mutate(year = format(as.Date(decision_date), '%Y')) |>
  group_by(year) |>
  mutate(count=n()) |>
  ungroup() |>
  group_by(year, generic) |>
  mutate(gen.count = n(), perc = 100*gen.count/count,
         percT.label = case_when(generic==TRUE&perc!=100~paste0(round(perc),'%')),
         percF.label = case_when(generic==FALSE&perc!=100~paste0(round(perc), '%')))



# fontes
c(sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf'),
  sysfonts::font_add_google("Quicksand","quicksand"),
  sysfonts::font_add_google("Open Sans","opensans"),
  showtext::showtext_auto(),
  showtext::showtext_opts(dpi=150))



# elementos textuais
week = "<span style='font-family:quicksand;font-size:27pt;color:white;'> #TidyTuesday, **W11**</span>"

caption = paste0(
  "<span style='font-family:fb;font-size:24pt;color:white;'>&#xf099;</span>",
  "<span style='font-family:opensans;font-size:25pt;color:white;'> @taferreiraua </span>",
  "<span style='font-family:fb;font-size:24pt;color:white;'>&#xf09b;</span>",
  "<span style='font-family:opensans;font-size:25pt;color:white;'> taferreiraua</span>")

text = paste0("<span style='font-size:75pt;color:white;'>**HIV generic drug<br>development in Europe**<br></span>", 
              "<span style='font-size:39pt;color:white;'>The development of generic drugs for the treatment<br></span>",
              "<span style='font-size:39pt;color:white;'>of the **Human Immunodeficiency Virus (HIV)** in the<br></span>",
              "<span style='font-size:39pt;color:white;'>European continent is recent. Cheaper, the use of<br></span>",
              "<span style='font-size:39pt;color:white;'>generic drugs is recommended by the **World Health**<br></span>",
              "<span style='font-size:39pt;color:white;'>**Organization** to combat the spread of the virus in<br>developing countries.<br></span>",
              "<span style='font-size:39pt;color:white;'><br>The chart below shows the proportion of </span>",
              "<span style='font-size:39pt;color:#1D3557;'>**generic**</span>",
              "<span style='font-size:39pt;color:white;'> and<br></span>",
              "<span style='font-size:39pt;color:#457B9D;'>**non-generic**</span>",
              "<span style='font-size:39pt;color:white;'> drugs developed per year.<br></span>",
              "<span style='font-size:29pt;color:white;'><br>Data from **European Medicines Agency**.</span>")



# plot
ggplot(med) +
  geom_bar(aes(year, fill=generic)) +
  geom_text(aes(x=year, y=gen.count-.5, label=percT.label), 
            size=9, 
            fontface='bold',
            family='quicksand',
            color='white',
            hjust=1) +
  geom_text(aes(x=year, y=count-.5, label=percF.label), 
            size=9, 
            fontface='bold',
            family='quicksand',
            color='white',
            hjust=1) +
  geom_richtext(aes(x='1998', y=30, label=text), 
                fill=NA, 
                label.colour=NA,
                hjust=0) +
  scale_fill_manual(values=c('#457B9D', '#1D3557')) +
  scale_y_continuous(breaks=c(2, 5, 14, 45)) +
  labs(title=week, caption=caption) +
  theme_minimal() +
  theme(plot.background = element_rect(fill='skyblue', color='skyblue'),
        plot.title = element_markdown(hjust=1),
        plot.caption = element_markdown(hjust=-.03),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth=.7, linetype='dotted'),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text.y = element_text(color='white', family='quicksand', size=29),
        axis.text.x = element_text(color='white', family='quicksand', face='bold', size=32, 
                                   margin=margin(t=-15, b=15)))

  
ggview(units='px', width=3000, height=3900)
ggsave(units='px', width=3000, height=3900, filename='13-03-2023-drugs.png')
