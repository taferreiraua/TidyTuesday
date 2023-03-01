library(tidyverse)
library(waffle)
library(ggview)
library(ggtext)
library(scales)


# dados
df = tidytuesdayR::tt_load(2023, week = 9)
afrisenti = df$afrisenti
languages = df$languages


# fontes e textos
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Spectral","spectral")
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

title = "<span style='font-family:spectral;font-size:40pt'>**POSITIVIDADE NO TWITTER AFRICANO**</span>"
subtitle = paste0("<span style='font-family:spectral;font-size:25pt;'>Proporção de sentimentos</span>", 
                  "<span style='font-family:spectral;font-size:25pt;color:#FFB703;'> positivos</span>",
                  "<span style='font-family:spectral;font-size:25pt;'> de acordo com a análise de mais de 70mil tweets em linguas africanas.<br></span>",
                  "<span style='font-family:spectral;font-size:23pt;'>Dados: <i>African Language Sentiment - AfriSenti.<i><br></span>")
caption <- paste0(
  "<span style='font-family:fb;color:#FFB703;'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua |</span>",
  "<span style='font-family:fb;color:#FFB703;'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> taferreiraua |</span>",
  "<span style='font-family:opensans;color:#FFB703;'> **#TidyTuesday**</span>")


# manipulação de dados
data = afrisenti |>
  left_join(languages, by='language_iso_code') |>
  select(language, label) |>
  mutate(label = case_when(label!='positive'~'other', TRUE ~ label)) |>
  group_by(language, label) |>
  count(label) |>
  ungroup() |>
  group_by(language) |>
  mutate(total = sum(n),
         perc = (100 * n)/total,
         perc.round = round(perc),
         pos.rate = case_when(label=='positive'~paste0(round(perc, 1), '%'), TRUE ~ ''),
         language = case_when(language=='Algerian Arabic/Darja'~'Árabe Argelino<br>(Darja)',
                              language=='Amharic'~'Amárico',
                              language=='Hausa'~'Hauçá',
                              language=='Kinyarwanda'~'Quiniaruanda',
                              language=='Moroccan Arabic/Darija'~'Árabe Marroquino<br>(Darija)',
                              language=='Mozambican Portuguese'~'Português<br>Moçambicano',
                              language=='Nigerian Pidgin'~'Pidgin Nigeriano',
                              language=='Swahili'~'Suaíli',
                              language=='Tigrinya'~'Tigrínia',
                              language=='Twi'~'Axante',
                              language=='Xitsonga'~'Tsonga',
                              language=='Yorùbá'~'Iorubá',
                              TRUE ~ language),
         strip.lang = factor(language, levels=language, 
                             labels=paste0("<span style='font-size:23pt;'><b>", toupper(language), "</b><br></span>", 
                                           "<span style='font-size:6pt;'><br></span>",
                                           "<span style='font-size:18pt;'>", comma(total), "<span style='font-size:17pt;'> tweets</span>")))


# plot
ggplot(data) +
  geom_waffle(aes(fill=label, values=perc.round), flip=T, color='white', size=1) +
  coord_equal() +
  facet_wrap(~strip.lang, ncol=5) +
  geom_text(aes(x=10, y=1.7, label=pos.rate), family='spectral', size=19, hjust=1) +
  scale_fill_manual(values=c('#D3D3D9', '#FFB703')) +
  labs(title=title, subtitle=subtitle, caption=caption) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color='white', fill='white'),
    plot.title = element_markdown(hjust=.5),
    plot.subtitle = element_markdown(hjust=.5),
    plot.caption = element_markdown(size=16, hjust=.98),
    plot.margin = margin(b=-25),
    panel.grid = element_blank(),
    strip.text = element_markdown(family='spectral', lineheight = .6),
    strip.clip = 'off',
    legend.position = 'none',
    axis.text = element_blank(),
    axis.title = element_blank()
  )


ggview(units='px', height=2800, width=3000)
ggsave(units='px', height=2800, width=3000, filename='28-02-2023-AfriSenti.png')
