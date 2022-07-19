library(tidyverse)
options(scipen = 999)

# Importando dados Inflação
bd <- rbcb::get_series(c(IPCA = 433))

# Inserindo presidentes
bd <- bd %>% 
  mutate(Presidente = case_when(
    date <= "1985-03-01" ~ "João Figueiredo",
    date > "1985-03-01" & date <= "1990-03-01" ~ "José Sarney",
    date > "1990-03-01" & date <= "1992-12-01" ~ "Fernando Collor",
    date > "1992-12-01" & date <= "1994-12-01" ~ "Itamar Franco",
    date > "1994-12-01" & date <= "2002-12-01" ~ "Fernando Henrique Cardoso",
    date > "2002-12-01" & date <= "2010-12-01" ~ "Lula",
    date > "2010-12-01" & date <= "2016-08-01" ~ "Dilma Rousseff",
    date > "2016-01-08" & date <= "2018-12-01" ~ "Michel Temer",
    TRUE ~ "Jair Bolsonaro"
  ))

# Alterando Labels para incluir mandato
bd$Presidente <- factor(bd$Presidente,
                        levels = c("João Figueiredo",
                                   "José Sarney",
                                   "Fernando Collor",
                                   "Itamar Franco",
                                   "Fernando Henrique Cardoso",
                                   "Lula",
                                   "Dilma Rousseff",
                                   "Michel Temer",
                                   "Jair Bolsonaro"),
                        labels = c("João Figueiredo\n(1979-1985)",
                                   "José Sarney\n(1985-1990)",
                                   "Fernando Collor\n(1990-1992)",
                                   "Itamar Franco\n(1992-1994)",
                                   "Fernando Henrique Cardoso\n(1995-2002)",
                                   "Lula\n(2003-2010)",
                                   "Dilma Rousseff\n(2011-2016)",
                                   "Michel Temer\n(2016-2018)",
                                   "Jair Bolsonaro\n(2019-Presente)"))




# PLOTS!!!!!! -------------------------------------------------------------

# Evolução Inflação total
inf_tot <- ggplot(bd) +
  aes(x = date, y = IPCA) +
  geom_line(size = 1) +
  # INSERINDO NOTA MAIOR INFLAÇÃO REGISTRADA
  # geom_vline(xintercept=as.Date("1990-03-01"), col = "blue", linetype = 2, size = 0.65) +
  annotate(geom = "text", x = as.Date("1985-01-01"), y = 80, 
           label = "Maior inflação registrada\n82.39% em Mar/1990", size = 4) +
  
  # INSERINDO PLANOS MONETÁRIOS
  geom_point(aes(x=as.Date("1984-08-01"), y = 9.35), size=6, shape=1, 
             color="blue", stroke = 1.5, alpha = I(0.01)) +
  geom_point(aes(x=as.Date("1986-02-01"), y = 12.72), size=6, shape=1, 
             color="blue", stroke = 1.5, alpha = I(0.01)) +
  geom_point(aes(x=as.Date("1989-01-01"), y = 37.49), size=6, shape=1, 
             color="blue", stroke = 1.5, alpha = I(0.01)) +
  geom_point(aes(x=as.Date("1990-03-01"), y = 82.39), size=6, shape=1, 
             color="blue", stroke = 1.5, alpha = I(0.01)) +
  geom_point(aes(x=as.Date("1993-07-01"), y = 30.72), size=6, shape=1, 
             color="blue", stroke = 1.5, alpha = I(0.01)) +
  geom_point(aes(x=as.Date("1994-06-01"), y = 47.43), size=6, shape=1, 
             color="blue", stroke = 1.5, alpha = I(0.01)) +
  
  # INSERINDO NOTA INÍCIO PLANO REAL
  geom_vline(xintercept=as.Date("1994-07-01"), col = "blue", linetype = 2, size = 0.65) +
  annotate(geom = "text", x = as.Date("1998-02-01"), y = 47.43, 
           label = "Início do Plano Real\nem Jul/94", size = 4) +
  
  scale_x_date(date_breaks = "1 year", expand = c(0.01, 0.01), date_labels =  "%Y") +
  labs(title = "Comportamento da Inflação brasileira",
       subtitle = "Cada círculo representa um novo Padrão Monetário\n",
       x = "",
       y = "IPCA Mensal (%)\n",
       caption = "Feito por: Artur Almeida\nFonte: BC") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 10, face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


# Evolução Inflação pós Plano Real
inf_tot_pr <- bd %>% 
  filter(date >= "1994-08-01") %>% 
ggplot() +
  aes(x = date, y = IPCA) +
  geom_line(size = 1, group = 1) +
  scale_x_date(date_breaks = "1 year", expand = c(0.01, 0.01), date_labels =  "%Y") +
  ylim(-0.8, 4) +
  # ITAMAR
  geom_vline(xintercept=as.Date("1994-12-01"), col = "blue", linetype = 2, size = 0.65) +
  # annotate(geom = "text", x = as.Date("1994-09-01"), y = 4,
  #          label = "Itamar\nFranco", size = 4) +
  # FHC
  geom_vline(xintercept=as.Date("2002-12-01"), col = "blue", linetype = 2, size = 0.65) +
  annotate(geom = "text", x = as.Date("1998-06-01"), y = 3.5,
           label = "Fernando Henrique\nCardoso", size = 4) +
  # Lula
  geom_vline(xintercept=as.Date("2010-12-01"), col = "blue", linetype = 2, size = 0.65) +
  annotate(geom = "text", x = as.Date("2007-02-01"), y = 3.5,
           label = "Lula", size = 4) +
  # Dilma
  geom_vline(xintercept=as.Date("2016-08-01"), col = "blue", linetype = 2, size = 0.65) +
  annotate(geom = "text", x = as.Date("2013-12-01"), y = 3.5,
           label = "Dilma Rousseff", size = 4) +
  # Temer
  geom_vline(xintercept=as.Date("2018-12-01"), col = "blue", linetype = 2, size = 0.65) +
  annotate(geom = "text", x = as.Date("2017-10-01"), y = 3.5,
           label = "Michel\nTemer", size = 4) +
  # Bolsonaro
  # geom_vline(xintercept=as.Date("2018-12-01"), col = "blue", linetype = 2, size = 0.65) +
  annotate(geom = "text", x = as.Date("2021-01-01"), y = 3.5,
           label = "Jair\nBolsonaro", size = 4) +
    labs(title = "Comportamento da Inflação brasileira",
       subtitle = "Pós Plano Real\n",
       x = "",
       y = "IPCA Mensal (%)\n",
       caption = "Feito por: Artur Almeida\nFonte: BC",
       color = "") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 10, face = "bold"),
        legend.position = "bottom")

# Plot panel - Escala fixa
inf_panel_fixa <- ggplot(bd) +
  aes(x = date, y = IPCA, color = Presidente) +
  geom_line(size = 1) +
  scale_color_manual(values = c("João Figueiredo\n(1979-1985)" = "red",
                                "José Sarney\n(1985-1990)" = "red",
                                "Fernando Collor\n(1990-1992)" = "red",
                                "Itamar Franco\n(1992-1994)" = "blue",
                                "Fernando Henrique Cardoso\n(1995-2002)" = "red",
                                "Lula\n(2003-2010)" = "blue",
                                "Dilma Rousseff\n(2011-2016)" = "blue",
                                "Michel Temer\n(2016-2018)" = "red",
                                "Jair Bolsonaro\n(2019-Presente)" = "red")) +
  facet_wrap(~ Presidente, scales = "free_x", nrow = 3) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  labs(title = "Comportamento da Inflação durante cada Presidente",
       subtitle = "Escala fixa\n",
       x = "",
       y = "IPCA Mensal (%)\n",
       caption = "Feito por: Artur Almeida\nFonte: BC") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5),
        legend.position = "none",
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 13))

# Plot panel - Escala livre
inf_panel_livre <- ggplot(bd) +
  aes(x = date, y = IPCA, color = Presidente) +
  geom_line(size = 1) +
  scale_color_manual(values = c("João Figueiredo\n(1979-1985)" = "red",
                                "José Sarney\n(1985-1990)" = "red",
                                "Fernando Collor\n(1990-1992)" = "red",
                                "Itamar Franco\n(1992-1994)" = "blue",
                                "Fernando Henrique Cardoso\n(1995-2002)" = "red",
                                "Lula\n(2003-2010)" = "blue",
                                "Dilma Rousseff\n(2011-2016)" = "blue",
                                "Michel Temer\n(2016-2018)" = "red",
                                "Jair Bolsonaro\n(2019-Presente)" = "red")) +
  facet_wrap(~ Presidente, scales = "free", nrow = 3) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  labs(title = "Comportamento da Inflação durante cada Presidente",
       subtitle = "Escala livre\n",
       x = "",
       y = "IPCA Mensal (%)\n",
       caption = "Feito por: Artur Almeida\nFonte: BC") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5),
        legend.position = "none",
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 13))

### Gráfico dumbbell - total
# Filtrando primeiro e último mês de cada mandato

bd_f <- bd %>% 
  group_by(Presidente) %>% 
  filter(row_number() == 1 | row_number() == n())

# Alterando data para início e fim
bd_f <- bd_f %>% 
  group_by(Presidente) %>% 
  mutate(Mandato = row_number()) %>% 
  mutate(Mandato = case_when(
    Mandato == "1" ~ "Início",
    Mandato == "2" ~ "Fim"
  ))

inf_dumbbell_tot <- ggplot(bd_f) +
  aes(x = IPCA, y = Presidente) +
  geom_line(aes(group = Presidente)) +
  geom_point(aes(color = Mandato, alpha = I(.8), size=I(5))) +
  scale_color_manual(values = c("Início" = "darkblue", "Fim" = "red")) +
  labs(title = "Como era a inflação no início e no fim dos mandatos de cada Presidente?",
       x = "\nIPCA Mensal (%)",
       y = "",
       color = "",
       caption = "Feito por: Artur Almeida\nFonte: BC") +
  theme_bw() +
  theme(legend.position="top",
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 13)) +
  guides(color = guide_legend(override.aes = list(size = 4.5)))

### Gráfico dumbbell - pós Plano Real
inf_dumbbell_pr <- bd_f %>% 
  filter(date >= "1995-01-01") %>% 
ggplot() +
  aes(x = IPCA, y = Presidente) +
  geom_line(aes(group = Presidente)) +
  geom_point(aes(color = Mandato, alpha = I(.8), size=I(5))) +
  scale_color_manual(values = c("Início" = "darkblue", "Fim" = "red")) +
  labs(title = "Como era a inflação no início e no fim dos mandatos de cada Presidente?",
       subtitle = "Pós Plano Real",
       x = "\nIPCA Mensal (%)",
       y = "",
       color = "",
       caption = "Feito por: Artur Almeida\nFonte: BC") +
  theme_bw() +
  theme(legend.position="top",
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 13)) +
  guides(color = guide_legend(override.aes = list(size = 4.5)))

# EXPORTANDO
ggsave(inf_tot, filename = here::here("inf_tot.png"),dpi = 1000, width = 10, height = 6)
ggsave(inf_tot_pr, filename = here::here("inf_tot_pr.png"),dpi = 1000, width = 10, height = 6)
ggsave(inf_panel_livre, filename = here::here("inf_panel_livre.png"),dpi = 1000, width = 10, height = 6)
ggsave(inf_panel_fixa, filename = here::here("inf_panel_fixa.png"),dpi = 1000, width = 10, height = 6)
ggsave(inf_dumbbell_tot, filename = here::here("inf_dumbbell_tot.png"),dpi = 1000, width = 10, height = 6)
ggsave(inf_dumbbell_pr, filename = here::here("inf_dumbbell_pr.png"),dpi = 1000, width = 10, height = 6)


