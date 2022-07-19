library(tidyverse)
library(ggplot2)

# Abrindo df
bd <- rbcb::get_series(c(IPCA = 433))

# Inserindo presidentes
bd <- bd %>% 
  mutate(Presidente = case_when(
    date <= "1983-09-01" ~ "Carlos Langoni (01/1980-09/1980)",
    date > "1983-09-01" & date <= "1985-03-01" ~ "Affonso Celso Pastore (09/1983-03/1985)",
    date > "1985-03-01" & date <= "1985-08-01" ~ "Antonio Lemgruber (03/1985-08/1985)",
    date > "1985-08-01" & date <= "1987-02-01" ~ "Fernão Bracher (08/1985-02/1987)",
    date > "1987-02-01" & date <= "1987-04-01" ~ "Francisco Gros.1 (02/1987-04/1987)",
    date > "1987-04-01" & date <= "1988-03-01" ~ "Fernando Milliet (05/1987-03/1988)",
    date > "1988-03-01" & date <= "1989-06-01" ~ "Elmo de Araújo Camões (03/1988-06/1989)",
    date > "1989-06-01" & date <= "1990-02-01" ~ "Wadico Waldir Bucchi (06/1989-03/1990)",
    date > "1990-02-01" & date <= "1991-05-01" ~ "Ibrahim Eris (03/1990-05/1991)",
    date > "1991-05-01" & date <= "1992-11-01" ~ "Francisco Gros.2 (05/1991-11/1992)",
    date > "1992-11-01" & date <= "1993-03-01" ~ "Gustavo Loyola.1 (11/1992-03/1993)",
    date > "1993-03-01" & date <= "1993-09-01" ~ "Paulo Ximenes (03/1993-09/1993)",
    date > "1993-09-01" & date <= "1994-12-01" ~ "Pedro Malan (09/1993-12/1994)",
    date > "1994-12-01" & date <= "1995-06-01" ~ "Pérsio Arida (01/1995-06/1995)",
    date > "1995-06-01" & date <= "1997-08-01" ~ "Gustavo Loyola.2 (06/1995-08/1997)",
    date > "1997-08-01" & date <= "1999-03-01" ~ "Gustavo Franco (08/1997-03/1999)",
    date > "1999-03-01" & date <= "2002-12-01" ~ "Arminio Fraga (03/1999-12/2002)",
    date > "2002-12-01" & date <= "2010-12-01" ~ "Henrique Meirelles (01/2003-12/2010)",
    date > "2010-12-01" & date <= "2016-05-01" ~ "Alexandre Tombini (01/2011-06/2016)",
    date > "2016-05-01" & date <= "2019-02-01" ~ "Ilan Goldfajn (06/2016-02/2019)",
    date > "2019-02-01" & date <= "2022-06-01" ~ "Roberto Campos Neto (02/2019-Presente)"
  ))


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

# Gráficos
bd_f$Presidente <- factor(bd_f$Presidente,
                        levels = rev(c("Carlos Langoni (01/1980-09/1980)",
                                   "Affonso Celso Pastore (09/1983-03/1985)",
                                   "Antonio Lemgruber (03/1985-08/1985)",
                                   "Fernão Bracher (08/1985-02/1987)",
                                   "Francisco Gros.1 (02/1987-04/1987)",
                                   "Fernando Milliet (05/1987-03/1988)",
                                   "Elmo de Araújo Camões (03/1988-06/1989)",
                                   "Wadico Waldir Bucchi (06/1989-03/1990)",
                                   "Ibrahim Eris (03/1990-05/1991)",
                                   "Francisco Gros.2 (05/1991-11/1992)",
                                   "Gustavo Loyola.1 (11/1992-03/1993)",
                                   "Paulo Ximenes (03/1993-09/1993)",
                                   "Pedro Malan (09/1993-12/1994)",
                                   "Pérsio Arida (01/1995-06/1995)",
                                   "Gustavo Loyola.2 (06/1995-08/1997)",
                                   "Gustavo Franco (08/1997-03/1999)",
                                   "Arminio Fraga (03/1999-12/2002)",
                                   "Henrique Meirelles (01/2003-12/2010)",
                                   "Alexandre Tombini (01/2011-06/2016)",
                                   "Ilan Goldfajn (06/2016-02/2019)",
                                   "Roberto Campos Neto (02/2019-Presente)")))
# Plot total
inf_dumbbell_bc_tot <- ggplot(bd_f) +
  aes(x = IPCA, y = Presidente) +
  geom_line(aes(group = Presidente)) +
  geom_point(aes(color = Mandato, alpha = I(.8), size=I(5))) +
  scale_color_manual(values = c("Início" = "darkblue", "Fim" = "red")) +
  labs(title = "Como era a inflação no início e no fim dos mandatos dos Presidentes\ndo Banco Central?",
       x = "\nIPCA Mensal",
       y = "",
       color = "",
       caption = "Feito por: Artur Almeida\nFonte: BC") +
  theme_bw() +
  theme(legend.position="right",
        axis.text.y = element_text(size = 8.75),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 12))

# Plot recente
inf_dumbbell_bc_pr <- bd_f %>% 
  tail(n = -26) %>% 
ggplot() +
  aes(x = IPCA, y = Presidente) +
  geom_line(aes(group = Presidente)) +
  geom_point(aes(color = Mandato, alpha = I(.8), size=I(5))) +
  scale_color_manual(values = c("Início" = "darkblue", "Fim" = "red")) +
  labs(title = "Como era a inflação no início e no fim dos mandatos dos Presidentes\ndo Banco Central?",
       subtitle = "Pós Plano Real\n",
       x = "\nIPCA Mensal",
       y = "",
       color = "",
       caption = "Feito por: Artur Almeida\nFonte: BC") +
  theme_bw() +
  theme(legend.position="right",
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 13))

# Grid com evolução de todos os presidentes
bd$Presidente <- factor(bd$Presidente,
                          levels = c("Carlos Langoni (01/1980-09/1980)",
                                     "Affonso Celso Pastore (09/1983-03/1985)",
                                     "Antonio Lemgruber (03/1985-08/1985)",
                                     "Fernão Bracher (08/1985-02/1987)",
                                     "Francisco Gros.1 (02/1987-04/1987)",
                                     "Fernando Milliet (05/1987-03/1988)",
                                     "Elmo de Araújo Camões (03/1988-06/1989)",
                                     "Wadico Waldir Bucchi (06/1989-03/1990)",
                                     "Ibrahim Eris (03/1990-05/1991)",
                                     "Francisco Gros.2 (05/1991-11/1992)",
                                     "Gustavo Loyola.1 (11/1992-03/1993)",
                                     "Paulo Ximenes (03/1993-09/1993)",
                                     "Pedro Malan (09/1993-12/1994)",
                                     "Pérsio Arida (01/1995-06/1995)",
                                     "Gustavo Loyola.2 (06/1995-08/1997)",
                                     "Gustavo Franco (08/1997-03/1999)",
                                     "Arminio Fraga (03/1999-12/2002)",
                                     "Henrique Meirelles (01/2003-12/2010)",
                                     "Alexandre Tombini (01/2011-06/2016)",
                                     "Ilan Goldfajn (06/2016-02/2019)",
                                     "Roberto Campos Neto (02/2019-Presente)"),
                        labels = c("Carlos Langoni\n(01/1980-09/1980)",
                                   "Affonso Celso Pastore\n(09/1983-03/1985)",
                                   "Antonio Lemgruber\n(03/1985-08/1985)",
                                   "Fernão Bracher\n(08/1985-02/1987)",
                                   "Francisco Gros.1\n(02/1987-04/1987)",
                                   "Fernando Milliet\n(05/1987-03/1988)",
                                   "Elmo de Araújo Camões\n(03/1988-06/1989)",
                                   "Wadico Waldir Bucchi\n(06/1989-03/1990)",
                                   "Ibrahim Eris\n(03/1990-05/1991)",
                                   "Francisco Gros.2\n(05/1991-11/1992)",
                                   "Gustavo Loyola.1\n(11/1992-03/1993)",
                                   "Paulo Ximenes\n(03/1993-09/1993)",
                                   "Pedro Malan\n(09/1993-12/1994)",
                                   "Pérsio Arida\n(01/1995-06/1995)",
                                   "Gustavo Loyola.2\n(06/1995-08/1997)",
                                   "Gustavo Franco\n(08/1997-03/1999)",
                                   "Arminio Fraga\n(03/1999-12/2002)",
                                   "Henrique Meirelles\n(01/2003-12/2010)",
                                   "Alexandre Tombini\n(01/2011-06/2016)",
                                   "Ilan Goldfajn\n(06/2016-02/2019)",
                                   "Roberto Campos Neto\n(02/2019-Presente)"))


# Escala Fixa
inf_panel_bc_fixa <- ggplot(bd) +
  aes(x = date, y = IPCA, color = Presidente) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Carlos Langoni\n(01/1980-09/1980)" = "red",
                                "Affonso Celso Pastore\n(09/1983-03/1985)" = "red",
                                "Antonio Lemgruber\n(03/1985-08/1985)" = "red",
                                "Fernão Bracher\n(08/1985-02/1987)" = "red",
                                "Francisco Gros.1\n(02/1987-04/1987)" = "red",
                                "Fernando Milliet\n(05/1987-03/1988)" = "blue",
                                "Elmo de Araújo Camões\n(03/1988-06/1989)" = "red",
                                "Wadico Waldir Bucchi\n(06/1989-03/1990)" = "red",
                                "Ibrahim Eris\n(03/1990-05/1991)" = "blue",
                                "Francisco Gros.2\n(05/1991-11/1992)" = "red",
                                "Gustavo Loyola.1\n(11/1992-03/1993)" = "red",
                                "Paulo Ximenes\n(03/1993-09/1993)" = "red",
                                "Pedro Malan\n(09/1993-12/1994)" = "blue",
                                "Pérsio Arida\n(01/1995-06/1995)" = "red",
                                "Gustavo Loyola.2\n(06/1995-08/1997)" = "blue",
                                "Gustavo Franco\n(08/1997-03/1999)" = "red",
                                "Arminio Fraga\n(03/1999-12/2002)" = "red",
                                "Henrique Meirelles\n(01/2003-12/2010)" = "blue",
                                "Alexandre Tombini\n(01/2011-06/2016)" = "blue",
                                "Ilan Goldfajn\n(06/2016-02/2019)" = "red",
                                "Roberto Campos Neto\n(02/2019-Presente)" = "blue")) +
  facet_wrap(~ Presidente, scales = "free_x", nrow = 3) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  labs(title = "Comportamento da Inflação durante cada mandato de Presidente do BC",
       subtitle = "Escala Fixa\n",
       x = "",
       y = "IPCA Mensal\n",
       caption = "Feito por: Artur Almeida\nFonte: BC") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "none",
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 13))


# Escala Livre
inf_panel_bc_livre <- ggplot(bd) +
  aes(x = date, y = IPCA, color = Presidente) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Carlos Langoni\n(01/1980-09/1980)" = "red",
                                "Affonso Celso Pastore\n(09/1983-03/1985)" = "red",
                                "Antonio Lemgruber\n(03/1985-08/1985)" = "red",
                                "Fernão Bracher\n(08/1985-02/1987)" = "red",
                                "Francisco Gros.1\n(02/1987-04/1987)" = "red",
                                "Fernando Milliet\n(05/1987-03/1988)" = "blue",
                                "Elmo de Araújo Camões\n(03/1988-06/1989)" = "red",
                                "Wadico Waldir Bucchi\n(06/1989-03/1990)" = "red",
                                "Ibrahim Eris\n(03/1990-05/1991)" = "blue",
                                "Francisco Gros.2\n(05/1991-11/1992)" = "red",
                                "Gustavo Loyola.1\n(11/1992-03/1993)" = "red",
                                "Paulo Ximenes\n(03/1993-09/1993)" = "red",
                                "Pedro Malan\n(09/1993-12/1994)" = "blue",
                                "Pérsio Arida\n(01/1995-06/1995)" = "red",
                                "Gustavo Loyola.2\n(06/1995-08/1997)" = "blue",
                                "Gustavo Franco\n(08/1997-03/1999)" = "red",
                                "Arminio Fraga\n(03/1999-12/2002)" = "red",
                                "Henrique Meirelles\n(01/2003-12/2010)" = "blue",
                                "Alexandre Tombini\n(01/2011-06/2016)" = "blue",
                                "Ilan Goldfajn\n(06/2016-02/2019)" = "red",
                                "Roberto Campos Neto\n(02/2019-Presente)" = "blue")) +
  facet_wrap(~ Presidente, scales = "free", nrow = 3) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  labs(title = "Comportamento da Inflação durante cada mandato de Presidente do BC",
       subtitle = "Escala Livre\n",
       x = "",
       y = "IPCA Mensal\n",
       caption = "Feito por: Artur Almeida\nFonte: BC") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "none",
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 7),
        legend.text = element_text(size = 13))

# EXPORTANDO
ggsave(inf_dumbbell_bc_tot, filename = here::here("inf_dumbbell_bc_tot.png"),dpi = 1000, width = 10, height = 6)
ggsave(inf_dumbbell_bc_pr, filename = here::here("inf_dumbbell_bc_pr.png"),dpi = 1000, width = 10, height = 6)
ggsave(inf_panel_bc_fixa, filename = here::here("inf_panel_bc_fixa.png"),dpi = 1000, width = 10, height = 6)
ggsave(inf_panel_bc_livre, filename = here::here("inf_panel_bc_livre.png"),dpi = 1000, width = 10, height = 6)
