library(tidyverse)

sba = read_csv("https://uofi.box.com/shared/static/vi37omgitiaa2yyplrom779qvwk1g14x.csv")

#fix all data formats
sba$GrAppv = as.numeric(gsub("[\\$,]", "", sba$GrAppv))
sba$SBA_Appv = as.numeric(gsub("[\\$,]", "", sba$SBA_Appv))
sba$DisbursementGross = as.numeric(gsub("[\\$,]", "", sba$DisbursementGross))
sba$BalanceGross = as.numeric(gsub("[\\$,]", "", sba$BalanceGross))
sba$ChgOffPrinGr = as.numeric(gsub("[\\$,]", "", sba$ChgOffPrinGr))
sba$ApprovalDate = as.Date(sba$ApprovalDate, format = "%d-%b-%y")
sba$DisbursementDate = as.Date(sba$DisbursementDate, format = "%d-%b-%y")

sba_defaults = sba %>% drop_na(MIS_Status, ApprovalFY) %>%
  group_by(ApprovalFY) %>%
  count(MIS_Status) %>%
  mutate(defaultrate = n / sum(n)) %>%
  filter(MIS_Status == "CHGOFF",
         ApprovalFY %in% 1994:2014)

country_avg_default = mean(sba_defaults$defaultrate)

sba_business = sba %>% drop_na(NewExist, UrbanRural, MIS_Status, ApprovalFY) %>%
  filter(NewExist != 0, UrbanRural != 0) %>%
  mutate(UrbanRural = case_when(UrbanRural == 1 ~ "Urban",
                                UrbanRural == 2 ~ "Rural"),
         NewExist = case_when(NewExist == 2 ~ "NewBusiness",
                              NewExist == 1 ~ "ExistingBusiness")) %>%
  group_by(ApprovalFY, NewExist, UrbanRural) %>%
  count(MIS_Status) %>%
  mutate(defaultrate = n / sum(n)) %>%
  filter(MIS_Status == "CHGOFF")



plot = ggplot() +
  geom_rect(aes(xmin = 2006.5, xmax = 2009.5, ymin = 0, ymax = .50), fill = "palegreen1", alpha = .6) +
  geom_rect(aes(xmin = 2001, xmax = 2002, ymin = 0, ymax = .50), fill = "palegreen1", alpha = .6) +
  #annotate(geom = "curve", x = 2000, y = .0, xend = 2007.5, yend = .10,
  #         curvature = .3, arrow = arrow(length = unit(2, "mm")), lwd = 1.5) +
  #annotate(geom = "text", x = 2000, y = .025, label = "bold(Recession)", parse = TRUE) +
  annotate(geom = "text", x = 2008, y = .045, label = "bold(Recession)", size = 3.2, parse = TRUE) +
  annotate(geom = "text", x = 2001.4, y = .35, label = "bold(Recession)", size = 3.2, parse = TRUE, angle = 90) +
  geom_hline(aes(yintercept = country_avg_default, linetype = "Country Mean Default Rate"), color = "black", size = 1.3) +
  geom_line(data = sba_business, aes(x = ApprovalFY, y = defaultrate, color = interaction(NewExist, UrbanRural)), size = 1.3) + 
  scale_color_manual(values = c("ExistingBusiness.Rural" = "red",
                                "NewBusiness.Rural" = "orange",
                                "ExistingBusiness.Urban" = "blue",
                                "NewBusiness.Urban" = "mediumorchid2"),
                     labels = c("Existing Rural Business",
                                "New Rural Business",
                                "Existing Urban Business",
                                "New Urban Business")) +
  labs(color = "Business Type", linetype = "Mean Default Rates",
       x = "Loan Approval Year", y = "Default Rate (%)",
       title = "Default Rates vs Approval Year\nBy Business Types (1994 - 2014)") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_linetype_manual(values = c("Country Mean Default Rate" = 2)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.40),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"))

ggsave("default_rates_by_business.png", plot = plot)
