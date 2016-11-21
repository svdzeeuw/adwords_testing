# h1_labels <- c("%Nodig%","%Kopen%", "%Bedrukken%",
#                "%Nieuwste|Nieuwe|201[6:7]%", "%Naam%","%Outlet%")                      
# h2_labels <- c("%Alle Maten%", "%Maat niet Goed%",
#                "%Altijd Gratis%", "%Ontdek|Nieuwe%", "%Bestel NU%",
#                "%(Koop voor)*23%", "%NU Besteld%", "%Personaliseer%")
# desc_labels <- c("%Direct%", "%Merken|Maten%","%Al Meer|klanten%",
#                  "%Scherpe Prijzen%", "%Personaliseer%")

library(dplyr)
#Data2 can be produced with desc_extract.Rmd

data2 <- data2[data2$Adgroup %in% 'Voetbalschoenen',]
hl1 <- summarise(group_by(data2, theme1), sum(ConversionValue), sum(Impressions), sum(Clicks),mean(CTR))
hl1$'conv/impressions' <- hl1$`sum(ConversionValue)`/hl1$`sum(Impressions)`

hl2 <- summarise(group_by(data2, theme2), sum(ConversionValue), sum(Impressions), sum(Clicks),mean(CTR))
hl2$'conv/impressions' <- hl2$`sum(ConversionValue)`/hl2$`sum(Impressions)`

desc <- summarise(group_by(data2, theme_Description), sum(ConversionValue), sum(Impressions), sum(Clicks),mean(CTR))
desc$'conv/impressions' <- desc$`sum(ConversionValue)`/desc$`sum(Impressions)`

all_combo <- summarise(group_by(data2, theme1, theme2,theme_Description),
                       sum(ConversionValue), sum(Impressions), sum(Clicks),mean(CTR))
all_combo$'conv/impressions' <- all_combo$`sum(ConversionValue)`/all_combo$`sum(Impressions)`


campaign_name <- "SKAG"
AD_group <- c("Voetbalschoenen")
h1_labs <- c("%Kopen%", "%Nodig?%")
h2_labs <- c("%NU Besteld, Morgen op Het Veld%", "%Altijd Gratis Verzending.%")
desc_labs <- c("Al Meer Dan 1 Miljoen Tevreden Klanten. Ontdek De Mooiste Collectie Van Ons Land",
               "Meer Dan 1000 Voetbalschoenen, Altijd Op Voorraad! Scherpe Prijzen.Bestel Nu.")#"Meer dan 1000 %s, Altijd Op Voorraad! Scherpe Prijzen.Bestel Nu.")
url_template <- "http://www.voetbalshop.nl/%s.html?no-blocks"
#url_template2 <- "http://www.voetbalshop.nl/fanshop/<AD_group>.html"
#url_template3 <- "http://www.voetbalshop.nl/<AD_group>/kunstgras.html"

# http://www.voetbalshop.nl/voetbalschoenen.html?no-blocks
# http://www.voetbalshop.nl/fanshop/trainingspakken.html
# http://www.voetbalshop.nl/fanshop/voetbalschoenen.html
# http://www.voetbalshop.nl/voetbalkleding/trainingspakken.html
# http://www.voetbalshop.nl/voetbalkleding/shirts.html
# http://www.voetbalshop.nl/voetbalkleding.html
# http://www.voetbalshop.nl/voetbalschoenen/kunstgras.html
# http://www.voetbalshop.nl/Zaalvoetbalschoenen.html?no-blocks


library(plyr)
l <- list(data.frame(h1_labs),data.frame(h2_labs), data.frame(desc_labs), data.frame(AD_group))
df <- do.call(rbind.fill, l)


# Create DF of all possible combinations of ADgroup,hl1, hl2 and description
df_combinations <- na.omit(expand.grid(df))
#df_combinations$Labels <- paste0("Desc:",df_combinations$desc_labs, "_H1:",
#                                 df_combinations$h1_labs, "_H2:", df_combinations$h2_labs,
#                                 "_All:", df_combinations$h1_labs, df_combinations$h2_labs,
#                                 df_combinations$desc_labs)
df_combinations$Campaign <- rep(campaign_name,length(df_combinations$AD_group))
df_combinations$Headline1 <- paste(df_combinations$AD_group,gsub("%","",df_combinations$h1_labs))
df_combinations$Headline2 <- gsub("%","",df_combinations$h2_labs)
df_combinations$Description <- sprintf(as.character(df_combinations$desc_labs),
                                       as.character(df_combinations$AD_group))
df_combinations$Path1 <- df_combinations$AD_group
df_combinations$Path2 <- rep("", length(df_combinations$AD_group))
df_combinations$"Final URL" <- rep(url_template, length(df_combinations$AD_group))
df_combinations$"Final URL" <- sprintf(as.character(df_combinations$`Final URL`),
                                       as.character(tolower(df_combinations$AD_group)))

df_combinations$hl1_id <- as.numeric(factor(as.vector(df_combinations$h1_labs))) + 10
df_combinations$hl2_id <- as.numeric(factor(as.vector(df_combinations$h2_labs))) + 20
df_combinations$desc_id<- as.numeric(factor(as.vector(df_combinations$desc_labs))) + 30
df_combinations$Labels <- paste0("hl1:", df_combinations$hl1_id, "_hl2:",
                                 df_combinations$hl2_id,"_desc:",
                                 df_combinations$desc_id)

final_df <- data.frame("Campaign" = df_combinations$Campaign,
                       "Ad group" = df_combinations$AD_group,
                       "Labels" = df_combinations$Labels,
                       "Headline 1" = df_combinations$Headline1,
                       "Headline 2" = df_combinations$Headline2,
                       "Description" = df_combinations$Description,
                       "Path 1" = df_combinations$Path1,
                       "Path 2" = df_combinations$Path2,
                       "Final URL" = df_combinations$`Final URL`)
write.table(final_df, sep = "\t",
            file = "ads_voetbalschoenen.csv",
            quote = F, row.names = F,
            fileEncoding = "UTF-16LE")

