# Example preprocessing script.


# GDELT Isreal Palestine SQL query -----------------------------------------------------------------------
query <- "
SELECT
  Actor1CountryCode AS source_iso,
  Actor2CountryCode AS target_iso,
  CONCAT(SUBSTR(CAST(MonthYear AS STRING), 1, 4), '-', SUBSTR(CAST(MonthYear AS STRING), 5, 2)) AS yearmonth, # yearmonth format 202309 into 2023-09
  SUM(CASE WHEN AvgTone < 0 THEN 1 ELSE 0 END) AS neg, # negative events
  COUNT(*) AS n # total events
FROM
  `gdelt-bq.gdeltv2.events`
WHERE
  (Actor1CountryCode = 'ISR' AND Actor2CountryCode = 'PSE')
  OR (Actor1CountryCode = 'PSE' AND Actor2CountryCode = 'ISR')
  AND MonthYear BETWEEN 199501 AND 202312 # Jan1995-Dec2023 to replicate GPI report, GDELT has data from 1979
GROUP BY
  source_iso, target_iso, yearmonth
ORDER BY
  source_iso, target_iso, yearmonth
"
# not clear on 'event':
# do i need to do anything with NumMentions (total number of mentions of this event across all source documents during the 15 minute update in which it was first seen) or 
# NumSources (total number of information sources containing one or more mentions of this event during the 15 minute update in which it was first seen) or
# NumArticles ( total number of source documents containing one or more mentions of this event during the 15 minute update in which it was first seen)



# full script ----------------------------------------------------------------------------------------------
library(bigrquery)
library(dplyr)
library(ggplot2)
library(zoo)

project_id <- "your_project_id"
bgcreds(filepath)


# Run the query
gdelt_data <- bq_table_download(
  bq_project_query(project_id, query)
)

# Process the data to compute proportions and moving averages ----------- chatgpt based on GPI graph code, someone check
dyadic <- gdelt_data %>%
  mutate(yearmonth = as.Date(paste0(yearmonth, "-01")),
         id = as.numeric(as.factor(yearmonth))) %>%
  group_by(source_iso, target_iso, yearmonth, id) %>%
  summarize(prop = sum(neg) / sum(n),
            n = sum(n)) %>%
  ungroup() %>%
  mutate(source = ifelse(source_iso == "ISR", "Israel", "Palestine")) %>%
  group_by(source) %>%
  mutate(avg = rollapply(prop, 24, FUN = mean, partial = TRUE),
         year = format(yearmonth, "%Y"))

# Plotting the data
pCHART_ISRPSE <- ggplot(dyadic, aes(x = id, y = prop, group = source, colour = source)) +
  geom_line(alpha = 0.7) +
  geom_line(aes(y = avg), size = 1.1) +
  scale_x_continuous(name = "", breaks = seq(1, max(dyadic$id), by = 48),
                     labels = unique(dyadic$year)[seq(1, length(unique(dyadic$year)), by = 4)]) +
  scale_colour_manual(values = c("#343868", "#71c5a6")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  labs(title = "Tensions Between Israel and Palestine in the Media, 1995-2023",
       x = "",
       y = "% NEGATIVE STORIES")

# Display the plot
print(pCHART_ISRPSE)

# Chart metadata ?? prob dont need
CHART_ISRPSE <- list(
  title = "Tensions Between Israel and Palestine in the Media, 1995-2023",
  sheet = "ISRPSE",
  source = "GDELT, IEP Calculations",
  xtext = "",
  ytext = "% NEGATIVE STORIES",
  type = "Chart",
  position = "Normal"
)



# how they did it w/icews -----------------------------------------------------------------------
db_get_icews = function(source_countries_codes, target_countries_codes = NULL){

  suppressMessages(library(icews)  )
  suppressMessages(library(DBI))
  suppressMessages(library(tidyverse))
  icews_con <- RSQLite::dbConnect(RSQLite::SQLite(), default_icews_location())
  countries = dbReadTable(icews_con, "countrycodes")
  source_countries = countries %>% filter(ID_0 %in% source_countries_codes) %>% pull(countryname) %>%
    gsub("'", "''", .)
  source_countries = paste(paste0("'", source_countries, "'"), collapse = ",")
  if(is.null(target_countries_codes)){
    target_countries = countries %>% pull(countryname) %>%
      gsub("'", "''", .)
    target_countries = paste(paste0("'", target_countries,"'"), collapse = ",")
  }else{
    target_countries = countries %>% filter(ID_0 %in% target_countries_codes) %>% pull(countryname) %>%
      gsub("'", "''", .)
    target_countries = paste(paste0("'", target_countries,"'"), collapse = ",")
  }

  dbDisconnect(icews_con)
  sql = paste("SELECT * FROM events WHERE source_country IN (", source_countries, ") AND
                  target_country IN (", target_countries, ");")
  df <- query_icews(sql, db_path = default_icews_location())
  df <- df %>% left_join(countries %>% rename(source_country = countryname, source_iso = ID_0))
  df <- df %>% left_join(countries %>% rename(target_country = countryname, target_iso = ID_0))
  df <- df %>% relocate(source_iso, target_iso)
  return(df)
}

codes <- c("ISR", "PSE") 

icews = db_get_icews(source_countries_codes = codes,
                     target_countries_codes = codes)

dyadic <- icews %>%
  dplyr::filter(source_iso == "ISR" | target_iso == "ISR",
                source_iso!=target_iso) %>%
  group_by(source_iso,target_iso, yearmonth) %>%
  reframe(neg = sum(grepl('^14|^15|^17|^18|^19|^20|^09|^9|^10|^11|^12|^13|^16', cameo_code)),
          n=n(),
          prop=neg/n) %>%
  mutate(id=as.numeric(as.factor(yearmonth)))

CHART_ISRPSE.df <- dyadic %>%
  dplyr::filter(source_iso =="PSE" & target_iso =="ISR"| source_iso =="ISR" & target_iso =="PSE") %>%
  mutate(source=ifelse(source_iso=="ISR","Israel","Palestine")) %>%
  group_by(source,yearmonth,id) %>%
  reframe(prop=sum(neg)/sum(n),
          n=sum(n)) %>%
  group_by(source) %>%
  mutate(avg=rollapply(prop, 24, FUN=mean,partial=T)) %>% # number is the number of months
  group_by(source) %>%
  mutate(year=substr(yearmonth,1,4))

pCHART_ISRPSE <- ggplot(CHART_ISRPSE.df) + 
  geom_line(aes(id,prop,group=source,colour=source),alpha=0.7) +
  geom_line(aes(id,avg,group=source,colour=source), size=1.1) +
  scale_x_continuous(name="",breaks=seq(1,337,by=48),
                     label=unique(CHART_ISRPSE.df$year)[seq(1, length(unique(CHART_ISRPSE.df$year)), by = 4)]) +
  scale_colour_manual(name="Moving average by actor:",values=c("#343868","#71c5a6")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

pCHART_ISRPSE <- f_ThemeGPI(plot = pCHART_ISRPSE, 
                               chart_info = CHART_ISRPSE, 
                               plottitle = "", 
                               xaxis = "Include", 
                               yaxis = "Include", 
                               xgridline = "", 
                               ygridline = "",
                               source = "Include") +
  scale_y_continuous(limits=c(0,1))
