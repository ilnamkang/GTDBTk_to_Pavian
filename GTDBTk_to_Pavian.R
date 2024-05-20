library(openxlsx)
library(tidyverse)
library(naniar)

## Import and transform the data
gtdb <- read.xlsx("gtdbtk.xlsx", sheet = "Sheet1", cols = 1:2) %>%
  # Assume that you have combined
  # <prefix>.bac120.summary.tsv and <prefix>.ar53.summary.tsv files
  # and saved it in "Sheet1" of "gtdbtk.xlsx" file.
  separate(classification, sep = ";", into = c("Domain", "Phylum", "Class", "Order",
                                               "Family", "Genus", "Species")) %>%
  replace_with_na_all(condition = ~.x %in% c("d__", "p__", "c__", "o__",
                                             "f__", "g__", "s__")) %>% 
  # E.g., "s__" indicates that a genome is not classified into any GTDB species (putatively, novel species).
  # This step is to replace these strings at all taxonomic ranks with "NA".
  # I don't know exactly why this step is required.
  # But, omitting this step seems to lead to problems in final Sankey plots. 
  mutate(
    Species = str_c(Domain, Phylum, Class, Order, Family, Genus, Species, sep="|"),
    Genus = str_c(Domain, Phylum, Class, Order, Family, Genus, sep="|"),
    Family = str_c(Domain, Phylum, Class, Order, Family, sep="|"),
    Order = str_c(Domain, Phylum, Class, Order, sep="|"),
    Class = str_c(Domain, Phylum, Class, sep="|"),
    Phylum = str_c(Domain, Phylum, sep="|")
    )
  # This step is to make classification string at all taxonomic ranks
  # include information of higher ranks.
  # Order (from species to phylum) is important here.
  # If you reverse the order, there would be much redundancy.


## Summary stats
# Ugly codes for summarizing the number of genomes assigned to GTDB taxa at each taxonomic rank
Domain.stat <- gtdb %>% 
  group_by(Domain) %>% 
  summarise(Genome_abundance = n()) %>% 
  rename(Taxa = Domain)

Phylum.stat <- gtdb %>% 
  group_by(Phylum) %>% 
  summarise(Genome_abundance = n()) %>% 
  rename(Taxa = Phylum)

Class.stat <- gtdb %>% 
  group_by(Class) %>% 
  summarise(Genome_abundance = n()) %>% 
  rename(Taxa = Class)

Order.stat <- gtdb %>% 
  group_by(Order) %>% 
  summarise(Genome_abundance = n()) %>% 
  rename(Taxa = Order)

Family.stat <- gtdb %>% 
  group_by(Family) %>% 
  summarise(Genome_abundance = n()) %>% 
  rename(Taxa = Family)

Genus.stat <- gtdb %>% 
  group_by(Genus) %>% 
  summarise(Genome_abundance = n()) %>% 
  rename(Taxa = Genus)

Species.stat <- gtdb %>% 
  group_by(Species) %>% 
  summarise(Genome_abundance = n()) %>% 
  rename(Taxa = Species)

# Combine the stat
gtdb.stat <- bind_rows(Domain.stat, Phylum.stat, Class.stat,
                     Order.stat, Family.stat,
                     Genus.stat, Species.stat) %>% 
  drop_na()
  # "drop_na()" seems to be necessary for more accurate plots.


## Edit for compatibility with Pavian.
# I tried to imitate the output format of
# https://github.com/bluenote-1577/sylph-utils/blob/main/sylph_to_taxprof.py
# Also refer to
# https://github.com/bluenote-1577/sylph/wiki/Taxonomic-profiling-with-the-GTDB%E2%80%90R214-database
gtdb.stat.pavian <- gtdb.stat %>% 
  mutate(relative_abundance = 100 * Genome_abundance / nrow(gtdb)) %>% 
  relocate(Genome_abundance, .after = relative_abundance) %>% 
  add_column(Dummy = "NA") 

# Save the results
write_tsv(gtdb.stat.pavian, file = "GTDBTk_Pavian.txt")

# Edit the result file as below in your text editor (not in R).
# Insert "#mpa_v3" at the first line.
# Add "#" before "Taxa".
---
#mpa_v3
#Taxa	relative_abundance	SAG_abundance	Dummy
d__Archaea	0.2604166666666667	1	NA
d__Bacteria	92.70833333333333	356	NA
---

## Visualization
# Uplad the file at https://fbreitwieser.shinyapps.io/pavian/

# OR, If you want to do it in local,
install.packages("remotes") # Only if required
remotes::install_github("fbreitwieser/pavian") # Only for the first time

pavian::runApp(port=5000, maxUploadSize="512M")  
# A Shiny interface pop-up will appear -> You can also open in Chrome
