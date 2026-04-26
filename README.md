# Insurance Data Analysis

Jag, en analytiker på det påhittade försäkringsbolaget Villkor i Finstil, har fått i uppdrag att

-   identifiera vilka faktorer som driver försäkringskostnader

-   utvärdera om nuvarande prissättning följer rimliga mönster

-   undersöka om en regressionsmodell kan användas som stöd vid framtida prissättning

## Start

1.  Klona repot

2.  Öppna projektet genom att dubbelklicka på insurance-data-analysis.Rproj

3.  Installera nödvändiga paket. Kör följande kod i konsolen för att installera alla bibliotek som krävs för analysen:

    install.packages(c("tidyverse", "naniar", "ggcorrplot", "modelsummary", "knitr")

## Så kör du analysen

-   Snabbmetoden: Öppna den färdigrenderade analysis.html för att se resultaten direkt

-   "Slow"-metoden: Öppna analysis.qmd och klicka på Render. Detta kör automatiskt hela kedjan av skript i mappen /scripts och skapar en helt ny rapport.

## Projektstruktur

-   data/: innehåller rådata (csv)

-   scripts/: koden uppdelad i logiska steg:

    -   01_load_data.R

    -   02_prepare_data.R

    -   03_descriptive_analysis.R

    -   04_regression_analysis.R

-   output/: alla figurer uppdelade per del:

    -   analysis

    -   eda

    -   regression

-   report/: den affärsmässiga rapporten till uppdragsgivaren försäkringsbolaget Villkor i finstil

-   analysis.qmd - den tekniska rapporten som knyter ihop kod och analys till ett enda härligt dokument

## Environment

-   R version: 4.5.3 (2026-03-11)

-   Paket:

    -   tidyverse 2.0.0

    -   naniar 1.1.0

    -   ggcorrplot 0.1.4.1

    -   modelsummary 2.6.0

    -   knitr 1.51

-   RStudio version: 2026.01.1
