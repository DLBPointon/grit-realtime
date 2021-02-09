# Implementing real time surveillance of genome curation impact on assembly quality
By Damon-Lee B Pointon

Project grit-realtime

## Proposal:
Genome assembly curation has a significant impact on assembly quality, and allows for the identification of opportunities for improvement within automated assembly generation. Analysing a multitude of assembly parameters is required, ideally in real time, in order to document the impact and elucidate opportunities. This is currently implemented by ad hoc extraction of the data from a Jira tracking database with a perl script and subsequent graph generation in Excel, a system that requires significant manual intervention. 
 
In order to streamline and further extend and adapt the process, we intend to produce an approach using Python and R, to interact with the Jira API as well as generate graphs in an automated and consistent fashion. This will ultimately result in the production of a dashboard/website that provides a real-time report on curation impacts for specified data groups, within a specified time frame.

## Project Outline:
### Phase 1 - Data Harvest from Jira
In Python 3.17, I have produced a script which pulls data from the online ticketing platform used in the team, Jira. This data includes various statistics, from pre and post curation, about the genome at hand.

Currently planning for this to be cron based. e.g. run at 10:30 everyday to see is there have been updates in the previous day.
Also planning an ability to overwrite the automatically produced TSV with a user supplied one (this will have to be added as a second tab to the app).


This utilises:

|Module | Reason |
|---|---|
|Argparse    | - for cli |
|csv         | - for tsv formatted writing|
|operator    | - used in sorting the tsv|
|regex (re)  | - for extensive regex use|
|maya        | - for string to datetime (used as initial string used formatting I was unclear with)|
|datetime    | - for datetime to str conversion|
|jira        | - python-jira is a python api wrapper for Jira|
|logging     | - Used for logging (will be implemented towards the end)|

## Phase 2 - R script graph and statistic generation
This will initially be trialled in jira_data.R to ensure that graphs are correct and agreed upon.

This utilises:

|Module | Reason |
|---|---|
|ggplot2 | - for graphing data |

## Phase 3 - RShiny App generation

Once code has been tested it is ported over to app.R which generates the App/Website.
The plan is for this to be a dynamic dashboard, currently (02-02-2021) this is static but usable.

This utilises:

|Module | Reason |
|---|---|
|ggplot2 | - for graphing data |
|shiny | - Rshiny application |
|shinydashboard | - dashboard implementation of RShiny |
|shinythemes | - Customising the look of RShiny |

## TO-DO List

|Job | Done? |
| :---: | :---: |
| Ability to split by Clade ||
| Specify Dates for graphing ||
| Add more documentation ||
| Add much more variety of graphs ||
| - Box graphs ||
| - Trend Lines ||



## Usage
If used on the tol-farm, If used independently of the cron job:

- conda activate R

- python3 {Directory containing}/jira_dump.py -USER {jira_user_id} -PASS {jira_pass} -SAVE {location for saving tsvs}

-- SAVE should be default as ```./output/``` (if scripts are being run from the grit-realtime folder)
and only changed if used separately from app.R (the R application, which is hard-coded for the output folder).

This will produce the tsv in the format required by app.R which is run by updating the website on the Shiny platform which requires secrets:
[grit-realtime](https://grit-realtime.shinyapps.io/scripts/).