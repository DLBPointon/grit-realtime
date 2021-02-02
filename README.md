# Implementing real time surveillance of genome curation impact on assembly quality
By Damon-Lee B Pointon

Project grit-realtime

## Proposal:
Genome assembly curation has a significant impact on assembly quality, and allows for the identification of opportunities for improvement within automated assembly generation. Analysing a multitude of assembly parameters is required, ideally in real time, in order to document the impact and elucidate opportunities. This is currently implemented by ad hoc extraction of the data from a Jira tracking database with a perl script and subsequent graph generation in Excel, a system that requires significant manual intervention. 
 
In order to streamline and further extend and adapt the process, we intend to produce an approach using Python and R, to interact with the Jira API as well as generate graphs in an automated and consistent fashion. This will ultimately result in the production of a dashboard/website that provides a real-time report on curation impacts for specified data groups, within a specified time frame.

## Project Outline:
### Phase 1 - Data Harvest from Jira
In Python 3.17, I have produced a script which pulls data from the online ticketing platform used in the team, Jira. This data includes various statistics, from pre and post curation, about the genome at hand.

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
