# Implementing real time surveillance of genome curation impact on assembly quality
By Damon-Lee B Pointon

Project grit-realtime

Proposal:
Genome assembly curation has a significant impact on assembly quality, and allows for the identification of opportunities for improvement within automated assembly generation. Analysing a multitude of assembly parameters is required, ideally in real time, in order to document the impact and elucidate opportunities. This is currently implemented by ad hoc extraction of the data from a Jira tracking database with a perl script and subsequent graph generation in Excel, a system that requires significant manual intervention. 
 
In order to streamline and further extend and adapt the process, we intend to produce an approach using Python and R, to interact with the Jira API as well as generate graphs in an automated and consistent fashion. This will ultimately result in the production of a dashboard/website that provides a real-time report on curation impacts for specified data groups, within a specified time frame.
