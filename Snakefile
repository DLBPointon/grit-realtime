rule run_jira_connect_python:
    shell:
        "python3 scripts/jira_connect.py -USER dp24 -PASS SamWinDam12! -SAVE ./output/"


rule run_grit_grapher_R:
    shell:
        "Rscript scripts/grit_graphs.R"