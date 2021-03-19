"""
Jira connect
Used by R script to connect to JIRA and pull data

# Eliminates all where there isn't a full complement of data for the ticket
"""
import argparse
import csv
import re
from datetime import date
from operator import itemgetter
import sys
from jira import JIRA
import maya
import requests

# Add logging


def parse_command_args(args=None):
    """
    A function to verify the command line arguments to be passed
    to the rest of the script.
    :param args:
    :return option:
    """
    parser = argparse.ArgumentParser(prog='jira_data.py',
                                     description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)

    parser.add_argument('-USER', '--JIRA-ID',
                        action='store',
                        help='ID used to log into JIRA',
                        type=str,
                        dest='user')

    parser.add_argument('-PASS', '--JIRA-PASSWORD',
                        action='store',
                        help='Password used to log into Jira',
                        type=str,
                        dest='passw')

    # Both of the above are passed to script via netrc?

    parser.add_argument('-SAVE', '--save-location',
                        action='store',
                        help='Where to save output files, default will be "./"',
                        type=str,
                        dest='save')

    option = parser.parse_args(args)
    return option


def reg_full_name(db_name):
    """
    Function to return the internal TolID + accession
    :param db_name:
    :return:
    """

    name_acc_search = re.search(r'_.*_([a-z]*[A-Z]\w+\d_\d)', db_name)  # catches fAplTae1_1 from dp24_vgp_fAplTae1_1
    if name_acc_search is None:
        name_acc_search = re.search(r'_.*_([a-z]*[A-Z]\w+\d)_', db_name)  # catches fAplTae1 from dp24_vgp_fAplTae1_asm6
        if name_acc_search is None:
            name_acc_search = re.search(r'_.*_([a-z]*[A-Z]\w+)', db_name)  # catches fAplTae from dp24_vgp_fAplTae
        else:
            pass

    name_acc = name_acc_search.group(1)
    return name_acc


def reg_length_info(scaff_data):
    """
    Function to return the length information hidden in assembly stats
    :param scaff_data:
    :return:
    """
    length_search = re.search(r'total\s*([0-9]\w+)\s*([0-9]\w+)', scaff_data)
    length_before = int(length_search.group(1))
    length_after = int(length_search.group(2))
    length_change_per = (length_after - length_before) / length_before * 100

    return length_before, length_after, length_change_per


def reg_n50_info(scaff_data):
    """
    Function to return the length information hidden in assembly stats
    :param scaff_data:
    :return:
    """
    n50_search = re.search(r'N50\s*([0-9]*)\s*([0-9]*)', scaff_data)
    n50_before = int(n50_search.group(1))
    n50_after = int(n50_search.group(2))
    n50_ab = n50_after - n50_before
    if n50_ab == 0:
        n50_change_per = 0
    else:
        n50_change_per = (n50_after - n50_before) / n50_before * 100

    return n50_before, n50_after, n50_change_per


def reg_scaff_count(scaff_data):
    """

    :param scaff_data:
    :return:
    """
    scaff_count_search = re.search(r'count\s*([0-9]*)\s*([0-9]*)', scaff_data)
    scaff_count_before = int(scaff_count_search.group(1))
    scaff_count_after = int(scaff_count_search.group(2))
    if scaff_count_before + scaff_count_after == 0:
        scaff_count_per = 0
    else:
        scaff_count_per = (scaff_count_after - scaff_count_before) / scaff_count_before * 100

    return scaff_count_before, scaff_count_after, scaff_count_per


def reg_chr_assignment(chromo_res):
    """
    Function to parse and return the chromosome assignment and assignment %
    :param chromo_res:
    :return:
    """
    chr_ass_search = re.search(r'(found.[0-9].*somes.*\(.*\))', chromo_res)
    if chr_ass_search:
        chr_ass = chr_ass_search.group(1)
    elif chr_ass_search is None:
        chr_ass_search = re.search(r'(found.[0-9].*somes)', chromo_res)
        if chr_ass_search:
            chr_ass = chr_ass_search.group(1)
        else:
            chr_ass = None
    else:
        chr_ass = None

    ass_percent_search = re.search(r'Chr.length.(\d*.\d*).%', chromo_res)
    if ass_percent_search:
        ass_percent = ass_percent_search.group(1)
    else:
        ass_percent = 'NA'

    return chr_ass, ass_percent


def reg_lat_name(latin_name):
    """
    A function to parse just the latin name from the returned value
    :param latin_name:
    :return:
    """
    lat_name_search = re.search(r'([A-Z]\S*.\w*)', latin_name)
    lat_name_result = lat_name_search.group(1)

    # For the occasional "genus_species" result rather than "genus species"
    if '_' in lat_name_result:
        split_here = lat_name_result.split('_')
        lat_name_result = " ".join(split_here)

    lat_name = lat_name_result.split(" ")

    response = requests.get(f'https://www.ebi.ac.uk/ena/taxonomy/rest/scientific-name/{lat_name[0]}%20{lat_name[1]}')

    if response.text == 'No results.':
        family_data = 'UNKNOWN'
    else:
        data = response.json()
        data_lineage = data[0]['lineage']
        family = data_lineage.split('; ')

        family_data = family[-3]

    return lat_name_result, family_data


def reg_make_prefix(sample_name):
    """
    Function to pull prefix, prefix_v and prefix_full from name_ass.
    from ilAliOxi1:
    prefix = i
    prefix_v = il
    prefix_full = insect, would hope to pull lepidoptera for more specificity
    :param sample_name:
    :return:
    """
    master_dict = {'a': 'Amphibia',
                   'b': 'Bird',
                   'c': 'Non-vascular plants',
                   'd': 'Dicotyledons',
                   'e': 'Echinoderms',
                   'f': 'Fish',
                   'g': 'Fungi',
                   'h': 'Platyhelminths',
                   'i': 'Insects',
                   'j': 'Jellyfish and Cnidaria',
                   'k': 'Other Chordates',
                   'l': 'Monocotyledons',
                   'm': 'Mammal',
                   'n': 'Nematodes',
                   'o': 'Sponges',
                   'p': 'Protists',
                   'q': 'Other Arthropods',
                   'r': 'Reptile',
                   's': 'Shark',
                   't': 'Other Animal Phyla',
                   'u': 'Algae',
                   'v': 'Other Vascular Plants',
                   'w': 'Annelids',
                   'x': 'Molluscs',
                   'y': 'Bacteria',
                   'z': 'Archae'
                   }

    insect_dict = {
        'i': 'Insect',
        'il': 'Lepidoptera',
        'iy': 'Hymenoptera',
        'id': 'Diptera',
        'ic': 'Coleoptera'
    }

    prefix_search = re.search(r'([a-z])', sample_name)
    prefix = prefix_search.group(1)

    prefix_v_search = re.search(r'([a-z]*)', sample_name)
    prefix_v = prefix_v_search.group(1)

    if master_dict.get(prefix) is not None:
        if prefix == 'i' and len(prefix_v) > 1:
            prefix_full = insect_dict.get(prefix_v)
        else:
            prefix_full = master_dict.get(prefix)
    else:
        print(f'Error, prefix ({prefix}) not found in master_dict')
        sys.exit()

    return prefix, prefix_v, prefix_full


def date_parsing(date_obj):
    """
    A function to return a parsable date/time object for use in graphing by date
    :param date_obj:
    :return:
    """
    obj = maya.parse(date_obj).datetime()
    ymd_date = obj.date().strftime('%Y-%m-%d')

    return ymd_date


def record_maker(issue):
    """
    Function to control the logic of the script
    :return:
    """
    # Dictionaries for issue field name:api_code

    id_for_custom_field_name = {
        'GRIT_ID': issue,
        'Project': issue.fields.issuetype.name,
        'Date': issue.fields.created,
        'sample_id': issue.fields.customfield_10201,
        'gEVAL_database': issue.fields.customfield_10214,
        'species_name': issue.fields.customfield_10215,
        'assembly_statistics': issue.fields.customfield_10226,
        'chromosome_result': issue.fields.customfield_10233,
        'curator': issue.fields.customfield_10300,
        'hic_kit': issue.fields.customfield_10511,
        'lat_name': issue.fields.customfield_10215

    }

    interaction_list = {
        'manual_breaks': issue.fields.customfield_10219,
        'manual_joins': issue.fields.customfield_10220,
        'manual_inversions': issue.fields.customfield_10221,
        'manual_haplotig_removals': issue.fields.customfield_10222,
    }

    name_acc = ''
    length_before = 0
    length_after = 0
    length_change_per = 0
    n50_before = 0
    n50_after = 0
    n50_change_per = 0
    chr_ass = ''
    ass_percent = 0
    ymd_date = None
    interventions = 0
    scaff_count_before = 0
    scaff_count_after = 0
    scaff_count_per = 0
    lat_name = ''
    family_data = ''

    for field, result in id_for_custom_field_name.items():
        if field == 'gEVAL_database':
            name_acc = reg_full_name(result)
        if field == 'assembly_statistics':
            length_before, length_after, length_change_per = reg_length_info(result)
            n50_before, n50_after, n50_change_per = reg_n50_info(result)
            scaff_count_before, scaff_count_after, scaff_count_per = reg_scaff_count(result)
        if field == 'chromosome_result':
            if result is None:
                chr_ass = None
                ass_percent = None
            else:
                chr_ass, ass_percent = reg_chr_assignment(result)
        if field == 'Date':
            ymd_date = date_parsing(result)
        if field == 'lat_name':
            lat_name, family_data = reg_lat_name(result)

    else:
        pass

    for field, result in interaction_list.items():
        if result is None:
            pass
        else:
            interventions += int(result)

    return name_acc, lat_name, family_data, length_before, length_after, length_change_per, n50_before, n50_after, \
               n50_change_per, scaff_count_before, scaff_count_after, scaff_count_per, chr_ass, ass_percent, ymd_date, \
               interventions


# Perhaps a function to check whether theres already a file here would be a good idea?

def tsv_file_append(record, location):
    """
    appends rather than overwrites
    :return:
    """
    today = date.today()
    todays_date = today.strftime("%d%m%y")

    file_name = f'{location}jira_dump_{todays_date}.tsv'
    print('writing')
    with open(file_name, 'a+', newline='') as end_file:
        tsv_out = csv.writer(end_file, delimiter='\t')
        tsv_out.writerow(record)

    return file_name


def tsv_file_sort(file_name):
    """
    A function to open, read, sort and re-write the contents of a tsv file
    :param file_name:
    :return: file_name_sort
    """
    reader = csv.reader(open(file_name), delimiter="\t")
    file_name_sort = f'{file_name}.sorted'

    for line in sorted(reader, key=itemgetter(0)):
        with open(f'{file_name_sort}', 'a+', newline='') as end_file:
            tsv_out = csv.writer(end_file, delimiter='\t')
            tsv_out.writerow(line)

    return file_name_sort


def tsv_file_prepender(file_name_sort):
    """
    A function to prepend a column list to the output tsv file
    :param file_name_sort:
    :return:
    """
    with open(file_name_sort, 'r+') as file:
        original = file.read()
        file.seek(0, 0)  # Move the cursor to top line
        file.write(
            '#sample_id\tlatin_name\tprefix\tprefix_v\tprefix_full\tfamily_data\tkey\tproject_type\t'
            'length before\tlength after\tlength change\tscaff n50 before\tscaff n50 after\tscaff n50 change\t'
            'scaff_count_before\tscaff_count_after\tscaff_count_per\tchr assignment\tassignment\t'
            'date_in_YMD\tmanual_interventions\n')
        file.write(original)


def main():
    """
    Main function to control essential aspects of script
    :return:
    """
    option = parse_command_args()
    if option.save:
        location = option.save
    else:
        location = "../output/"

    jira = "https://grit-jira.sanger.ac.uk"  # Base url
    auth_jira = JIRA(jira, basic_auth=(option.user, option.passw))  # Auth

    # Jira JQL search for tickets that are past the curation stage
    projects = auth_jira.search_issues('project = "Assembly curation" and status = Done OR status = Submitted OR '
                                       'status = "In Submission" OR status = "Post Processing++" ORDER BY key ASC',
                                       maxResults=10000)
    # fields = ('assignee', 'summary', 'description')  # Specific Fields of interest
    file_name = ''

    print(len(projects))

    for i in projects:
        issue = auth_jira.issue(f'{i}')  # Needs to be set before id_custom_field_name
        print(f'----STARTING {issue} ----')
        summary = issue.fields.summary
        summary_search = re.search(r'(not being curated)', summary)
        if summary_search:
            nbc = summary_search.group(1)
            if nbc == '(not being curated)':
                pass
        else:
            if issue.fields.customfield_10226 is None:
                pass
            else:
                #  --- Block requires no parsing
                project_type = issue.fields.issuetype
                lat_name = issue.fields.customfield_10215
                #  -- End of Block

                name_acc, lat_name, family_data, length_before, length_after, length_change_per, n50_before, \
                    n50_after, n50_change_per, scaff_count_before, scaff_count_after, scaff_count_per, chr_ass, \
                    ass_percent, ymd_date, interventions = record_maker(issue)

                prefix, prefix_v, prefix_label = reg_make_prefix(name_acc)

                record = [name_acc, lat_name, prefix, prefix_v, prefix_label, family_data, issue, project_type,
                          length_before, length_after, length_change_per, n50_before, n50_after, n50_change_per,
                          scaff_count_before, scaff_count_after, scaff_count_per, chr_ass, ass_percent, ymd_date,
                          interventions]
                file_name = tsv_file_append(record, location)
                print(record)
                print(f'---- END OF {issue} ------')
    print('SORTING')
    file_name_sort = tsv_file_sort(file_name)
    tsv_file_prepender(file_name_sort)
    print('FIN')


if __name__ == "__main__":
    main()
