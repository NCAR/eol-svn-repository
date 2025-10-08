#!/usr/bin/env python

import csv
import os

class GCMDKeyword(object):
    """
    This class represents a GCMD keyword and allows for the matching of two
    GCMDKeyword objects to see if they match.

    This class was designed to help match GCMD keywords from different keyword lists.
    """

    def __init__(self, cat, topic, term, v1, v2, v3, dv, uuid):
        self.category = cat
        self.topic = topic if topic != "" else "NULL"
        self.term = term if term != "" else "NULL"
        self.variable_1 = v1 if v1 != "" else "NULL"
        self.variable_2 = v2 if v2 != "" else "NULL"
        self.variable_3 = v3 if v3 != "" else "NULL"
        self.detail_variable = dv if dv != "" else "NULL"
        self.uuid = uuid.lower() if uuid != "" else "NULL"

    def is_match_uuid(self, keyword):
        """Checks if the keyword supplied matches the current keyword"""
        return self.uuid == keyword.uuid

    def is_match_variables(self, keyword):
        """Checks if the keyword supplied matches the current keyword by means of comparing variables"""
        if self.topic != keyword.topic:
            return False

        if self.term != keyword.term:
            return False

        if self.variable_1 != keyword.variable_1:
            return False

        if self.variable_2 != keyword.variable_2:
            return False

        if self.variable_3 != keyword.variable_3:
            return False

        if self.detail_variable != keyword.detail_variable:
            return False

        return True

    def __repr__(self):
        str = self.category + "," + self.topic + ","
        str += self.term + "," + self.variable_1 + ","
        str += self.variable_2 + "," + self.variable_3 + ","
        str += self.detail_variable + "," + self.uuid

        return str


class Dataset(object):
    """
    This class represents a dataset containing GCMD keyword(s)
    """
    def __init__(self, archive_ident, dataset_id, cat, topic, term, v1, v2, v3, dv, uuid):
        self.archive_ident = archive_ident
        self.dataset_id = dataset_id
        self.gcmd_keyword = GCMDKeyword(cat, topic, term, v1, v2, v3, dv, uuid)

    def __repr__(self):
        str = self.archive_ident + "," + self.dataset_id + ","
        str += self.gcmd_keyword.__repr__()

        return str


def read_keywords_list(filename):
    """Reads a CSV file of GCMD keywords into a list of objects"""
    keywords = []
    with open(filename, 'rb') as keywords_csv:
        reader = csv.reader(keywords_csv, delimiter=',')
        for row in reader:
            if row[0] == "category":
                continue

            keywords.append(
                GCMDKeyword(row[0], row[1], row[2], row[3], row[4],
                            row[5], row[6], row[7]
                )
            )
    return keywords


def read_dataset_keywords(filename):
    """Reads a CSV file containing dataset IDs and their associated GCMD keywords"""
    datasets = []
    with open(filename, 'rb') as dataset_csv:
        reader = csv.reader(dataset_csv, delimiter=',')
        for row in reader:
            if row[0] == "ARCHIVE_IDENT":
                continue
            datasets.append(
                Dataset(row[0], row[1], "EARTH SCIENCE", row[2], row[3],
                        row[4], row[5], row[6], row[7], ""
                )
            )
    return datasets


def get_keyword_variables(keywords, keyword_to_find):
    """Takes in a list of keywords and a keyword and checks if it exists in the list. Matches on the keyword"""
    for keyword in keywords:
        if keyword.is_match_variables(keyword_to_find):
            return keyword

    return None


def get_keyword_uuid(keywords, keyword_to_find):
    """Takes in a list of keywords and a keyword and checks if it exists in the list. Matches on UUID"""
    for keyword in keywords:
        if keyword.is_match_uuid(keyword_to_find):
            return keyword

    return None


def main():
    """Main method"""
    old_keywords = read_keywords_list('science_keywords-old.csv')
    new_keywords = read_keywords_list('science_keywords.csv')

    datasets = read_dataset_keywords("Bering_Sea_datasets_and_keywords_linked.csv")

    # Match the keywords in the CSV to the "old" list
    unmatched_old_keywords = []
    matched_old_keywords = []
    for dataset in datasets:
        old_keyword = get_keyword_variables(old_keywords, dataset.gcmd_keyword)
        if old_keyword is None:
            unmatched_old_keywords.append(dataset)
        else:
            dataset.gcmd_keyword = old_keyword
            matched_old_keywords.append(dataset)

    print("Number of keywords that appear in the old GCMD keyword list: {}").format(len(matched_old_keywords))
    print("Number of keywords that don't appear in the old GCMD keyword list: {}").format(len(unmatched_old_keywords))

    unmatched_old_file = open("unmatched_keywords_in_old_list.txt", "w")
    for unmatched_old_keyword in unmatched_old_keywords:
        unmatched_old_file.write(unmatched_old_keyword.__repr__() + "\n")
    unmatched_old_file.close()

    # Take the "old" matches and match them agains the "new" keywords
    unmatched_new_keywords = []
    matched_new_keywords = []
    for dataset in matched_old_keywords:
        new_keyword = get_keyword_uuid(new_keywords, dataset.gcmd_keyword)
        if new_keyword is None:
            unmatched_new_keywords.append(dataset)
        else:
            dataset.gcmd_keyword = new_keyword
            matched_new_keywords.append(dataset)

    print("Number of keywords that appear in the new GCMD keyword list: {}").format(len(matched_new_keywords))
    print("Number of keywords that don't appear in the new GCMD keyword list: {}").format(len(unmatched_new_keywords))

    unmatched_new_file = open("unmatched_keywords_in_new_list.txt", "w")
    for unmatched_new_keyword in unmatched_new_keywords:
        unmatched_new_file.write(unmatched_new_keyword.__repr__() + "\n")
    unmatched_new_file.close()

    matched_new_file = open("matched_keywords_in_new_list.txt", "w")
    for matched_new_keyword in matched_new_keywords:
        matched_new_file.write(matched_new_keyword.__repr__() + "\n")
    matched_new_file.close()

    # Write SQL inserts for the matched keywords and datasets
    # Insert into the dataset_gcmd_keyword table the dataset ID and the GCMD keyword ID
    # GCMD keyword ID is equivalent to the position in the original file (index + 1 in the array)

    sql_insert_file = open("dataset_gcmd_insert.sql", "w")
    sql_insert_file.write("INSERT INTO dataset_gcmd_science_keyword (dataset_id, gcmd_science_keyword_id) VALUES\n")
    for matched_new_keyword in matched_new_keywords:
        gcmd_keyword_id = new_keywords.index(matched_new_keyword.gcmd_keyword) + 1
        sql_insert_file.write("({0}, {1}),\n".format(matched_new_keyword.dataset_id, gcmd_keyword_id))

    sql_insert_file.seek(-2, os.SEEK_END)
    sql_insert_file.truncate()
    sql_insert_file.write(";\n")
    sql_insert_file.close()


if __name__ == '__main__':
    main()

