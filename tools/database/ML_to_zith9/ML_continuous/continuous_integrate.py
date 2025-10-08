#************************************************************************
# continuous_integrate.py
#
# Script to keep Zinc updated with newly created metadata in Master List
# 
# Requires Python 2.7 and Python MySQL Connector. 
#   As of writing, Barolo has both.
#
# Please read readme.txt in the CSV_files directory for info on input files 
#
# Possible change: use row_revise_time or row_create_time of dataset
#                  in zith to compile list of archive_idents of datasets 
#                  revised between each date-time this script is run in 
#                  check_new_datasets for better runtime and no need for
#                  checked_datasets.csv file. 
#                  script needs to keep track of when it was run last.
#
# python continuous_integrate.py
# 
# The flow is:
#   login to dmg_merged_ml and zith9
#   create_new_sites
#   create_new_events
#   make_new_datasets
#     make_dataset
#       calculate_archive_ident
#         make next guess
#     link new dataset to projects
#     create xlinks for dataset
#   check_new_datasets
#     sync_dataset
#       link platforms, categories, sites, events to dataset
#   
# --Hee Su Chang
#   May 2019
#************************************************************************

import MySQLdb
from helper.login import login
import re
import subprocess
import sys
import time

ML_DB_NAME = 'ml_test_c'
ZITH_DB_NAME = 'zith9_test_c'
CHECKED_DATASETS = './CSV_files/checked_datasets.csv'
ONGOING_PROJECTS = './CSV_files/ongoing_projects.csv'
PLAT_IN = './CSV_files/class-platform.csv'
CAT_IN = './CSV_files/class-category.csv'
EVENT_IN = './CSV_files/class-event.csv'
SITE_IN = './CSV_files/class-site.csv'
LOGS_DIR = './logs/'


class ML_Continuous_Integrator():
    # establishes connections with DBs
    def __init__(self):
        self.contact = 149   # Don
        self.db_ml = login(ML_DB_NAME)
        if self.db_ml is None:
            sys.exit(1)
        self.cursor_ml = self.db_ml.cursor(MySQLdb.cursors.DictCursor)

        self.db_zith = login(ZITH_DB_NAME)
        if self.db_zith is None:
            sys.exit(1)
        self.cursor_zith = self.db_zith.cursor(MySQLdb.cursors.DictCursor)

    def release(self):
        self.db_zith.close()
        self.db_ml.close()

    # find all ML.xx datasets and call make_dataset
    def make_new_datasets(self):
        self.cursor_ml.execute("SELECT dataset_id FROM dataset WHERE dataset_id LIKE 'ML.%';")
        for dataset in self.cursor_ml.fetchall():
            self.make_dataset(dataset['dataset_id'])

    # recreate all ML sites created since last integration into zinc and add to class-site.csv
    def create_new_sites(self):
        self.cursor_ml.execute("SELECT class_id, name FROM classification WHERE type_id=3;")
        for site in self.cursor_ml.fetchall():
            with open(SITE_IN) as search:
                for line in search:
                    if line.split('|')[0] == site['name']:
                        break
                else:
                    self.cursor_zith.execute("INSERT INTO site (name, row_revise_contact_id) VALUES (%s, %s)",(site['name'], self.contact,))
                    with open(SITE_IN, 'a') as f:
                        f.write(site['name']+'|'+site['name']+'\n')
                    with open(LOGS_DIR+time.strftime("%Y-%m-%d")+'_report_new_sites.log', 'a') as f:
                        f.write(site['name']+'\n')

    # recreate all ML events created since last integration into zinc and add to class-event.csv
    def create_new_events(self):
        self.cursor_ml.execute("SELECT class_id, name FROM classification WHERE type_id=2;")
        for event in self.cursor_ml.fetchall():
            with open(EVENT_IN) as search:
                for line in search:
                    if line.split('|')[0] == event['name']:
                        break
                else:
                    self.cursor_zith.execute("INSERT INTO event (name, row_revise_contact_id) VALUES (%s, %s)",(event['name'], self.contact,))
                    with open(EVENT_IN, 'a') as f:
                        f.write(event['name']+'|'+event['name']+'\n')
                    with open(LOGS_DIR+time.strftime("%Y-%m-%d")+'_report_new_events.log', 'a') as f:
                        f.write(event['name']+'\n')

    # check for new datasets with archive_idents in ML and call sync_dataset for those new datasets
    def check_new_datasets(self):

        self.cursor_ml.execute("SELECT dataset_id, name FROM dataset WHERE dataset_id NOT LIKE '%.fc%' AND dataset_id NOT LIKE 'ML.%';")

        for dataset in self.cursor_ml.fetchall():
            archive_ident = dataset['dataset_id']
            title = dataset['name']
            with open(CHECKED_DATASETS) as search:
                for line in search:
                    line = line.replace('\n','')
                    if archive_ident == line:
                        break
                else:
                    self.cursor_zith.execute("SELECT id FROM dataset WHERE archive_ident=%s;",(archive_ident,))
                    dataset = self.cursor_zith.fetchone()
                    if dataset is not None:
                        self.sync_dataset(archive_ident, dataset['id'], title)
    
    # write error logs
    def write_log(self, log_name, error, cmd):
        with open(LOGS_DIR+time.strftime("%Y-%m-%d")+log_name+'.log', 'a') as log:
            log.write('QUERY: '+cmd+'\n')
            log.write('ERROR: '+str(error)+'\n')

    # update zinc dataset with metadata from ML
    def sync_dataset(self, archive_ident, did_zith, title):
        self.cursor_ml.execute("SELECT c.name FROM classification AS c JOIN dataset_classification AS dc ON c.class_id=dc.class_id WHERE dc.dataset_id=%s AND c.type_id=4;",(archive_ident,))
        for platform in self.cursor_ml.fetchall():
            with open(PLAT_IN) as platforms:
                for line in platforms:
                    if re.search('^'+platform['name']+'\|',line):
                        plat_zith = line.split('|')[1].replace('\n','')
                        self.cursor_zith.execute("SELECT * FROM dataset_platform WHERE dataset_id=%s AND platform_id=(SELECT id FROM platform WHERE name=%s LIMIT 1);",(did_zith, plat_zith))
                        if self.cursor_zith.fetchone() is None:
                            try:
                                self.cursor_zith.execute("INSERT INTO dataset_platform (dataset_id, platform_id) VALUES (%s, (SELECT id FROM platform WHERE name=%s LIMIT 1));",(did_zith, plat_zith,))
                            except (MySQLdb.Error, MySQLdb.Warning) as e:
                                if not e[0] == 1062: # 1062 will come up when dataset already has this platform in zinc so ignore
                                    self.write_log('_err_sync_platform', e, self.cursor_zith._last_executed)

        self.cursor_ml.execute("SELECT c.name FROM classification AS c JOIN dataset_classification AS dc ON c.class_id=dc.class_id WHERE dc.dataset_id=%s AND c.type_id=1;",(archive_ident,))
        for category in self.cursor_ml.fetchall():
            with open(CAT_IN) as categories:
                for line in categories:
                    if re.search('^'+category['name']+'\|',line):
                        cat_zith = line.split('|')[1].replace('\n','')
                        self.cursor_zith.execute("SELECT * FROM dataset_category WHERE dataset_id=%s AND category_id=(SELECT id FROM category WHERE name=%s LIMIT 1);",(did_zith, cat_zith))
                        if self.cursor_zith.fetchone() is None:
                            try:
                                self.cursor_zith.execute("INSERT INTO dataset_category (dataset_id, category_id) VALUES (%s, (SELECT id FROM category WHERE name=%s LIMIT 1));",(did_zith, cat_zith,))
                            except (MySQLdb.Error, MySQLdb.Warning) as e:
                                if not e[0] == 1062: # 1062 will come up when dataset already has this category in zinc so ignore
                                    self.write_log('_err_sync_category', e, self.cursor_zith._last_executed)

        self.cursor_ml.execute("SELECT c.name FROM classification AS c JOIN dataset_classification AS dc ON c.class_id=dc.class_id WHERE dc.dataset_id=%s AND c.type_id=2;",(archive_ident,))
        for event in self.cursor_ml.fetchall():
            with open(EVENT_IN) as events:
                for line in events:
                    if re.search('^'+event['name']+'\|',line):
                        event_zith = line.split('|')[1].replace('\n','')
                        self.cursor_zith.execute("SELECT * FROM dataset_event WHERE dataset_id=%s AND event_id=%s;",(did_zith, event_zith))
                        if self.cursor_zith.fetchone() is None:
                            try:
                                self.cursor_zith.execute("INSERT INTO dataset_event (dataset_id, event_id) VALUES (%s, (SELECT id FROM event WHERE name=%s LIMIT 1));",(did_zith, event_zith,))
                            except (MySQLdb.Error, MySQLdb.Warning) as e:
                                if not e[0] == 1062: # 1062 will come up when dataset already has this event in zinc so ignore    
                                    self.write_log('_err_sync_event', e, self.cursor_zith._last_executed)

        self.cursor_ml.execute("SELECT c.name FROM classification AS c JOIN dataset_classification AS dc ON c.class_id=dc.class_id WHERE dc.dataset_id=%s AND c.type_id=3;",(archive_ident,))
        for site in self.cursor_ml.fetchall():
            with open(SITE_IN) as sites:
                for line in sites:
                    if re.search('^'+site['name']+'\|',line):
                        site_zith = line.split('|')[1].replace('\n','')
                        self.cursor_zith.execute("SELECT * FROM dataset_site WHERE dataset_id=%s AND site_id=%s",(did_zith, site_zith))
                        if self.cursor_zith.fetchone() is None:
                            try:
                                self.cursor_zith.execute("INSERT INTO dataset_site (dataset_id, site_id) VALUES (%s, (SELECT id FROM site WHERE name=%s LIMIT 1))",(did_zith, site_zith,))
                            except (MySQLdb.Error, MySQLdb.Warning) as e:
                                if not e[0] == 1062: # 1062 will come up when dataset already has this site in zinc so ignore
                                    self.write_log('_err_sync_site', e, self.cursor_zith._last_executed)

        self.db_zith.commit()
   
        # if dataset is from an ongoing project, don't add to checked_datasets list so script updates next time as well
        self.cursor_ml.execute("SELECT project_id FROM dataset_project WHERE dataset_id=%s;",(archive_ident,))
        for project in self.cursor_ml.fetchall():
            with open(ONGOING_PROJECTS) as projects:
                for line in projects:
                    if re.search('\b'+project['project_id']+'\b', line.replace('\n','')):
                        return

        with open(CHECKED_DATASETS, 'a') as f:
            f.write(str(archive_ident)+'\n')
    
    # assign ML.xx dataset an archive_ident and create in zinc with metadata
    def make_dataset(self, dataset_id_ml):
        self.cursor_ml.execute("SELECT name, url, doc_url, date_updated, preliminary_flag, dp.project_id, dp.date_posted, dp.in_progress_flag, dp.hide_flag FROM dataset JOIN dataset_project AS dp ON dataset.dataset_id=dp.dataset_id where dataset.dataset_id=%s;",(dataset_id_ml,))

        dataset_ml = self.cursor_ml.fetchone()

        name = dataset_ml['name']
        url = dataset_ml['url']
        doc_url = dataset_ml['doc_url']
        date_updated = dataset_ml['date_updated']
        prelim = dataset_ml['preliminary_flag']
        project_name = dataset_ml['project_id']
        date_posted = dataset_ml['date_posted']
        progress = dataset_ml['in_progress_flag']
        hide_flag = dataset_ml['hide_flag']

        add_note = 'This dataset links to supplemental information for the project.'
        no_link_txt = 'This dataset has not yet been received.'
        hidden_txt = 'Since the project ended over 3 years ago, this dataset has been hidden.'
        ml_hidden_txt = 'This link in the Master List was hidden.'

        if project_name is None:
            return

        new_archive_ident = self.calculate_archive_ident(dataset_ml['project_id'])

        self.cursor_zith.execute("SELECT begin_date, end_date, minimum_latitude, maximum_latitude, minimum_longitude, maximum_longitude, internal_contact_id from project where name=%s;",(dataset_ml['project_id'],))
        proj_meta = self.cursor_zith.fetchone()

        proj_begin = proj_meta['begin_date']
        proj_end = proj_meta['end_date']
        proj_minlat = proj_meta['minimum_latitude']
        proj_maxlat = proj_meta['maximum_latitude']
        proj_minlon = proj_meta['minimum_longitude']
        proj_maxlon = proj_meta['maximum_longitude']
        proj_internal_contact = proj_meta['internal_contact_id']

        if prelim == 1:
            prelim = 'preliminary'
        else:
            prelim = None

        if progress == 1:
            progress = 'onGoing'
        else:
            progress = 'planned'

        if url == doc_url:
            doc_url = ''

        if url == '' and doc_url == '':
            add_note = no_link_txt
            progress = 'planned'

        else:
            prelim = 'final'
            progress = 'completed'

        if dataset_ml['hide_flag'] == 1:
            vis_flag = 0
            add_note = ml_hidden_txt + ' ' + add_note
        else:
            vis_flag = 1

        dataset_query = "INSERT INTO dataset (archive_ident, title, archive_note, begin_date, end_date, minimum_latitude, maximum_latitude, minimum_longitude, maximum_longitude, internal_contact_id, visible, quality, progress, row_revise_contact_id) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s);"
        try:
            self.cursor_zith.execute(dataset_query,(new_archive_ident, name, add_note, proj_begin, proj_end, proj_minlat, proj_maxlat, proj_minlon, proj_maxlon, proj_internal_contact, str(vis_flag), prelim, progress, str(self.contact),))
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            self.write_log('_err_make_dataset', e, self.cursor_zith._last_executed)
            return
        dataset_id_zith = self.cursor_zith.lastrowid

        self.cursor_ml.execute("SELECT project_id FROM dataset_project WHERE dataset_id=%s;",(dataset_id_ml,))
        for project in self.cursor_ml.fetchall():
            self.cursor_zith.execute("SELECT id FROM project WHERE name=%s;",(project['project_id'],))
            project_id_zith = self.cursor_zith.fetchone()['id']
            self.cursor_zith.execute("INSERT INTO dataset_project (dataset_id, project_id) VALUES (%s, %s);",(dataset_id_zith, project_id_zith,))

        if date_updated is not None and not date_updated == 'NULL':
            self.cursor_zith.execute("UPDATE dataset SET row_revise_time=%s WHERE id=%s;",(date_updated, dataset_id_zith,))

        if doc_url is not None:
            self.link_url(doc_url, dataset_id_zith)

        if url is not None:
            self.link_url(url, dataset_id_zith)

        self.cursor_ml.execute("UPDATE dataset SET dataset_id=%s WHERE dataset_id=%s;",(new_archive_ident, dataset_id_ml,))

        self.db_ml.commit()
        self.db_zith.commit()

        with open(LOGS_DIR+time.strftime("%Y-%m-%d")+'_report_new_datasets.log', 'a') as f:
            f.write(dataset_id_ml+'->'+new_archive_ident+'\n')

    def link_url(self, href, dataset_id_zith):
        xlink_id = ''
        self.cursor_zith.execute("SELECT id FROM xlink WHERE href=%s",(href,))

        if self.cursor_zith.rowcount == 1:
            xlink_id = self.cursor_zith.fetchone()['id']
        else:
            self.cursor_zith.execute("INSERT INTO xlink (href, row_revise_contact_id) VALUES (%s, %s);",(href, self.contact,))
            xlink_id = self.cursor_zith.lastrowid

        try:
            self.cursor_zith.execute("INSERT INTO dataset_xlink (dataset_id, xlink_id) VALUES (%s, %s);",(dataset_id_zith, xlink_id,))
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            if not e[0] == 1062: # 1062 will come up when dataset already has this xlink in zinc so ignore
                self.write_log('_err_make_xlink', e, self.cursor_zith._last_executed)


    def calculate_archive_ident(self, project_id):
        self.cursor_zith.execute("SELECT dataset_id_prefix AS prefix FROM dataset_prefix_project WHERE project_name=%s;",(project_id,))
        prefix_lookup = self.cursor_zith.fetchone()
        prefix = None
        if prefix_lookup:
            prefix = str(prefix_lookup['prefix'])

	if prefix is None:
            self.cursor_zith.execute("SELECT id from project WHERE name=%s;",(project_id,))
            prefix = str(self.cursor_zith.fetchone()['id'])

        next_ident = self.make_next_guess(prefix)
        return next_ident

    def make_next_guess(self, prefix):
        if not '.' in prefix:
            prefix += '.'

        notpfx = prefix + 'fc.%'

        if re.search('_$',prefix) is None and re.search('%$',prefix) is None:
            prefix+='%'

        self.cursor_zith.execute("SELECT MAX(substring_index(archive_ident,'.',-1)+0)+1 AS next FROM dataset WHERE archive_ident LIKE %s AND archive_ident NOT LIKE %s;",(prefix, notpfx,))
        next_sfx = self.cursor_zith.fetchone()['next']
        if next_sfx is None or next_sfx == 'NULL':
            next_sfx = 1
        elif re.search('^[0-9]+(\.0)?$', str(next_sfx)) is None:
            next_sfx = 1
        next_sfx = int(next_sfx)
        if re.search('^.*[.]', prefix):
            prefix = re.split('[.]', prefix)[0] + '.'

        guess = '{}{}'.format(prefix,'{:03d}'.format(next_sfx)[:3])
        self.cursor_zith.execute("SELECT 1 FROM dataset WHERE archive_ident=%s LIMIT 1",(guess,))
        self.cursor_zith.fetchone()

        if self.cursor_zith.rowcount == 1:
            next_length = len(str(next_sfx))-1
            for i in range(next_length):
                prefix += '_'

            guess = self.make_next_guess(prefix)

        return guess

if __name__ == '__main__':
    integrator = ML_Continuous_Integrator()
    print('Creating new sites')
    integrator.create_new_sites()
    print('Creating new events')
    integrator.create_new_events()
    print('Making ML.xx datasets into actual zinc datasets')
    integrator.make_new_datasets()
    print('Check new datasets for updates and sync metadata')
    integrator.check_new_datasets()
    integrator.release()
