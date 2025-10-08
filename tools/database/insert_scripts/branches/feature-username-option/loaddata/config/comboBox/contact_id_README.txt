This is a readme for instructions on which individual contact_id yaml files should point to in case of link failure. the higher level item is the original file in which the lower level items point to.

The links for the contact_id yaml files should be as the following:

dts_status_id:
- ingest_status_id
- load_status_id
- approve_status_id

codiac_contact_id_active:
- internal_contact_id_codiac
- contact_id_codiac.yml
- contact_id_codiac_2.yml

codiac_contact_id_all:
- point_of_contact_id_codiac

dts_contact_id_active:
- ingest_contact_id_dts
- load_contact_id_dts
- approve_contact_id_dts
- author_id_dts
- internal_contact_id_dts

dts_contact_id_all:
- source_contact_id_dts
