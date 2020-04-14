
REPORT zrest_tables_archive.

INCLUDE zrest_tables_archive_top.
INCLUDE zrest_tables_archive_sel.
INCLUDE zrest_tables_archive_f01.

START-OF-SELECTION.
  PERFORM extract_data.
  PERFORM disp_output.
