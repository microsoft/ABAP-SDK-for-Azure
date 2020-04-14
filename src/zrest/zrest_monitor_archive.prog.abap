
report zrest_monitor_archive line-size 220.

include zrest_monitor_archive_top.
include zrest_monitor_archive_sel.
include zrest_monitor_archive_f01.

start-of-selection.

  perform extract_data.
  perform archive_data.
  perform handle_orphans.

