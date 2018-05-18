*&---------------------------------------------------------------------*
*& Report  ZTELEM_ZREST_SCHEDULER_BATCH
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

************************************************************************
*                                                                      *
*----------------------------------------------------------------------*
* Program Title: ZREST_SCHEDULER_BATCH                          *
* Created by   : Madhavi Yalala                      Date: Feb' 2018   *
*                                                                      *
* Description  : Purpose of this program is to process messages from   *
*              ZREST_MONITOR using ZREST_SCHEDULER program via multiple*
*              parallel jobs based on user input on selection screen   *
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
*Feb' 2018  | V-MYAL   | 3019845| MP2K905030 | Initial development     *
*03/09/2018 | V-MYAL   | 3019845| MP2K905176 | Fix msg count in job log*
*05/17/2018 | V-MYAL   | 3454161| MP2K905442 | More enhancements       *
*----------------------------------------------------------------------*

REPORT ZREST_SCHEDULER_BATCH.

***TOP Include for DATA
INCLUDE ZREST_SCHED_BATCH_TOP.

***Include for SELECTION-SCREEN
INCLUDE ZREST_SCHED_BATCH_SEL.

***Include for Sub routines
INCLUDE ZREST_SCHED_BATCH_F01.


***--------------------------------------------------------------------*
***Main Processing

START-OF-SELECTION.

  CLEAR gv_batch_jobcnt.
  CLEAR gv_rec_cnt.
  PERFORM get_data CHANGING gv_batch_jobcnt
                            gv_rec_cnt
                            gv_rc.

  IF NOT gv_rc IS INITIAL.
    MESSAGE i368(00) WITH text-003.
    EXIT.
  ENDIF.

**Use number of records selected and the batch size from selection screen to calcualte the
***number of batch jobs needed
***if the job count is calculated as 1 and the total number records selected > 30 use the
***default batch job count from selection. If total number of records selected is <= 30,
***use just 1 batch job

***Begin of MP2K905442
***if the number of calculated batch jobs is more than the maximum number of jobs entered on
***selection screen, use the number of jobs from selection screen

*  IF gv_batch_jobcnt = 1.
*    IF gv_rec_cnt <= 30.
*      gv_batch_jobcnt = 1.
*    ELSE.
*      gv_batch_jobcnt = p_jobs.
*    ENDIF.
*  ENDIF.

  IF gv_batch_jobcnt = 1.
    IF gv_rec_cnt <= 30.
      gv_batch_jobcnt = 1.
    ELSE.
      gv_batch_jobcnt = p_jobs.  "Minimum number of jobs
    ENDIF.
  ELSEif gv_batch_jobcnt > 1.
    IF gv_batch_jobcnt > p_mjobs.
      gv_batch_jobcnt = p_mjobs. "Maximum number of jobs
    ENDIF.
  ENDIF.

***End of MP2K905225

***Split the selected data into all jobs
***Assign job numbers to the records
  CLEAR gv_index.
  LOOP AT gt_zmessageid INTO gs_zmessageid.

    gv_index = gv_index + 1.

    IF gv_index < gv_batch_jobcnt.
      gs_zmid_batch-batch_num = gv_index.
      gs_zmid_batch-zmessageid = gs_zmessageid.
      APPEND gs_zmid_batch TO gt_zmid_batch.
    ELSEIF gv_index = gv_batch_jobcnt.
      gs_zmid_batch-batch_num = gv_index.
      gs_zmid_batch-zmessageid = gs_zmessageid.
      APPEND gs_zmid_batch TO gt_zmid_batch.
      CLEAR gv_index.
    ENDIF.

  ENDLOOP.

***Start the process of actually creating the jobs using batch numbers assigned
*** inthe step above

  CLEAR gs_zmessageid.

  SORT gt_zmid_batch BY batch_num.

  CLEAR gv_msg_cnt.                                         "MP2K905176

  DO gv_batch_jobcnt TIMES.
    gv_batch_idx = gv_batch_idx + 1.

    CLEAR gs_zmid_batch.
    LOOP AT gt_zmid_batch INTO gs_zmid_batch
          WHERE batch_num = gv_batch_idx.

      gr_zmessageid-sign = 'I'.
      gr_zmessageid-option = 'EQ'.
      gr_zmessageid-low = gs_zmid_batch-zmessageid.
      APPEND gr_zmessageid TO gtr_zmessageid.

      ADD 1 TO gv_msg_cnt.                                  "MP2K905176

      AT END OF batch_num.
        PERFORM create_job.

        CLEAR gtr_zmessageid.
        CLEAR gv_msg_cnt.
      ENDAT.

    ENDLOOP.

  ENDDO.
