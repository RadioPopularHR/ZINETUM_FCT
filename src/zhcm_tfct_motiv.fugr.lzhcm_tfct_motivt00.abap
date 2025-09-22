*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHCM_TFCT_MOTIV.................................*
DATA:  BEGIN OF STATUS_ZHCM_TFCT_MOTIV               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHCM_TFCT_MOTIV               .
CONTROLS: TCTRL_ZHCM_TFCT_MOTIV
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHCM_TFCT_MOTIV               .
TABLES: *ZHCM_TFCT_MOTIVT              .
TABLES: ZHCM_TFCT_MOTIV                .
TABLES: ZHCM_TFCT_MOTIVT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
