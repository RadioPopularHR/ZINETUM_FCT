*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHCM_TFCT_MODA..................................*
DATA:  BEGIN OF STATUS_ZHCM_TFCT_MODA                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHCM_TFCT_MODA                .
CONTROLS: TCTRL_ZHCM_TFCT_MODA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHCM_TFCT_MODA                .
TABLES: *ZHCM_TFCT_MODAT               .
TABLES: ZHCM_TFCT_MODA                 .
TABLES: ZHCM_TFCT_MODAT                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
