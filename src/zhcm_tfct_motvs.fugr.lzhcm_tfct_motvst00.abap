*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHCM_TFCT_MOTVS.................................*
DATA:  BEGIN OF STATUS_ZHCM_TFCT_MOTVS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHCM_TFCT_MOTVS               .
CONTROLS: TCTRL_ZHCM_TFCT_MOTVS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHCM_TFCT_MOTVS               .
TABLES: *ZHCM_TFCT_MOTVST              .
TABLES: ZHCM_TFCT_MOTVS                .
TABLES: ZHCM_TFCT_MOTVST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
