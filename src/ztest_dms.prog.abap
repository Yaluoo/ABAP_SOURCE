*&---------------------------------------------------------------------*
*& Report ZTEST_DMS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_dms.


TYPES: BEGIN OF ty_tab,
       raw(255) TYPE x,
       END OF ty_tab.

DATA: lw_tab TYPE ty_tab,
      lt_tab TYPE STANDARD TABLE OF ty_tab.

DATA: lv_file TYPE string VALUE 'C:\Users\mayangchun\Desktop\SAP文档管理方案V1.0.pdf'.
DATA: lv_xstring TYPE xstring.
DATA: l_length TYPE i.


*1.上传文档至DMS
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename            = lv_file
    filetype            = 'BIN'
*    has_field_separator = 'X'
  IMPORTING
    FILELENGTH          = l_length
    HEADER              = lv_xstring
  TABLES
    data_tab            = lt_tab
  EXCEPTIONS
    file_open_error     = 1
    file_read_error     = 2
    OTHERS              = 3.

IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
  EXPORTING
    input_length       = l_length
*   FIRST_LINE         = 0
*   LAST_LINE          = 0
 IMPORTING
   buffer             = lv_xstring
  TABLES
    binary_tab         = lt_tab
 EXCEPTIONS
   failed             = 1
   OTHERS             = 2
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
*

DATA l_arc_doc_id TYPE SAEARDOID.
CALL FUNCTION 'ZZF_DMS_XSTRING_UPLOAD'
  EXPORTING
    l_xstring                  = lv_xstring
    filename                   = 'SAP文档管理方案.PDF'
*   DOC_TYPE                    =
    ar_object                  = 'ZDMS_XLSX'
    object_id                  = '100000002'
    sap_object                 = 'ZDMS'
    file_descr                 = 'TEST'
   importing
     arc_doc_id                 = l_arc_doc_id
 EXCEPTIONS
   mimetype_not_find          = 1
   xstring_create_error       = 2
   file_insert_error          = 3
   OTHERS                     = 4
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

**2.读取xstring
CALL FUNCTION 'ZZF_DMS_READ'
  EXPORTING
    object_id            = '100000002'
    sap_object           =  'ZDMS'
  IMPORTING
    XSTRING              = lv_xstring
  EXCEPTIONS
    HTTP_GET_ERROR       = 1
    OTHERS               = 2
           .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

**3.读取并下载
*CALL FUNCTION 'ZZF_DMS_READ_DOWNLOAD'
*  EXPORTING
*    object_id            = '100000002'
*    sap_object           = 'ZDMS'
*  EXCEPTIONS
*    HTTP_GET_ERROR       = 1
*    OTHERS               = 2
*          .
*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.

**4.删除
*CALL FUNCTION 'ZZF_DMS_DELETE'
*  EXPORTING
*    object_id               = '100000001'
*    sap_object              = 'ZDMS'
*  EXCEPTIONS
*    HTTP_DELETE_ERROR       = 1
*    OTHERS                  = 2
*          .
*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.

*5.获取附件列表
   DATA:lt_atta TYPE zztat001.
   CALL FUNCTION 'ZZF_DMS_GET_LIST'
     EXPORTING
       object_id           = '100000001'
       sap_object          = 'ZDMS'
    IMPORTING
      et_atta             = lt_atta
    EXCEPTIONS
      url_get_error       = 1
      OTHERS              = 2
             .
   IF sy-subrc <> 0.
* Implement suitable error handling here
   ENDIF.
