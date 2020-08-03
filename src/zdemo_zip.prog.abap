*&---------------------------------------------------------------------*
*& Report ZOSWIN_ZIP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_zip.
* 思路：
*   1. 需要下载的文件在服务器哪个位置，如何获取整个资源目录
*   2. 如何先在系统生成一个ZIP空包文件 -X
*   3. 遍历资源目录文件，包括文件夹结构添加到ZIP文件包

*al11-查看
*cg3z-上传
*cg3y-下载
*本地直接到bsp
*提供下载功能zip
*提供上传服务器目录并解压

* 1.错误处理
* 2.获取文件列表不完全，文件夹是否都下载

*1.功能:
*         1. 打包指定路径资源 ： 打包资源路径，zip包名
*         2. 是否在服务器生成新的zip包

*         下载，生成新包，删除

*&*********zip 处理全局变量*********&*
 DATA: gt_binary_tab TYPE TABLE OF x255.
 DATA: gv_file_length TYPE i .

*&*********输入参数*********&*
 DATA: lv_source_path TYPE dirname_al11.   "打包资源路径
 DATA: lv_zip_name TYPE string.            "zip包名
 DATA: lv_zip_path TYPE rcgiedial-iefile . "资源存放路径


SELECTION-SCREEN BEGIN OF BLOCK bk1  WITH FRAME TITLE TEXT-001.
PARAMETERS: r_rb1 RADIOBUTTON GROUP gr1 USER-COMMAND sel DEFAULT 'X' ,   "自动打包下载
*            r_rb4 RADIOBUTTON GROUP gr1 ,                                "本地上传解压
            r_rb2 RADIOBUTTON GROUP gr1 ,                                "数据上传
            r_rb3 RADIOBUTTON GROUP gr1  .                               "数据下载
SELECTION-SCREEN END OF BLOCK bk1 .

SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE TEXT-002.
 PARAMETERS: p_spath TYPE dirname_al11 LOWER CASE MODIF ID rb1.
 SELECTION-SCREEN SKIP 1.
 PARAMETERS: p_zname TYPE string LOWER CASE MODIF ID rb1.
 PARAMETERS: p_zpath  TYPE c LOWER CASE LENGTH 30 MODIF ID rb1.
 SELECTION-SCREEN SKIP 1.

 PARAMETERS: p_down TYPE c AS CHECKBOX DEFAULT 'X' USER-COMMAND sel MODIF ID rb1.    "本地下载
 PARAMETERS: p_up   TYPE c AS CHECKBOX DEFAULT 'X' USER-COMMAND sel MODIF ID rb1.   "打包上传到服务器
SELECTION-SCREEN END OF BLOCK bk2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

INITIALIZATION.
  p_spath = '/usr/sap/YES/D50/data'.
  p_zname = sy-datum && sy-uzeit && '.zip'.
  p_zpath = '/tmp'.
*  p_zpath = '/tmp/' && p_zname.

START-OF-SELECTION.
  lv_source_path = p_spath.
  lv_zip_name = p_zname.
  lv_zip_path = p_zpath.

  BREAK-POINT.
  IF r_rb1 EQ abap_true.
    REFRESH: gt_binary_tab.
    CLEAR gv_file_length.
    "打包文件
    PERFORM fill_zip TABLES gt_binary_tab
                       USING gv_file_length
                              lv_source_path . "待打包资源路径

    IF p_down EQ abap_true.
      "下载到本地
      PERFORM download_zip TABLES gt_binary_tab
                             USING gv_file_length.
    ENDIF.

    IF p_up EQ abap_true.
      "上传到服务器
      PERFORM upload_zip TABLES gt_binary_tab
                             USING lv_zip_name  "zip包名
                                    lv_zip_path. "资源存放路径
    ENDIF.

  ELSEIF r_rb2 EQ abap_true.

    CALL TRANSACTION 'CG3Z'.

  ELSEIF r_rb3 EQ abap_true.

    CALL TRANSACTION 'CG3Y'.
  ENDIF.


*&---------------------------------------------------------------------*
*& Form create_empty_zip
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ZIPNAME
*&      --> LV_PATH
*&---------------------------------------------------------------------*
FORM create_empty_zip  USING pv_zip_name
                              pv_zip_path .

   DATA: i_file TYPE rcgiedial-iefile,
         l_subrc LIKE sy-subrc.

   "包名
   IF pv_zip_name IS INITIAL.
      pv_zip_name = sy-datum && sy-uzeit && '.zip'.
   ENDIF.

   "服务器存放路径
   IF pv_zip_path IS INITIAL.
     pv_zip_path = '/tmp/' && pv_zip_name.
   ELSE.
     pv_zip_path = pv_zip_path && '/' && pv_zip_name.
   ENDIF.

   i_file = pv_zip_path.
* open dataset for writing-Begin Correction
   CLEAR l_subrc.
   CATCH SYSTEM-EXCEPTIONS open_dataset_no_authority = 1
                           dataset_too_many_files = 2
                           OTHERS = 4.
     "二进制模式打开文件
     OPEN DATASET i_file FOR OUTPUT IN BINARY MODE.
     l_subrc = sy-subrc.
   ENDCATCH.
   IF NOT sy-subrc IS INITIAL OR
      NOT  l_subrc IS INITIAL.
     RAISE open_failed.
   ENDIF.

* close the dataset
  CATCH SYSTEM-EXCEPTIONS dataset_cant_close = 1
                          OTHERS = 4.
    "关闭应用服务器文件
    CLOSE DATASET i_file.
  ENDCATCH.

  IF NOT sy-subrc IS INITIAL.
    RAISE close_failed.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fill_zip
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ZIP_PATH
*&      --> LV_SOURCE_PATH
*&---------------------------------------------------------------------*
FORM fill_zip  TABLES pt_binary_tab LIKE gt_binary_tab
                      USING pv_file_length pv_source_path.

  TYPES: BEGIN OF ts_file,
         dirname     TYPE dirname_al11,   " name of directory
         name        TYPE filename_al11,  " name of entry
         type(10)    TYPE c,              " type of entry.
         len(8)      TYPE p,              " length in bytes.
         owner       TYPE fileowner_al11, " owner of the entry.
         mtime(6)    TYPE p,              " last mod.date, sec since 1970
         mode(9)     TYPE c,              " like "rwx-r-x--x": prot. mode
         useable(1)  TYPE c,
         subrc(4)    TYPE c,
         errno(3)    TYPE c,
         errmsg(40)  TYPE c,
         mod_date    TYPE d,
         mod_time(8) TYPE c,              " hh:mm:ss
         seen(1)     TYPE c,
         changed(1)  TYPE c,
         status(1)   TYPE c,
       END OF ts_file.

  DATA: file      TYPE ts_file,
        file_list TYPE STANDARD TABLE OF ts_file WITH HEADER LINE WITH NON-UNIQUE SORTED KEY k1 COMPONENTS name,
        file_list_t TYPE STANDARD TABLE OF ts_file WITH HEADER LINE WITH NON-UNIQUE SORTED KEY k1 COMPONENTS name.

  DATA: lv_dir_path TYPE dirname_al11,
        lv_file_name TYPE string,
        lv_zip_path(100) TYPE c.

  DATA: lo_zip TYPE REF TO cl_abap_zip,
        lv_zip_content TYPE xstring,
        lv_content TYPE xstring.

  FIELD-SYMBOLS: <f_file> TYPE ANY TABLE.

  IF pv_source_path IS INITIAL.
    lv_dir_path = '/usr/sap/YES/D50/data'.
  ELSE.
    lv_dir_path = pv_source_path.
  ENDIF.

  CALL FUNCTION 'ZRFC_GET_DIRECTORY'
    EXPORTING
      i_dir_path       = lv_dir_path
    TABLES
      t_file           = file_list.

  DELETE file_list WHERE name+0(1) = '.'.
  SORT file_list BY dirname.

  BREAK: xuwp.

  CHECK file_list[] IS NOT INITIAL.
  CREATE OBJECT lo_zip.

  LOOP AT file_list INTO DATA(ls_file_list).
   DATA(l_file_path) = ls_file_list-dirname && '/' && ls_file_list-name.
   IF ls_file_list-useable EQ abap_true.
      CLEAR lv_content .
      OPEN DATASET l_file_path FOR INPUT IN BINARY MODE.
      READ DATASET l_file_path INTO lv_content .
      CLOSE DATASET l_file_path.

      CLEAR lv_file_name.
*      lv_file_name = l_file_path+1.
      BREAK-POINT.
      lv_file_name = l_file_path+39(*).
      lo_zip->add( name = lv_file_name  content = lv_content ).
   ELSE.
       lv_dir_path = l_file_path.
       CALL FUNCTION 'ZRFC_GET_DIRECTORY'
        EXPORTING
          i_dir_path       = lv_dir_path
        TABLES
          t_file           = file_list_t.

       DELETE file_list_t WHERE name+0(1) = '.'.
       SORT file_list_t BY dirname.
       APPEND LINES OF file_list_t TO file_list.

       CLEAR lv_dir_path.
       REFRESH:file_list_t.
   ENDIF.
  ENDLOOP.

  CLEAR lv_zip_content.
  lv_zip_content = lo_zip->save( ).

*Conver the xstring content to binary
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
   EXPORTING
     buffer = lv_zip_content
   IMPORTING
     output_length = pv_file_length
   TABLES
     binary_tab = pt_binary_tab.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form modify_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM modify_screen .

  LOOP AT SCREEN.
   IF r_rb1 NE abap_true AND  screen-group1 EQ 'RB1'.
    screen-active  = '0'.
    MODIFY SCREEN.
   ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form download_zip
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_BINARY_TAB
*&      --> GV_FILE_LENGTH
*&---------------------------------------------------------------------*
FORM download_zip  TABLES  pt_binary_tab LIKE gt_binary_tab
                      USING  pv_file_length.

  DATA: lv_filename TYPE string,
        lv_path TYPE string,
        lv_full_path TYPE string,
        lv_filelength TYPE i.

  cl_gui_frontend_services=>file_save_dialog(
   EXPORTING
     window_title         = 'SELECT THE LOCATION TO SAVE THE FILE'
     file_filter          = '(*.ZIP)|*.ZIP|'
     default_file_name    = lv_zip_name       "默认文件名
   CHANGING
     filename             = lv_filename      "保存文件名
     path                 = lv_path          "相对路径
     fullpath             = lv_full_path     "绝对路径
  ).

  cl_gui_frontend_services=>gui_download(
    EXPORTING
      bin_filesize              = pv_file_length
      filename                  = lv_full_path     "下载路径
      filetype                  = 'BIN'
    IMPORTING
      filelength                = lv_filelength
    CHANGING
      data_tab                  = pt_binary_tab[]
   ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form upload_zip
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_BINARY_TAB
*&      --> LV_ZIP_PATH
*&---------------------------------------------------------------------*
FORM upload_zip  TABLES pt_binary_tab LIKE gt_binary_tab
                    USING pv_zip_name pv_zip_path.

   "生成空zip包
    PERFORM create_empty_zip USING pv_zip_name "zip包名
                                    pv_zip_path.  "资源存放路径

    "数据传递
    PERFORM transfer_data TABLES pt_binary_tab
                             USING pv_zip_path.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form transfer_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_BINARY_TAB
*&      --> PV_ZIP_PATH
*&---------------------------------------------------------------------*
FORM transfer_data  TABLES pt_binary_tab LIKE gt_binary_tab
                      USING pv_zip_path.

   "将打包内容添加到新生成的zip包
   OPEN DATASET pv_zip_path FOR OUTPUT IN BINARY MODE.
   LOOP AT pt_binary_tab INTO DATA(ls_banary_tab).
     TRANSFER ls_banary_tab TO pv_zip_path.
   ENDLOOP.

   CLOSE DATASET pv_zip_path.

ENDFORM.
