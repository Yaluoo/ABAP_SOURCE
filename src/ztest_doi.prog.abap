*&---------------------------------------------------------------------*
*& Report ZTEST_DOI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_doi.
TABLES: t001.
TYPE-POOLS: slis,vrm, sbdst, soi."引入必要的类型组
CONSTANTS document_name(30) VALUE 'test'."模板名字
CONSTANTS inplace VALUE 'X'."控制参数,在GUI中显示Excel
DATA: flag .
DATA:container TYPE REF TO cl_gui_custom_container,"容器实例
     control     TYPE REF TO i_oi_container_control,"控制器实例
     document TYPE REF TO i_oi_document_proxy,"文档操作对象
     spreadsheet TYPE REF TO i_oi_spreadsheet,"分隔符对象
     error    TYPE REF TO i_oi_error,"错误信息
     errors TYPE REF TO i_oi_error OCCURS 0 WITH HEADER LINE."错误信息 * spreadsheet interface structures for Excel data input
DATA: rangeitem TYPE soi_range_item.
DATA: ranges TYPE soi_range_list.
DATA: excel_input TYPE soi_generic_table.
DATA: excel_input_wa TYPE soi_generic_item.
DATA: initialized(1),
       retcode TYPE soi_ret_string.
 DATA: item_url(256)."存放模板的Url
 DATA  document_type(80)."文档类型
 DATA: excel(80) VALUE 'Excel.Sheet'.
 DATA: line_count TYPE i,
       column_count TYPE i.
  DATA: ok_code TYPE sy-ucomm,
        save_ok TYPE sy-ucomm.
CLASS c_oi_errors DEFINITION LOAD.

DATA: BEGIN OF itab OCCURS 0.
  INCLUDE STRUCTURE t001.
DATA: END OF itab.

SELECT-OPTIONS bukrs FOR t001-bukrs.

START-OF-SELECTION.

PERFORM getdata.

CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&    Form   getdata
*&---------------------------------------------------------------------*
*    text
*----------------------------------------------------------------------*
*   -->   p1        text *   <--   p2        text
*----------------------------------------------------------------------*
FORM getdata .
  SELECT * FROM t001 INTO TABLE itab WHERE bukrs IN bukrs.
ENDFORM.                    " getdata
*&---------------------------------------------------------------------*
*&    Module   STATUS_0100   OUTPUT
*&---------------------------------------------------------------------*
*    text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'SA1'.
  IF flag = space .
    PERFORM create_basic_objects USING ''   '' '' '' document_name.
    PERFORM output_to_excel.
 ENDIF.
ENDMODULE.                    "STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&    Module   USER_COMMAND_0100   INPUT
*&---------------------------------------------------------------------*
*    text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
   flag = 'X'.
save_ok = ok_code.
CLEAR ok_code.
CASE save_ok.
  WHEN 'CANCLE' .
IF NOT document IS INITIAL.
CALL METHOD document->close_document."关闭文档
FREE document.
ENDIF.
IF NOT control IS INITIAL.
CALL METHOD control->destroy_control.
FREE control.
ENDIF.
LEAVE PROGRAM.
WHEN 'BACK' .
IF NOT document IS INITIAL.
CALL METHOD document->close_document.
FREE document.
ENDIF.
IF NOT control IS INITIAL.
  CALL METHOD control->destroy_control.
  FREE control.
  ENDIF.
  SET SCREEN 0.   " quit the program "set screen 1000.
WHEN '&DATA_SAVE'.
    DATA: top TYPE i ,
          left TYPE i ,
          columns TYPE i ,
          rows TYPE i .

*    CALL METHOD spreadsheet->get_selection
*    EXPORTING
*      no_flush = ''
*    IMPORTING
*      top      = top
*      left     = left
*      rows     = rows
*      columns  = columns
*      error    = errors
*      retcode  = retcode.

*    REFRESH: ranges, excel_input .
*    APPEND 'cell1' TO ranges.
*
*    CALL METHOD spreadsheet->insert_range_dim
*    EXPORTING
*      name = 'cell1'
*      no_flush = 'X'
*      top = 3
*      left    = 1
*      rows    = 10
*      columns = 3
*    IMPORTING
*      error = errors.

*    CALL METHOD spreadsheet->set_selection
*    EXPORTING
*      top     = 3
*      left    = 1
*      rows    = 10
*      columns = 3.
*
*    CALL METHOD spreadsheet->insert_range
*    EXPORTING
*      name     = 'Test'
*      rows     = 1
*      columns  = 3
*      no_flush = ''
*    IMPORTING
*      error    = errors.
*    IF errors->has_failed = 'X'.
*      EXIT.
**      call method iref_error->raise_message
**        exporting
**          type = 'E'.
*    ENDIF.
CALL METHOD spreadsheet->get_ranges_names
  EXPORTING
    no_flush = ' '
    updating = -1
  IMPORTING
    error    = errors
    retcode  = retcode
    ranges   = ranges
    .

*GET_RANGES_NAMES
*    rangeitem-name = 'cell' .
*    rangeitem-columns = '3'.
*    rangeitem-rows = '3'.
*    APPEND rangeitem TO ranges .

*    CALL METHOD spreadsheet->set_selection
*    EXPORTING
*      left    = 1
*      top     = 3
*      rows    = 24
*      columns = 3.
*
*    CALL METHOD spreadsheet->insert_range
*    EXPORTING
*      columns = 1
*      rows = 24
*      name = 'RangeName'.

* get data
    CALL METHOD spreadsheet->get_ranges_data
*      EXPORTING
*        no_flush  = 'X'
*        all       = 'X'
*        updating  = -1
*        rangesdef =
      IMPORTING
        contents  = excel_input
        error     = errors
*        retcode   =
      CHANGING
        ranges    = ranges
        .

   ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100   INPUT
*    -->P_OBJ_KEY text
*    -->P_DOCNAME text
*---------------------------------------------------------------------
*
FORM   create_basic_objects USING p_app_name
      p_classname
      p_classtype
      p_obj_key
      p_docname .
CHECK initialized IS INITIAL .
* 获取SAP DOI 的控制器接口
CALL METHOD
c_oi_container_control_creator=>get_container_control
IMPORTING
  control = control
  error = error.
* check no errors occured
CALL METHOD error->raise_message
EXPORTING
  type = 'E' .
" 实例化容器实例
CREATE OBJECT container
EXPORTING
  container_name = 'CONTAINER' .
DATA l_app_name(200).
IF p_app_name IS INITIAL .
  l_app_name = 'TEST' .
ELSE .
  l_app_name = p_app_name .
ENDIF .
" 初始化控制器的接口
CALL METHOD control->init_control
EXPORTING
  r3_application_name = l_app_name
  inplace_enabled = inplace
  inplace_scroll_documents = 'X'
  parent = container
  register_on_close_event = 'X'
  register_on_custom_event = 'X'
  no_flush = 'X'
IMPORTING
  error = errors.
* save error object in collection
APPEND errors.
CLEAR item_url .
" 定义BDS实例变量，用于设置文档信息
DATA: bds_instance TYPE REF TO cl_bds_document_set .
DATA: doc_signature TYPE sbdst_signature ,
      wa_doc_signature LIKE LINE OF doc_signature ,
      doc_components TYPE sbdst_components,
      doc_uris TYPE sbdst_uri ,
      wa_doc_uris LIKE LINE OF doc_uris .
* 以下三个值为 Tcode:OAOR 里面新建模板文件的参数
DATA: doc_classname TYPE sbdst_classname VALUE 'ZTEST' ,
      doc_classtype TYPE sbdst_classtype VALUE 'OT' ,
      doc_object_key TYPE sbdst_object_key VALUE 'ZTEST' .
wa_doc_signature-prop_name = 'DESCRIPTION' .
document_type = excel.
wa_doc_signature-prop_value = p_docname. " 赋给文档名字
APPEND wa_doc_signature TO doc_signature . "DOC_SIGNATURE 存储了文档信息
CREATE OBJECT bds_instance .
CALL METHOD bds_instance->get_info " 获取文档信息
EXPORTING
  classname = doc_classname
  classtype = doc_classtype
  object_key = doc_object_key
CHANGING
  components = doc_components
  signature = doc_signature .
CALL METHOD bds_instance->get_with_url " 获取文档的 url 地址放到 DOC_URIS 中
EXPORTING
  classname = doc_classname
  classtype = doc_classtype
  object_key = doc_object_key
CHANGING
  uris = doc_uris
  signature = doc_signature .
FREE bds_instance . " 释放该对象
READ TABLE doc_uris INTO wa_doc_uris INDEX 1.
item_url = wa_doc_uris-uri . " 获取对象的地址
* 告诉SAP DOI 容器开辟一个 Excel 策略
CALL METHOD control->get_document_proxy
EXPORTING
  document_type = 'Excel.Sheet'
  no_flush = 'X'
IMPORTING
  document_proxy = document
  error = errors.
APPEND errors.
* 根据模板地址打开 Excel 文件
CALL METHOD document->open_document
EXPORTING
  open_inplace = inplace
  document_url = item_url .
DATA: has TYPE i .
CALL METHOD document->has_spreadsheet_interface
EXPORTING
  no_flush = ''
IMPORTING
  is_available = has
  error = errors.
APPEND errors.
" 获取模板文档的表格分割器接口给 SPREADSHEET
CALL METHOD document->get_spreadsheet_interface
EXPORTING
  no_flush = ''
IMPORTING
  sheet_interface = spreadsheet
  error = errors.
APPEND errors.
* 激活第一个 sheet
CALL METHOD spreadsheet->select_sheet
EXPORTING
  name   = 'Sheet1'
* NO_FLUSH = ' '
IMPORTING
  error = errors.
* RETCODE   =
.
APPEND errors.
" LOOP AT ERRORS.
"  CALL METHOD ERRORS->RAISE_MESSAGE
"   EXPORTING
"      TYPE = 'E'.
"ENDLOOP.
FREE errors.
initialized = 'X' .
ENDFORM.    "CREATE_BASIC_OBJECTS
*&--------------------------------------------------------------------
*
*&    Form   output_to_excel
*&--------------------------------------------------------------------
*
*    fill the EXCEL sheet
*---------------------------------------------------------------------
*
FORM output_to_excel .
  DATA num TYPE i VALUE 0.
  LOOP AT itab .
    num = sy-tabix + 2.
    PERFORM fill_cell USING num 1 itab-bukrs. " 行列值
    PERFORM fill_cell USING num 2 itab-spras.
    PERFORM fill_cell USING num 3 itab-butxt .
  ENDLOOP.
ENDFORM.    "output_to_excel
*&--------------------------------------------------------------------
*
*&    Form   FILL_CELL
*&--------------------------------------------------------------------
*
*    text
*---------------------------------------------------------------------
*
*    -->I    text
*    -->J    text
*    -->VAL     text
*---------------------------------------------------------------------
*
FORM fill_cell   USING i j val .
DATA: columns_number TYPE i ,
      rows_number TYPE i .
columns_number = 1.
rows_number = 1.
CALL METHOD spreadsheet->insert_range_dim
EXPORTING
  name = 'cell'
  no_flush = 'X'
  top = i
  left    = j
  rows    = rows_number
  columns = columns_number
IMPORTING
  error = errors.
APPEND errors.

REFRESH: ranges, excel_input .
rangeitem-name = 'cell' .
rangeitem-columns = 1.
rangeitem-rows = 1.
APPEND rangeitem TO ranges .

excel_input_wa-column = 1.
excel_input_wa-row = 1.
excel_input_wa-value = val.
APPEND excel_input_wa TO excel_input .
* set data
CALL METHOD spreadsheet->set_ranges_data
EXPORTING
  ranges   = ranges
  contents = excel_input
  no_flush = 'X'
IMPORTING
  error = errors.
APPEND errors.
  CALL METHOD spreadsheet->fit_widest
  EXPORTING
    name = space
    no_flush = 'X' .
*  CALL METHOD spreadsheet->delete_ranges
*  EXPORTING
*    ranges = ranges.
  REFRESH: ranges, excel_input .
ENDFORM.    "fill_cell
