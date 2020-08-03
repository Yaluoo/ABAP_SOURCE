*&---------------------------------------------------------------------*
*& Report ZOSWIN_PFCG2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_role_user.

TABLES:bapibname,bapiagr.
************************************************************************
*类型声明
************************************************************************
TYPES:
  BEGIN OF typ_bname,
    bname TYPE bapibname-bapibname,
  END OF typ_bname,

  BEGIN OF typ_agr,
    agr_name TYPE agr_define-agr_name,
  END OF typ_agr,

  BEGIN OF typ_out,
    bname   TYPE bapibname-bapibname,
    type    TYPE bapiret2-type,
    message TYPE bapiret2-message,
    sel     TYPE c,
  END OF typ_out.

************************************************************************
*变量声明
************************************************************************
DATA:
  it_bname    TYPE TABLE OF typ_bname,
  it_agr      TYPE TABLE OF typ_agr,
  it_out      TYPE TABLE OF typ_out,
  it_fieldcat TYPE slis_t_fieldcat_alv.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_agr  FOR bapiagr-agr_name ,    "角色名称
                s_user FOR bapibname-bapibname . "用户名

PARAMETERS: p_cancel AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1 .

START-OF-SELECTION.
* 抽取数据
  PERFORM frm_get_data.

* 保存数据
  PERFORM frm_save_data.

* ALV显示
  PERFORM frm_output_alv.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       抽取数据
*----------------------------------------------------------------------*
FORM frm_get_data .

  SELECT bname
    INTO TABLE it_bname
    FROM usr02
   WHERE bname IN s_user.

  IF sy-subrc <> 0.
    MESSAGE s001(00) WITH '输入的用户名在系统中不存在！' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SELECT agr_name
    INTO TABLE it_agr
    FROM agr_define
   WHERE agr_name IN s_agr.

  IF sy-subrc <> 0.
    MESSAGE s001(00) WITH '输入的角色在系统中不存在！' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_DATA
*&---------------------------------------------------------------------*
*       保存数据
*----------------------------------------------------------------------*
FORM frm_save_data .
  DATA:
    wa_activity     TYPE bapiagr,
    wa_return       TYPE bapiret2,
    wa_out          TYPE typ_out,
    it_new_activity TYPE TABLE OF bapiagr,
    it_old_activity TYPE TABLE OF bapiagr,
    it_return       TYPE TABLE OF bapiret2.

  LOOP AT it_agr ASSIGNING FIELD-SYMBOL(<fs_agr>).
    CLEAR wa_activity.
    wa_activity-agr_name = <fs_agr>-agr_name.
    wa_activity-from_dat = sy-datum.
    wa_activity-to_dat   = '99991231'.
    APPEND wa_activity TO it_new_activity.
  ENDLOOP.
  BREAK-POINT.

  LOOP AT it_bname ASSIGNING FIELD-SYMBOL(<fs_bname>).
    " 获取用户当前权限
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username       = <fs_bname>-bname
      TABLES
        activitygroups = it_old_activity
        return         = it_return.

    IF p_cancel EQ abap_true. "取消分配
      LOOP AT it_agr INTO DATA(ls_agr).
        DELETE it_old_activity WHERE agr_name = ls_agr-agr_name.
      ENDLOOP.
    ELSE.
      APPEND LINES OF it_new_activity TO it_old_activity.
    ENDIF.

    CLEAR it_return.

    " 分配权限
    CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
      EXPORTING
        username       = <fs_bname>-bname
      TABLES
        activitygroups = it_old_activity
        return         = it_return.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<fs_return>) WHERE type <> 'S' AND type <> 'W'.
      wa_out-message = <fs_return>-message.
      wa_out-type    = <fs_return>-type.
      EXIT.
    ENDLOOP.

    IF sy-subrc <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      wa_out-type = 'S'.
      IF p_cancel eq abap_true.
        wa_out-message = <fs_bname>-bname && '已取消分配！'.
      ELSE.
        wa_out-message = <fs_bname>-bname && '权限分配成功！'.
      ENDIF.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ENDIF.

    wa_out-bname = <fs_bname>-bname.
    APPEND wa_out TO it_out.
    CLEAR:
      it_return,
      it_old_activity,
      wa_out.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT_ALV
*&---------------------------------------------------------------------*
*       ALV显示
*----------------------------------------------------------------------*
FORM frm_output_alv .
  DATA:
    wa_layout   TYPE slis_layout_alv,
    wa_fieldcat TYPE slis_fieldcat_alv.
  CLEAR it_fieldcat.

  DEFINE alv_fieldcat.
    CLEAR wa_fieldcat.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-seltext_l = &2.
    wa_fieldcat-outputlen = &3.
    APPEND wa_fieldcat TO it_fieldcat.
  END-OF-DEFINITION.

  alv_fieldcat: 'BNAME'   '账户名'      '20',
                'TYPE'    '消息类型'    '5',
                'MESSAGE' '消息文本'    '30'.

  wa_layout-colwidth_optimize = 'X'.
  wa_layout-zebra = 'X'.

  wa_layout-box_fieldname = 'SEL'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = it_fieldcat
      i_default          = 'X'
      i_bypassing_buffer = 'X'
      i_save             = 'A'
      is_layout          = wa_layout
    TABLES
      t_outtab           = it_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0. "
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
