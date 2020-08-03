*&---------------------------------------------------------------------*
*& Report ZDEMO_PFCG3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_pfcg.



CLASS lcl_fiori_menu DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_fiori_menu.
        INCLUDE:         TYPE cl_pfcg_menu_tools=>ty_fiori_menu_file.
    TYPES: url_type_text TYPE agr_title
         , text          TYPE agr_hiert-text
         , url           TYPE agr_buffi-url
      , END OF ty_fiori_menu .
    TYPES:
      tt_fiori_menu TYPE STANDARD TABLE OF ty_fiori_menu .

    DATA: mv_own_system    TYPE          tbdls-logsys
        , mt_cat_params    TYPE          /ui2/if_cat_prov_pfcg=>tp_parameters
        .

  METHODS:
      constructor
    , set_outtab
        IMPORTING iv_append   TYPE char01 DEFAULT space
        CHANGING  ct_fiori_menu TYPE tt_fiori_menu
    .

  PRIVATE SECTION.

    CONSTANTS:
          co_function_import      TYPE          ui_func            VALUE 'IMPORT'
        , co_function_export      TYPE          ui_func            VALUE 'EXPORT'
        , co_function_roled       TYPE          ui_func            VALUE 'ROLED'
        , co_function_chk         TYPE          ui_func            VALUE 'CHK'
        .

"oswin
     DATA:
           mt_outtab             TYPE          tt_fiori_menu
         , mt_f4_url_type        TYPE          lvc_t_drop
        .

    METHODS:
*      check RETURNING VALUE(ev_error) TYPE char01
*    ,
    check_appl
        IMPORTING iv_url_type     TYPE url_exits-url_type
                  iv_disp_name    TYPE agr_buffi-url
        EXPORTING ev_text         TYPE agr_hiert-text
    , check_url_type
        IMPORTING iv_url_type     TYPE url_exits-url_type
        RETURNING VALUE(ev_exist) TYPE char01
    , create_url
        IMPORTING iv_url_type     TYPE url_exits-url_type
                  iv_disp_name    TYPE agr_buffi-url
        EXPORTING ev_url          TYPE agr_buffi-url
*    , set_outtab
*        IMPORTING iv_append   TYPE char01 DEFAULT space
*        CHANGING  ct_fiori_menu TYPE tt_fiori_menu
    .

ENDCLASS.

  DATA: gv_okcode         TYPE          sy-ucomm
    , gr_fiori_menu     TYPE REF TO   lcl_fiori_menu
    .

CLASS lcl_fiori_menu IMPLEMENTATION.

  METHOD check_appl.
*        importing iv_url_type     type url_exits-url_type
*                  iv_disp_name    type susr_role_menu-disp_name
*        exporting ev_text         type agr_hiert-text

    DATA: lt_range_catalog TYPE          cl_pfcg_menu_tools=>tt_range_fiori_cat
        , ls_range_catalog LIKE LINE OF  lt_range_catalog
        , lt_range_group   TYPE          cl_pfcg_menu_tools=>tt_range_fiori_grp
        , ls_range_group   LIKE LINE OF  lt_range_group
        , lt_menu_node_f4  TYPE          cl_pfcg_menu_tools=>tt_menu_node_f4
        , lr_menu_node_f4  TYPE REF TO   cl_pfcg_menu_tools=>ty_menu_node_f4
        .


    CLEAR: ev_text.

    CASE iv_url_type.
      WHEN cl_pfcg_menu_tools=>co_url_cat_provider.
        " Check catalog existence and get text
        CLEAR: lt_range_catalog.
        ls_range_catalog-sign    = 'I'.
        ls_range_catalog-option  = 'EQ'.
        ls_range_catalog-low     = iv_disp_name.
        APPEND ls_range_catalog TO lt_range_catalog.

        CALL METHOD cl_pfcg_menu_tools=>f4_get_fiori_catalog
          EXPORTING
            it_range_catalog = lt_range_catalog
          IMPORTING
            et_menu_node_f4  = lt_menu_node_f4.

      WHEN cl_pfcg_menu_tools=>co_url_group_provider.
        " Check group existence and get text
        CLEAR: lt_range_group.
        ls_range_group-sign    = 'I'.
        ls_range_group-option  = 'EQ'.
        ls_range_group-low     = iv_disp_name.
        APPEND ls_range_group TO lt_range_group.

        CALL METHOD cl_pfcg_menu_tools=>f4_get_fiori_group
          EXPORTING
            it_range_group  = lt_range_group
          IMPORTING
            et_menu_node_f4 = lt_menu_node_f4.
    ENDCASE.

    READ TABLE lt_menu_node_f4 REFERENCE INTO lr_menu_node_f4
      WITH KEY node_name = iv_disp_name.
    IF sy-subrc EQ 0.
      ev_text = lr_menu_node_f4->node_text.
      IF ev_text IS INITIAL.
        ev_text = iv_disp_name.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check_url_type.
*        importing iv_url_type     type url_exits-url_type
*        returning value(ev_exist) type char01

    READ TABLE mt_f4_url_type
      WITH KEY value = iv_url_type
      TRANSPORTING NO FIELDS.
    " Allowed menu entries
    IF sy-subrc EQ 0.
      ev_exist = 'X'.
    ELSE.
      ev_exist = space.
    ENDIF.

  ENDMETHOD.

  METHOD create_url.
*        importing iv_url_type     type url_exits-url_type
*                  iv_disp_name    type susr_role_menu-disp_name
*        exporting ev_url          type agr_buffi-url

    DATA: lr_cat_prov    TYPE REF TO   /ui2/cl_cat_prov_pages
        , lv_cat_id_ext  TYPE          agr_buffi-url
        , ls_params      TYPE          /ui2/if_cat_prov_pfcg=>tp_parameter
        , lr_catalog     TYPE REF TO   /ui2/if_catalog
        .

    CLEAR: ev_url.

    CASE iv_url_type.
      WHEN cl_pfcg_menu_tools=>co_url_cat_provider.

        IF mv_own_system IS INITIAL.
          CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
            IMPORTING
              own_logical_system             = mv_own_system
            EXCEPTIONS
              own_logical_system_not_defined = 1
              OTHERS                         = 2.
          IF sy-subrc NE 0.
            ASSERT 1 = 0.
          ENDIF.
        ENDIF.

        IF mt_cat_params IS INITIAL.
          ls_params-name  = 'DEST_FES'.
          ls_params-value = ''.
          INSERT ls_params INTO TABLE mt_cat_params.
          ls_params-name  = 'ROLE_ORIGIN'.
          ls_params-value = mv_own_system.
          INSERT ls_params INTO TABLE mt_cat_params.
          ls_params-name  = 'AUTH_DEFAULTS'.
          ls_params-value = 'X'.
          INSERT ls_params INTO TABLE mt_cat_params.
        ENDIF.

        CONCATENATE 'X-SAP-UI2-CATALOGPAGE:' iv_disp_name INTO lv_cat_id_ext.

        CREATE OBJECT lr_cat_prov.
        CALL METHOD lr_cat_prov->/ui2/if_cat_prov_pfcg~get_pfcg_url_from_parts
          EXPORTING
            iv_catalog_id_ext       = lv_cat_id_ext
            it_params               = mt_cat_params
          RECEIVING
            rv_pfcg_url             = ev_url
          EXCEPTIONS
            parameters_not_supplied = 1
            OTHERS                  = 2.
        IF sy-subrc NE 0.
          " Implement suitable error handling here
        ENDIF.

      WHEN cl_pfcg_menu_tools=>co_url_group_provider.

        CONCATENATE 'sap-ui2-group:' iv_disp_name INTO ev_url.
    ENDCASE.

  ENDMETHOD.

  METHOD constructor.

    DATA: ls_f4_url_type TYPE lvc_s_drop.

    " Available menu types

    " First listbox (handle '1').
    ls_f4_url_type-handle = '1'.
    ls_f4_url_type-value = 'CAT_PROVIDER'.
    APPEND ls_f4_url_type TO mt_f4_url_type.

    ls_f4_url_type-handle = '1'.
    ls_f4_url_type-value = 'GROUP_PROVIDER'.
    APPEND ls_f4_url_type TO mt_f4_url_type.

  ENDMETHOD.

  METHOD set_outtab.
*        importing iv_append   type char01 default space
*        changing  ct_fiori_menu type tt_fiori_menu

    DATA: lr_fiori_menu    TYPE REF TO   ty_fiori_menu
        .

    IF iv_append EQ space.
      mt_outtab = ct_fiori_menu.
    ELSE.
      APPEND LINES OF ct_fiori_menu TO mt_outtab.
    ENDIF.

    LOOP AT mt_outtab REFERENCE INTO lr_fiori_menu.
      IF me->check_url_type( lr_fiori_menu->url_type ) EQ 'X'. "url type
        lr_fiori_menu->url_type_text = cl_pfcg_menu_tools=>url_type_get_text( iv_url_type = lr_fiori_menu->url_type ).
      ENDIF.

      CALL METHOD me->check_appl
        EXPORTING
          iv_url_type  = lr_fiori_menu->url_type  "url_ytpe
          iv_disp_name = lr_fiori_menu->disp_name "catalog or group name
        IMPORTING
          ev_text      = lr_fiori_menu->text.

      IF lr_fiori_menu->text IS NOT INITIAL.
        CALL METHOD me->create_url
          EXPORTING
            iv_url_type  = lr_fiori_menu->url_type
            iv_disp_name = lr_fiori_menu->disp_name
          IMPORTING
            ev_url       = lr_fiori_menu->url.
      ENDIF.
    ENDLOOP.

    REFRESH: ct_fiori_menu.
    ct_fiori_menu = mt_outtab.

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA: ct_fiori_menu TYPE lcl_fiori_menu=>tt_fiori_menu.

  BREAK-POINT.
  ct_fiori_menu = VALUE #( ( agr_name  = 'ZOSWIN_TEST01'
                             role_text = '测试'
                             url_type  = 'CAT_PROVIDER'  "CAT_PROVIDER    GROUP_PROVIDER
                             disp_name = 'zxuwp_test' )
                           ( agr_name  = 'ZOSWIN_TEST01'
                             role_text = '测试'
                             url_type  = 'GROUP_PROVIDER'  "
                             disp_name = 'zxuwp_test' )
                          ).


  PERFORM set_outtab TABLES ct_fiori_menu.

  PERFORM save_data TABLES ct_fiori_menu.

  BREAK-POINT.

*********************************************************************************
*&---------------------------------------------------------------------*
*& Form save_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_FIORI_MENU
*&---------------------------------------------------------------------*
FORM save_data  TABLES lt_fiori_menu LIKE ct_fiori_menu.

    DATA: mv_create_role        TYPE          char01 VALUE 'X'
        , mv_delete_menu        TYPE          char01
        , mv_genprof            TYPE          char01
        , mv_data_changed       TYPE          char01
        .

    DATA: lr_fiori_menu     TYPE REF TO   lcl_fiori_menu=>ty_fiori_menu
        , lt_menu_4_role    TYPE          lcl_fiori_menu=>tt_fiori_menu
        , lv_title          TYPE          agr_title
        , lv_error          TYPE          char01
        , lv_abort          TYPE          char01
        , lt_messages       TYPE          cl_pfcg_menu_tools=>tt_pfcg_msg_log
*        , lt_fiori_menu     type          lcl_fiori_menu=>tt_fiori_menu
        .

*    IF iv_mode_create EQ 'X'.
*      mv_create_role = 'X'.
*    ENDIF.
*    IF iv_mode_overwr EQ 'X'.
*      mv_delete_menu = 'X'.
*    ENDIF.
*    mv_genprof = iv_genprof.

    SORT lt_fiori_menu BY agr_name.

    LOOP AT lt_fiori_menu REFERENCE INTO lr_fiori_menu.
      AT NEW agr_name.
        CLEAR: lt_menu_4_role, lv_title.
      ENDAT.

      " Take first valid title
      IF lv_title IS INITIAL.
        lv_title = lr_fiori_menu->role_text.
      ENDIF.

      APPEND lr_fiori_menu->* TO lt_menu_4_role.

      AT END OF agr_name.
        IF lt_menu_4_role IS INITIAL.
          CONTINUE.
        ENDIF.

        " Create roles
        IF mv_create_role EQ 'X'.
          PERFORM create_role USING lr_fiori_menu->agr_name lv_title CHANGING lt_messages lv_error lv_abort.
          IF lv_abort EQ 'X'.
            EXIT.
          ENDIF.
          IF lv_error EQ 'X'.
            CONTINUE.
          ENDIF.
        ENDIF.

        " Create or append menu
        PERFORM create_role_menu USING    lr_fiori_menu->agr_name lt_menu_4_role mv_delete_menu
                                 CHANGING lt_messages lv_error lv_abort.
        IF lv_abort EQ 'X'.
          EXIT.
        ENDIF.
        IF lv_error EQ 'X'.
          CONTINUE.
        ENDIF.

        " Generate profile
        IF mv_genprof EQ 'X'.
          PERFORM create_auth USING lr_fiori_menu->agr_name CHANGING lt_messages lv_error lv_abort.
        ENDIF.
        IF lv_abort EQ 'X'.
          EXIT.
        ENDIF.

      ENDAT.
    ENDLOOP.

    IF lt_messages IS NOT INITIAL.
      CALL METHOD cl_pfcg_menu_tools=>message_log_display
        EXPORTING
          iv_popup    = 'X'
        CHANGING
          ct_messages = lt_messages.
    ENDIF.

ENDFORM.
FORM create_role USING    iv_dest_role TYPE agr_name
                          iv_title     TYPE agr_title
                 CHANGING ct_messages  TYPE cl_pfcg_menu_tools=>tt_pfcg_msg_log
                          ev_error     TYPE char01
                          ev_abort     TYPE char01.

  DATA: lt_return         TYPE          bapirettab
      .

  CLEAR: ev_error, ev_abort.

  CALL FUNCTION 'PRGN_CHECK_ROLE_EXISTS'
    EXPORTING
      role_name           = iv_dest_role
    EXCEPTIONS
      role_does_not_exist = 1.

  IF sy-subrc EQ 1.
    CALL FUNCTION 'PRGN_RFC_CREATE_AGR_MULTIPLE'
      EXPORTING
        activity_group                = iv_dest_role
        activity_group_text           = iv_title
      TABLES
        return                        = lt_return
      EXCEPTIONS
        activity_group_already_exists = 1
        invalid_inheriting_role       = 2
        activity_group_enqueued       = 3
        namespace_problem             = 4
        illegal_characters            = 5
        error_when_creating_actgroup  = 6
        not_authorized                = 7
        OTHERS                        = 8.
    IF sy-subrc NE 0.
      ev_error = 'X'.

      CALL METHOD cl_pfcg_menu_tools=>message_log_add_msg_for_role
        EXPORTING
          it_return   = lt_return
          iv_role     = iv_dest_role
        CHANGING
          ct_messages = ct_messages.
    ELSE.
      COMMIT WORK.
    ENDIF.
  ENDIF.

ENDFORM.

FORM create_role_menu USING    iv_dest_role TYPE agr_name
                               it_menu_asg  TYPE lcl_fiori_menu=>tt_fiori_menu
                               iv_del_menu  TYPE char01
                      CHANGING ct_messages  TYPE cl_pfcg_menu_tools=>tt_pfcg_msg_log
                               ev_error     TYPE char01
                               ev_abort     TYPE char01.

  DATA: lr_role         TYPE REF TO   cl_pfcg_menu_modify
      , lt_return       TYPE          bapirettab
      , lr_fiori_menu   TYPE REF TO   lcl_fiori_menu=>ty_fiori_menu
      , lv_rejected     TYPE          abap_bool
      , lt_nodes        TYPE          cl_pfcg_menu_modify=>tt_menu_node
      .

  CLEAR: ev_error, ev_abort.


  " Create instance
  CALL METHOD cl_pfcg_menu_modify=>retrieve_for_update
    EXPORTING
      iv_role   = iv_dest_role
    IMPORTING
      er_role   = lr_role
      et_return = lt_return.

  CALL METHOD cl_pfcg_menu_tools=>message_log_add_msg_for_role
    EXPORTING
      it_return   = lt_return
      iv_role     = iv_dest_role
    CHANGING
      ct_messages = ct_messages.

  IF lr_role IS INITIAL. RETURN. ENDIF.

  IF iv_del_menu EQ 'X'.
    CALL METHOD lr_role->menu_get_nodes
      IMPORTING
        et_nodes  = lt_nodes
        et_return = lt_return.
    IF lt_nodes IS NOT INITIAL.
      CALL METHOD lr_role->menu_delete_node
        EXPORTING
          iv_object_id = '1'
        IMPORTING
          et_return    = lt_return.

      CALL METHOD cl_pfcg_menu_tools=>message_log_add_msg_for_role
        EXPORTING
          it_return   = lt_return
          iv_role     = iv_dest_role
        CHANGING
          ct_messages = ct_messages.

      READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        CALL METHOD lr_role->cancel( ).
        ev_error = 'X'.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  " Add menu
  LOOP AT it_menu_asg REFERENCE INTO lr_fiori_menu.
    CASE lr_fiori_menu->url_type.
      WHEN cl_pfcg_menu_tools=>co_url_cat_provider.

        " lv_node_text = lr_fiori_menu->url.
        CALL METHOD lr_role->menu_add_application_group
          EXPORTING
            iv_first_node_in_folder = ''
            iv_node_text            = lr_fiori_menu->text
            iv_url_type             = lr_fiori_menu->url_type
            iv_url                  = lr_fiori_menu->url
            iv_calculate_apps       = 'X'
          IMPORTING
            et_return               = lt_return.

      WHEN cl_pfcg_menu_tools=>co_url_group_provider.
        CALL METHOD lr_role->menu_add_url
          EXPORTING
            iv_first_node_in_folder = ''
            iv_node_text            = lr_fiori_menu->text
            iv_url_type             = lr_fiori_menu->url_type
            iv_url                  = lr_fiori_menu->url
          IMPORTING
            et_return               = lt_return.

    ENDCASE.

    CALL METHOD cl_pfcg_menu_tools=>message_log_add_msg_for_role
      EXPORTING
        it_return   = lt_return
        iv_role     = iv_dest_role
      CHANGING
        ct_messages = ct_messages.
  ENDLOOP.

  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    CALL METHOD lr_role->cancel( ).
    ev_error = 'X'.
    RETURN.
  ENDIF.

  CALL METHOD cl_pfcg_menu_modify=>save
    IMPORTING
      ev_rejected = lv_rejected
      et_return   = lt_return.

  CALL METHOD cl_pfcg_menu_tools=>message_log_add_msg_for_role
    EXPORTING
      it_return   = lt_return
      iv_role     = iv_dest_role
    CHANGING
      ct_messages = ct_messages.

  IF lv_rejected EQ space.
    COMMIT WORK.
  ELSE.
    ev_error = 'X'.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.


FORM create_auth USING    iv_dest_role TYPE agr_name
                 CHANGING ct_messages  TYPE cl_pfcg_menu_tools=>tt_pfcg_msg_log
                          ev_error     TYPE char01
                          ev_abort     TYPE char01.

  DATA: lt_return         TYPE          bapirettab
      .

  CLEAR: ev_error, ev_abort.


  CALL FUNCTION 'PRGN_AUTO_GENERATE_PROFILE_NEW'
    EXPORTING
      activity_group                = iv_dest_role
      no_dialog                     = 'X'
      rebuild_auth_data             = 'X'
      org_levels_with_star          = 'X'
      fill_empty_fields_with_star   = 'X'
      generate_profile              = 'X'
    TABLES
      return                        = lt_return
    EXCEPTIONS
      activity_group_does_not_exist = 1
      activity_group_enqueued       = 2
      profile_name_exists           = 3
      profile_not_in_namespace      = 4
      no_auth_for_prof_creation     = 5
      no_auth_for_role_change       = 6
      no_auth_for_auth_maint        = 7
      no_auth_for_gen               = 8
      no_auths                      = 9
      open_auths                    = 10
      too_many_auths                = 11
      profgen_tables_not_updated    = 12
      error_when_generating_profile = 13
      OTHERS                        = 14.
  IF sy-subrc NE 0.
    ev_error = 'X'.

    CALL METHOD cl_pfcg_menu_tools=>message_log_add_msg_for_role
      EXPORTING
        it_return   = lt_return
        iv_role     = iv_dest_role
      CHANGING
        ct_messages = ct_messages.
  ELSE.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_outtab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CT_FIORI_MENU
*&---------------------------------------------------------------------*
FORM set_outtab TABLES lt_fiori_menu LIKE ct_fiori_menu.

  CREATE OBJECT gr_fiori_menu.

  CALL METHOD gr_fiori_menu->set_outtab
    CHANGING
      ct_fiori_menu = lt_fiori_menu[].

ENDFORM.
