*&---------------------------------------------------------------------*
*& Report ZDMEO_PFCG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_pfcg2.

TABLES:bapibname,bapiagr.

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
*    , cancel
    , init
        IMPORTING iv_mode_create TYPE char01
                  iv_mode_update TYPE char01
                  iv_mode_overwr TYPE char01
                  iv_genprof     TYPE char01
*    , pbo
    , set_mapping
        IMPORTING iv_append   TYPE char01 DEFAULT space
                  iv_fullpath TYPE string OPTIONAL
                  iv_codepage TYPE abap_encoding OPTIONAL
        CHANGING  ct_fiori_menu TYPE tt_fiori_menu OPTIONAL
*    , user_command
    .

  PRIVATE SECTION.

    CONSTANTS:
          co_function_import      TYPE          ui_func            VALUE 'IMPORT'
        , co_function_export      TYPE          ui_func            VALUE 'EXPORT'
        , co_function_roled       TYPE          ui_func            VALUE 'ROLED'
        , co_function_chk         TYPE          ui_func            VALUE 'CHK'
        .


    DATA: mr_container          TYPE REF TO   cl_gui_custom_container
        , mr_alv_grid           TYPE REF TO   cl_gui_alv_grid
        , mt_outtab             TYPE          tt_fiori_menu
        , mt_f4_url_type        TYPE          lvc_t_drop
        , mv_create_role        TYPE          char01
        , mv_delete_menu        TYPE          char01
        , mv_genprof            TYPE          char01
        , mv_data_changed       TYPE          char01
        .

    METHODS:
      check RETURNING VALUE(ev_error) TYPE char01
    , check_appl
        IMPORTING iv_url_type     TYPE url_exits-url_type
                  iv_disp_name    TYPE agr_buffi-url
        EXPORTING ev_text         TYPE agr_hiert-text
    , check_changed_data
        RETURNING VALUE(ev_valid) TYPE char01
    , check_role
        IMPORTING iv_agr_name     TYPE agr_name
        EXPORTING es_symsg        TYPE symsg
        RETURNING VALUE(ev_error) TYPE char01
    , check_url_type
        IMPORTING iv_url_type     TYPE url_exits-url_type
        RETURNING VALUE(ev_exist) TYPE char01
    , create_url
        IMPORTING iv_url_type     TYPE url_exits-url_type
                  iv_disp_name    TYPE agr_buffi-url
        EXPORTING ev_url          TYPE agr_buffi-url
    , display_role
        IMPORTING
          is_row_id TYPE lvc_s_row OPTIONAL
          e_column  TYPE lvc_s_col OPTIONAL
    , ev_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
    , ev_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING
          e_modified et_good_cells
    , ev_handle_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display
    , ev_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
*    , ev_handle_menu for event menu_button of cl_gui_alv_grid
*        importing e_object e_ucomm
    , ev_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
    , ev_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm
    , export
    , f4_url_type_drdn
    , get_fieldcat EXPORTING et_fieldcat TYPE lvc_t_fcat
    , import
        IMPORTING iv_fullpath TYPE string OPTIONAL
                  iv_codepage TYPE abap_encoding OPTIONAL
                  iv_refresh  TYPE char01 DEFAULT space
    , refresh_alv
    , save
    , set_outtab
        IMPORTING iv_append   TYPE char01 DEFAULT space
        CHANGING  ct_fiori_menu TYPE tt_fiori_menu
    .

ENDCLASS.

DATA: gv_okcode         TYPE          sy-ucomm
    , gr_fiori_menu     TYPE REF TO   lcl_fiori_menu
    .

CLASS lcl_fiori_menu IMPLEMENTATION.

*  method cancel.
*    data: lv_answer   type char01
*        .
*
*    if gv_okcode eq 'CANC'.
*      perform exit_popup using    'Cancel?'(015)  ##TEXT_DIFF
*                         changing lv_answer.
*      if lv_answer eq 'Y'.
*        leave to screen 0.
*        return.
*      else.
*        clear: gv_okcode.
*      endif.
*    endif.
*
*  endmethod.

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

  METHOD init.

    IF iv_mode_create EQ 'X'.
      mv_create_role = 'X'.
    ENDIF.
    IF iv_mode_overwr EQ 'X'.
      mv_delete_menu = 'X'.
    ENDIF.

    mv_genprof = iv_genprof.

  ENDMETHOD.

*  method pbo.
*
*    data: lt_fieldcat     type          lvc_t_fcat
*        , ls_layout       type          lvc_s_layo
*        , lt_exclude      type          ui_functions
*        , ls_variant      type          disvariant
*        , ls_f4_event     type          lvc_s_f4
*        , lt_f4_event     type          lvc_t_f4
*        .
*
*    set pf-status '0100'.
*    set titlebar '0100'.
*
*    if mr_container is initial.
*
*      " exclude not needed function from toolbar
*      append cl_gui_alv_grid=>mc_fc_graph              to lt_exclude.
*      append cl_gui_alv_grid=>mc_fc_sum                to lt_exclude.
*      append cl_gui_alv_grid=>mc_mb_sum                to lt_exclude.
*      append cl_gui_alv_grid=>mc_fc_subtot             to lt_exclude.
*      append cl_gui_alv_grid=>mc_fc_refresh            to lt_exclude.
*      append cl_gui_alv_grid=>mc_fc_loc_copy_row       to lt_exclude.
*      append cl_gui_alv_grid=>mc_fc_loc_cut            to lt_exclude.
*      append cl_gui_alv_grid=>mc_mb_export             to lt_exclude.
*
*      create object mr_container
*        exporting
*          container_name = 'CONTROL_ROLES'.
*
*      create object mr_alv_grid
*        exporting
*          i_parent = mr_container.
*
*      " Save variant
*      ls_variant-report = sy-repid.
*
*      " Layout
*      if mv_create_role eq 'X'.
*        ls_layout-grid_title = t_crea.
*      else.
*        if mv_delete_menu eq space.
*          ls_layout-grid_title = t_upd.
*        else.
*          ls_layout-grid_title = t_ovr.
*        endif.
*      endif.
*      ls_layout-smalltitle = 'X'.
*
*      me->get_fieldcat( importing et_fieldcat = lt_fieldcat ).
*
*      me->f4_url_type_drdn( ).
*
*      " display grid
*      call method mr_alv_grid->set_table_for_first_display
*        exporting
*          is_variant           = ls_variant
*          i_save               = 'A'
*          is_layout            = ls_layout
*          it_toolbar_excluding = lt_exclude
*        changing
*          it_outtab            = mt_outtab
*          it_fieldcatalog      = lt_fieldcat.
*
*      call method mr_alv_grid->set_ready_for_input
*        exporting
*          i_ready_for_input = 1.
*
*      " set event handlers
*      set handler me->ev_handle_toolbar        for mr_alv_grid.
**      set handler me->ev_handle_menu           for mr_alv_grid.
*      set handler me->ev_user_command          for mr_alv_grid.
*      set handler me->ev_data_changed          for mr_alv_grid.
*      set handler me->ev_data_changed_finished for mr_alv_grid.
*      set handler me->ev_double_click          for mr_alv_grid.
*      set handler me->ev_handle_f4             for mr_alv_grid.
*
*      call method mr_alv_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
*
*      " F4: Application - Check after F4 help
*      clear: ls_f4_event.
*      ls_f4_event-fieldname  = 'DISP_NAME'.
*      ls_f4_event-register   = 'X'.
*      ls_f4_event-getbefore  = 'X'. " In the middle of both events F4 is handled
*      ls_f4_event-chngeafter = 'X'.
*      append ls_f4_event to lt_f4_event.
*      call method mr_alv_grid->register_f4_for_fields( it_f4 = lt_f4_event ).
*
*      mr_alv_grid->set_toolbar_interactive( ).
*    endif.
*
*  endmethod.

  METHOD set_mapping.

    DATA: lr_fiori_menu    TYPE REF TO   ty_fiori_menu
        .

    IF ct_fiori_menu IS NOT INITIAL.
      CALL METHOD gr_fiori_menu->set_outtab
        EXPORTING
          iv_append     = iv_append
        CHANGING
          ct_fiori_menu = ct_fiori_menu.
    ELSEIF iv_fullpath IS NOT INITIAL.
      CALL METHOD me->import
        EXPORTING
          iv_fullpath = iv_fullpath
          iv_codepage = iv_codepage.
    ENDIF.

  ENDMETHOD.

*  method user_command.
*
*    data: lv_valid    type char01
*        , lv_answer   type char01
*        .
*
*    lv_valid = gr_fiori_menu->check_changed_data( ).
*    if lv_valid ne 'X'.
*      clear: gv_okcode.
*      return.
*    endif.
*
*    case gv_okcode.
*      when 'EXEC'.
*        call method me->save( ).
*      when 'BACK' or 'EXIT'.
*        if mv_data_changed eq 'X'.
*          perform exit_popup using    'Cancel?'(015) ##TEXT_DIFF
*                             changing lv_answer.
*          if lv_answer ne 'Y'.
*            clear: gv_okcode.
*            return.
*          endif.
*        endif.
*
*        leave to screen 0.
*      when others.
*    endcase.
*
*  endmethod.

  METHOD check.

    DATA: ls_row_id          TYPE          lvc_s_row
        , ls_column_id       TYPE          lvc_s_col
        , lr_outtab          TYPE REF TO   ty_fiori_menu
        , lv_changed         TYPE          char01
        , ls_symsg           TYPE          symsg
        .

    ev_error = 'X'.

    LOOP AT mt_outtab REFERENCE INTO lr_outtab.
      ls_row_id-index = sy-tabix.

      IF lr_outtab->* IS INITIAL.
        CONTINUE.
      ENDIF.

      " Fill all required fields
      IF lr_outtab->agr_name  IS INITIAL OR
               lr_outtab->url_type  IS INITIAL OR
               lr_outtab->disp_name IS INITIAL.
        " Set cursor
        IF lr_outtab->agr_name IS INITIAL.
          ls_column_id-fieldname = 'AGR_NAME'.
        ELSEIF lr_outtab->url_type IS INITIAL.
          ls_column_id-fieldname = 'URL_TYPE'.
        ELSE.
          ls_column_id-fieldname = 'DISP_NAME'.
        ENDIF.

        CALL METHOD mr_alv_grid->set_current_cell_via_id
          EXPORTING
            is_row_id    = ls_row_id
            is_column_id = ls_column_id.

        MESSAGE s055(00) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      " Check Role
      IF me->check_role( EXPORTING iv_agr_name = lr_outtab->agr_name IMPORTING es_symsg = ls_symsg ) EQ 'X'.
        ls_column_id-fieldname = 'AGR_NAME'.
        CALL METHOD mr_alv_grid->set_current_cell_via_id
          EXPORTING
            is_row_id    = ls_row_id
            is_column_id = ls_column_id.

        MESSAGE ID ls_symsg-msgid TYPE 'S' NUMBER ls_symsg-msgno
                WITH ls_symsg-msgv1 ls_symsg-msgv2 ls_symsg-msgv3 ls_symsg-msgv4
                DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      " If URL is not filled: URL_TYPE or DISP_NAME must be wrong
      IF lr_outtab->url IS INITIAL.
        " Allowed menu entries
        IF me->check_url_type( lr_outtab->url_type ) EQ space.
          ls_column_id-fieldname = 'URL_TYPE'.
          CALL METHOD mr_alv_grid->set_current_cell_via_id
            EXPORTING
              is_row_id    = ls_row_id
              is_column_id = ls_column_id.
          MESSAGE s319(01) WITH 'Invalid type'(016) lr_outtab->url_type space space DISPLAY LIKE 'E' ##TEXT_DIFF.
          RETURN.
        ELSE.
          lr_outtab->url_type_text = cl_pfcg_menu_tools=>url_type_get_text( iv_url_type = lr_outtab->url_type ).
        ENDIF.

        " Application must be wrong
        CALL METHOD me->check_appl
          EXPORTING
            iv_url_type  = lr_outtab->url_type
            iv_disp_name = lr_outtab->disp_name
          IMPORTING
            ev_text      = lr_outtab->text.
        IF lr_outtab->text IS INITIAL.
          ls_column_id-fieldname = 'DISP_NAME'.
          CALL METHOD mr_alv_grid->set_current_cell_via_id
            EXPORTING
              is_row_id    = ls_row_id
              is_column_id = ls_column_id.
          MESSAGE s319(01) WITH 'Invalid name'(017) lr_outtab->disp_name space space DISPLAY LIKE 'E' ##TEXT_DIFF.
          RETURN.
        ELSE.
          " Fill URL
          lv_changed = 'X'.
          CALL METHOD me->create_url
            EXPORTING
              iv_url_type  = lr_outtab->url_type
              iv_disp_name = lr_outtab->disp_name
            IMPORTING
              ev_url       = lr_outtab->url.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_changed EQ 'X'.
      me->refresh_alv( ).
    ENDIF.

    MESSAGE s071(0k).
    CLEAR: ev_error.

  ENDMETHOD.

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

  METHOD check_changed_data.
*        returning value(ev_valid) type char01

    CALL METHOD mr_alv_grid->check_changed_data
      IMPORTING
        e_valid = ev_valid.

  ENDMETHOD.

  METHOD check_role.
*        importing iv_agr_name     type agr_name
*        exporting es_symsg        type symsg
*        returning value(ev_error) type char01

    CLEAR: es_symsg, ev_error.

    " Selecting or determining the role type
    CALL FUNCTION 'PRGN_GET_COLLECTIVE_AGR_FLAG'
      EXPORTING
        activity_group     = iv_agr_name
      EXCEPTIONS
        flag_not_available = 1
        agr_does_not_exist = 2
        OTHERS             = 3.
    IF sy-subrc LE 1.
      " Role exists
      IF mv_create_role EQ 'X'.
        " Role should be created -> Error
        ev_error = 'X'.
        IF 1 = 0. MESSAGE s409(s#) WITH space. ENDIF.
        es_symsg-msgid = 'S#'.
        es_symsg-msgno = '409'.
        es_symsg-msgty = 'S'.
        es_symsg-msgv1 = iv_agr_name.
      ENDIF.
    ELSE.
      " Role does not exist
      IF mv_create_role EQ space.
        " Role should be changed -> Error
        ev_error = 'X'.
        IF 1 = 0. MESSAGE s410(s#) WITH space. ENDIF.
        es_symsg-msgid = 'S#'.
        es_symsg-msgno = '410'.
        es_symsg-msgty = 'S'.
        es_symsg-msgv1 = iv_agr_name.
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

  METHOD display_role.
*        importing
*          is_row_id type lvc_s_row optional
*          e_column  type lvc_s_col optional

    DATA: ls_row_id         TYPE          lvc_s_row
        , lr_outtab         TYPE REF TO   ty_fiori_menu
        .

    IF is_row_id IS INITIAL.
      mr_alv_grid->get_current_cell( IMPORTING es_row_id = ls_row_id ).
      IF ls_row_id-index IS INITIAL.
        MESSAGE s700(s#) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ELSE.
      ls_row_id = is_row_id.
    ENDIF.

    READ TABLE mt_outtab REFERENCE INTO lr_outtab INDEX ls_row_id-index.
    IF sy-subrc NE 0.
      MESSAGE s700(s#) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Display role
    CALL FUNCTION 'PRGN_ACTIVITY_GROUP_MAINTAIN'
      EXPORTING
        activity_group           = lr_outtab->agr_name
        action                   = 'A'
      EXCEPTIONS
        activity_group_not_found = 1
        OTHERS                   = 2.
    IF sy-subrc NE 0.
      MESSAGE s410(s#) WITH lr_outtab->agr_name.
    ENDIF.

  ENDMETHOD.

  METHOD ev_data_changed.
*        importing
*          er_data_changed
*          e_onf4
*          e_onf4_before
*          e_onf4_after
*          e_ucomm

    DATA: lr_good_cell  TYPE REF TO   lvc_s_modi
        , lv_value(150) TYPE          c
        , ls_menu_line  TYPE          ty_fiori_menu
        , ls_symsg      TYPE          symsg
        .

    LOOP AT er_data_changed->mt_good_cells REFERENCE INTO lr_good_cell.
      CLEAR: ls_menu_line.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = lr_good_cell->row_id
          i_fieldname = lr_good_cell->fieldname
        IMPORTING
          e_value     = lv_value.

      CASE lr_good_cell->fieldname.
        WHEN 'AGR_NAME'.
          ls_menu_line-agr_name = lv_value.

          IF ls_menu_line-agr_name IS NOT INITIAL.
            " Check Role
            IF me->check_role( EXPORTING iv_agr_name = ls_menu_line-agr_name IMPORTING es_symsg = ls_symsg ) EQ 'X'.
              CALL METHOD er_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = ls_symsg-msgid
                  i_msgno     = ls_symsg-msgno
                  i_msgty     = 'E'
                  i_msgv1     = ls_symsg-msgv1
                  i_msgv2     = ls_symsg-msgv2
                  i_msgv3     = ls_symsg-msgv3
                  i_msgv4     = ls_symsg-msgv4
                  i_fieldname = 'AGR_NAME'
                  i_row_id    = lr_good_cell->row_id.
            ENDIF.
          ENDIF.

        WHEN 'URL_TYPE'.
          ls_menu_line-url_type = lv_value.

          CLEAR: ls_menu_line-url_type_text
               , ls_menu_line-text
               , ls_menu_line-url
               .

          IF ls_menu_line-url_type IS NOT INITIAL.
            " Allowed menu entries
            IF me->check_url_type( ls_menu_line-url_type ) EQ 'X'.
              ls_menu_line-url_type_text = cl_pfcg_menu_tools=>url_type_get_text( iv_url_type = ls_menu_line-url_type ).
            ENDIF.
          ENDIF.

          " Set URL_TYPE Text
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = lr_good_cell->row_id
              i_fieldname = 'URL_TYPE_TEXT'
              i_value     = ls_menu_line-url_type_text.

          " Set Text
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = lr_good_cell->row_id
              i_fieldname = 'TEXT'
              i_value     = ls_menu_line-text.
          " Set URL
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = lr_good_cell->row_id
              i_fieldname = 'URL'
              i_value     = ls_menu_line-url.

          IF ls_menu_line-url_type      IS NOT INITIAL AND
             ls_menu_line-url_type_text IS INITIAL.
            " Allowed menu entries
            IF 1 = 0. MESSAGE s319(01) WITH space space space space. ENDIF.
            CALL METHOD er_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = '01'
                i_msgno     = '319'
                i_msgty     = 'E'
                i_fieldname = 'URL_TYPE'
                i_msgv1     = 'Invalid type'(016) ##TEXT_DIFF
                i_msgv2     = ls_menu_line-url_type
                i_row_id    = lr_good_cell->row_id.
          ENDIF.

        WHEN 'DISP_NAME'.
          ls_menu_line-disp_name = lv_value.

          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = lr_good_cell->row_id
              i_fieldname = 'URL_TYPE'
            IMPORTING
              e_value     = ls_menu_line-url_type.

          CLEAR: ls_menu_line-text
               , ls_menu_line-url
               .

          IF ls_menu_line-disp_name IS NOT INITIAL AND
             ls_menu_line-url_type  IS NOT INITIAL.

            " Upper case conversion - not required...
*            case ls_menu_line-url_type.
*              when cl_pfcg_menu_tools=>co_url_cat_provider.
*              when cl_pfcg_menu_tools=>co_url_group_provider.
*                lv_value = ls_menu_line-disp_name.
*                translate ls_menu_line-disp_name to upper case.
*                if lv_value ne ls_menu_line-disp_name.
*                  call method er_data_changed->modify_cell
*                    exporting
*                      i_row_id    = lr_good_cell->row_id
*                      i_fieldname = lr_good_cell->fieldname
*                      i_value     = ls_menu_line-disp_name.
*                endif.
*              when others.
*            endcase.

            CALL METHOD me->check_appl
              EXPORTING
                iv_url_type  = ls_menu_line-url_type
                iv_disp_name = ls_menu_line-disp_name
              IMPORTING
                ev_text      = ls_menu_line-text.

            IF ls_menu_line-text IS NOT INITIAL.
              CALL METHOD me->create_url
                EXPORTING
                  iv_url_type  = ls_menu_line-url_type
                  iv_disp_name = ls_menu_line-disp_name
                IMPORTING
                  ev_url       = ls_menu_line-url.
            ENDIF.
          ENDIF.

          " Set Text
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = lr_good_cell->row_id
              i_fieldname = 'TEXT'
              i_value     = ls_menu_line-text.
          " Set URL
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = lr_good_cell->row_id
              i_fieldname = 'URL'
              i_value     = ls_menu_line-url.

          " Last thing: Error message
          IF ls_menu_line-disp_name IS NOT INITIAL AND
             ls_menu_line-url_type  IS NOT INITIAL AND
             ls_menu_line-text      IS INITIAL.
            IF 1 = 0. MESSAGE s319(01) WITH space space space space. ENDIF.
            CALL METHOD er_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = '01'
                i_msgno     = '319'
                i_msgty     = 'E'
                i_fieldname = 'DISP_NAME'
                i_msgv1     = 'Invalid name'(017) ##TEXT_DIFF
                i_msgv2     = ls_menu_line-disp_name
                i_row_id    = lr_good_cell->row_id.
          ENDIF.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD ev_data_changed_finished.
*        importing
*          e_modified
*          et_good_cells

    IF e_modified EQ 'X'.
      mv_data_changed = 'X'. " Manual changes
    ENDIF.

  ENDMETHOD.

  METHOD ev_double_click.
*        importing
*          e_row
*          e_column

    CASE e_column.
      WHEN 'AGR_NAME'.
        me->display_role( EXPORTING is_row_id = e_row e_column = e_column ).
    ENDCASE.

  ENDMETHOD.

  METHOD ev_handle_f4.
*        importing
*          e_fieldname
*          e_fieldvalue
*          es_row_no
*          er_event_data
*          et_bad_cells
*          e_display

    DATA: lr_outtab          TYPE REF TO   ty_fiori_menu
        , lv_searchhelp      TYPE          shlpname
        , lt_return_values   TYPE TABLE OF ddshretval
        , lr_return_values   TYPE REF TO   ddshretval
        , lv_value           TYPE          help_info-fldvalue
        , ls_modi            TYPE          lvc_s_modi
        .

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    IF e_fieldname NE 'DISP_NAME'.
      RETURN.
    ENDIF.

    er_event_data->m_event_handled = 'X'.

    READ TABLE mt_outtab REFERENCE INTO lr_outtab INDEX es_row_no-row_id.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CASE lr_outtab->url_type.
      WHEN cl_pfcg_menu_tools=>co_url_cat_provider.
        lv_searchhelp = '/UI2/PAGES_SH'.
      WHEN cl_pfcg_menu_tools=>co_url_group_provider.
        lv_searchhelp = '/UI2/GROUPS_SH'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    READ TABLE et_bad_cells INTO ls_modi
      WITH KEY row_id    = es_row_no-row_id
               fieldname = 'DISP_NAME'.
    IF sy-subrc EQ 0.
      lv_value = ls_modi-value.
    ELSE.
      lv_value = e_fieldvalue.
    ENDIF.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = ''
        fieldname         = ''
        searchhelp        = lv_searchhelp
        value             = lv_value
        multiple_choice   = space
        display           = space
      TABLES
        return_tab        = lt_return_values
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE lt_return_values REFERENCE INTO lr_return_values INDEX 1.
    IF sy-subrc NE 0 OR lr_return_values->fieldval IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN er_event_data->m_data->* TO <itab>.

    CLEAR: ls_modi.
    ls_modi-row_id    = es_row_no-row_id.
    ls_modi-fieldname = 'DISP_NAME'.
    ls_modi-value     = lr_return_values->fieldval.
    APPEND ls_modi TO <itab>.

  ENDMETHOD.

*  method ev_handle_menu.
**        importing e_object e_ucomm
*
**    if e_ucomm = co_function_todel.
**      call method e_object->add_function( fcode = co_function_todel_a text = 'Setzen'(034) ).
**      call method e_object->add_function( fcode = co_function_todel_d text = 'Zurücknehmen'(035) ).
**    endif.
*
*  endmethod.

  METHOD ev_handle_toolbar.
*        importing
*          e_object

    DATA: ls_toolbar TYPE        stb_button
        , lr_toolbar TYPE REF TO stb_button
        .

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = co_function_roled.
    ls_toolbar-text      = 'Rolle'(018) ##TEXT_DIFF.
    ls_toolbar-quickinfo = 'Display role'(019) ##TEXT_DIFF.
    ls_toolbar-icon      = icon_display.
    INSERT ls_toolbar INTO e_object->mt_toolbar INDEX 1.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 3.
    INSERT ls_toolbar INTO e_object->mt_toolbar INDEX 1.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = co_function_export.
    ls_toolbar-quickinfo = 'Export'(020) ##TEXT_DIFF.
    ls_toolbar-icon      = icon_export.
    INSERT ls_toolbar INTO e_object->mt_toolbar INDEX 1.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = co_function_import.
    ls_toolbar-quickinfo = 'Import'(021) ##TEXT_DIFF.
    ls_toolbar-icon      = icon_import.
    INSERT ls_toolbar INTO e_object->mt_toolbar INDEX 1.

    " Replace Check
    READ TABLE e_object->mt_toolbar REFERENCE INTO lr_toolbar
      WITH KEY function = '&CHECK'.
    IF sy-subrc EQ 0.
      lr_toolbar->function = co_function_chk.
    ENDIF.

  ENDMETHOD.

  METHOD ev_user_command.
*        importing
*          e_ucomm

    CASE e_ucomm.
      WHEN co_function_import.
        CALL METHOD me->import( iv_refresh = 'X' ).
      WHEN co_function_export.
        CALL METHOD me->export( ).
      WHEN co_function_chk.
        CALL METHOD me->check( ).
      WHEN co_function_roled.
        me->display_role( ).
    ENDCASE.

  ENDMETHOD.

  METHOD export.

    DATA: lv_user_action      TYPE          i
        , lv_filename         TYPE          string
        , lv_path             TYPE          string
        , lv_fullpath         TYPE          string
        , lt_txt_file         TYPE          cl_pfcg_menu_tools=>tt_fiori_menu_file
        , lv_codepage         TYPE          abap_encoding
        .

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_extension         = 'TXT'
        "default_file_name         = ''
        with_encoding             = 'X'
      CHANGING
        filename                  = lv_filename
        path                      = lv_path
        fullpath                  = lv_fullpath
        user_action               = lv_user_action
        file_encoding             = lv_codepage
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
    IF lv_user_action NE 0.
      RETURN.
    ENDIF.


    lt_txt_file = mt_outtab.

    " Write CSV file: tab separated fields
    " - Codepage should be selected
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = lv_fullpath
        write_field_separator   = 'X'
        codepage                = lv_codepage
      CHANGING
        data_tab                = lt_txt_file
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    CLEAR: mv_data_changed.

  ENDMETHOD.

  METHOD f4_url_type_drdn.

    mr_alv_grid->set_drop_down_table( it_drop_down = mt_f4_url_type ).

  ENDMETHOD.


  METHOD get_fieldcat.
*        exporting et_fieldcat type lvc_t_fcat

    DATA: lr_fieldcat TYPE REF TO   lvc_s_fcat
        , ls_fieldcat TYPE          lvc_s_fcat
        .


    " Add Rolename
    CLEAR: ls_fieldcat.
    ls_fieldcat-fieldname = 'AGR_NAME'.
    ls_fieldcat-ref_field = 'AGR_NAME'.
    ls_fieldcat-ref_table = 'AGR_DEFINE'.
    APPEND ls_fieldcat TO et_fieldcat.

    " Add RoleText
    CLEAR: ls_fieldcat.
    ls_fieldcat-fieldname = 'ROLE_TEXT'.
    ls_fieldcat-ref_field = 'TEXT'.
    ls_fieldcat-ref_table = 'AGR_TEXTS'.
    APPEND ls_fieldcat TO et_fieldcat.

    " Add URLType
    CLEAR: ls_fieldcat.
    ls_fieldcat-fieldname = 'URL_TYPE'.
    ls_fieldcat-ref_field = 'URL_TYPE'.
    ls_fieldcat-ref_table = 'URL_EXITS'.
    APPEND ls_fieldcat TO et_fieldcat.

    " Add Fiori catalog or group name
    CLEAR: ls_fieldcat.
    ls_fieldcat-fieldname = 'DISP_NAME'.
    ls_fieldcat-ref_field = 'URL'.
    ls_fieldcat-ref_table = 'AGR_BUFFI'.
    "ls_fieldcat-rollname  = 'USOBXNAME'.
    APPEND ls_fieldcat TO et_fieldcat.

    " Add TypeText
    CLEAR: ls_fieldcat.
    ls_fieldcat-fieldname = 'URL_TYPE_TEXT'.
    ls_fieldcat-datatype  = 'CHAR'.
    ls_fieldcat-lowercase = 'X'.
    APPEND ls_fieldcat TO et_fieldcat.

    " Add Fiori catalog or group text
    CLEAR: ls_fieldcat.
    ls_fieldcat-fieldname = 'TEXT'.
    ls_fieldcat-ref_field = 'TEXT'.
    ls_fieldcat-ref_table = 'AGR_HIERT'.
    APPEND ls_fieldcat TO et_fieldcat.

    " Add URL
    CLEAR: ls_fieldcat.
    ls_fieldcat-fieldname = 'URL'.
    ls_fieldcat-ref_field = 'URL'.
    ls_fieldcat-ref_table = 'AGR_BUFFI'.
    APPEND ls_fieldcat TO et_fieldcat.

    LOOP AT et_fieldcat REFERENCE INTO lr_fieldcat.
      CASE lr_fieldcat->fieldname.
        WHEN 'AGR_NAME'.
          lr_fieldcat->col_pos    = 1.
          lr_fieldcat->edit       = 'X'.
          lr_fieldcat->f4availabl = 'X'.
        WHEN 'ROLE_TEXT'.
          lr_fieldcat->col_pos    = 2.
          lr_fieldcat->edit       = 'X'.
          lr_fieldcat->outputlen  = 20.
        WHEN 'URL_TYPE'.
          lr_fieldcat->col_pos    = 10.
          lr_fieldcat->edit       = 'X'.
          lr_fieldcat->drdn_hndl  = '1'.
          lr_fieldcat->checktable = '!'.        "do not check foreign keys
          lr_fieldcat->scrtext_s  = 'Type'(022) ##TEXT_DIFF.
          lr_fieldcat->scrtext_m  = 'Type'(022) ##TEXT_DIFF.
          lr_fieldcat->scrtext_l  = 'Type of menu entry'(023) ##TEXT_DIFF.
          lr_fieldcat->reptext    = 'Type of menu entry'(023) ##TEXT_DIFF.
        WHEN 'DISP_NAME'.
          lr_fieldcat->col_pos    = 12.
          lr_fieldcat->edit       = 'X'.
          lr_fieldcat->outputlen  = 20.
          lr_fieldcat->f4availabl = 'X'.
          lr_fieldcat->scrtext_s  = 'Name'(024) ##TEXT_DIFF.
          lr_fieldcat->scrtext_m  = 'Name'(024) ##TEXT_DIFF.
          lr_fieldcat->scrtext_l  = 'Name of menu entry'(025) ##TEXT_DIFF.
          lr_fieldcat->reptext    = 'Name of menu entry'(025) ##TEXT_DIFF.
        WHEN 'URL_TYPE_TEXT'.
          lr_fieldcat->col_pos    = 13.
          lr_fieldcat->outputlen  = 15.
          lr_fieldcat->scrtext_s  = 'Type'(022) ##TEXT_DIFF.
          lr_fieldcat->scrtext_m  = 'Type'(022) ##TEXT_DIFF.
          lr_fieldcat->scrtext_l  = 'Type of menu entry'(023) ##TEXT_DIFF.
          lr_fieldcat->reptext    = 'Type of menu entry'(023) ##TEXT_DIFF.
        WHEN 'TEXT'.
          lr_fieldcat->col_pos    = 15.
          lr_fieldcat->outputlen  = 20.
        WHEN 'URL'.
          lr_fieldcat->col_pos    = 20.
          lr_fieldcat->outputlen  = 20.
          lr_fieldcat->no_out     = 'X'.
        WHEN OTHERS.
          lr_fieldcat->tech       = 'X'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD import.

    DATA: lt_file             TYPE          filetable
        , ls_file             TYPE          file_table
        , lv_rc               TYPE          i
        , lv_user_action      TYPE          i
        , lv_filename         TYPE          string
        , lt_fiori_menu       TYPE          tt_fiori_menu
        , lr_fiori_menu       TYPE REF TO   ty_fiori_menu
        , lt_txt_file         TYPE          cl_pfcg_menu_tools=>tt_fiori_menu_file
        , lv_codepage         TYPE          abap_encoding
        .

    IF iv_fullpath IS INITIAL.
      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          default_extension       = 'TXT'
          with_encoding           = 'X'
          multiselection          = abap_false
        CHANGING
          file_table              = lt_file
          rc                      = lv_rc
          user_action             = lv_user_action
          file_encoding           = lv_codepage
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.

      IF lv_user_action NE 0.
        RETURN.
      ENDIF.

      READ TABLE lt_file INTO ls_file INDEX 1.
      IF sy-subrc NE 0 OR ls_file-filename IS INITIAL.
        RETURN.
      ENDIF.
      lv_filename = ls_file-filename.
    ELSE.
      lv_filename = iv_fullpath.
      lv_codepage = iv_codepage.
    ENDIF.

    " Read CSV file: tab separated fields
    " - Codepage should be selected
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = lv_filename
        has_field_separator     = 'X'
        codepage                = lv_codepage
      CHANGING
        data_tab                = lt_txt_file
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " Convert
    lt_fiori_menu = lt_txt_file.
    LOOP AT lt_fiori_menu REFERENCE INTO lr_fiori_menu.
      TRANSLATE lr_fiori_menu->agr_name TO UPPER CASE.
    ENDLOOP.

    CALL METHOD me->set_outtab( EXPORTING iv_append = 'X' CHANGING ct_fiori_menu = lt_fiori_menu ).

    IF iv_refresh EQ 'X'.
      CALL METHOD me->refresh_alv( ).
    ENDIF.

  ENDMETHOD.

  METHOD refresh_alv.

    DATA: ls_stable     TYPE          lvc_s_stbl
        .

    mr_alv_grid->refresh_table_display( EXPORTING is_stable = ls_stable i_soft_refresh = 'X' ).

  ENDMETHOD.

  METHOD save.

    DATA: lr_fiori_menu     TYPE REF TO   ty_fiori_menu
        , lt_menu_4_role    TYPE          tt_fiori_menu
        , lv_title          TYPE          agr_title
        , lv_error          TYPE          char01
        , lv_abort          TYPE          char01
        , lt_messages       TYPE          cl_pfcg_menu_tools=>tt_pfcg_msg_log
        , lt_fiori_menu     TYPE          tt_fiori_menu
        .


    " Check also imported file
    IF me->check( ) EQ 'X'.
      RETURN.
    ENDIF.

    lt_fiori_menu = mt_outtab.
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

    CLEAR: mv_data_changed.

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
      IF me->check_url_type( lr_fiori_menu->url_type ) EQ 'X'.
        lr_fiori_menu->url_type_text = cl_pfcg_menu_tools=>url_type_get_text( iv_url_type = lr_fiori_menu->url_type ).
      ENDIF.

      CALL METHOD me->check_appl
        EXPORTING
          iv_url_type  = lr_fiori_menu->url_type
          iv_disp_name = lr_fiori_menu->disp_name
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

  ENDMETHOD.
*
ENDCLASS.


TYPES:
    BEGIN OF ty_fiori_menu.
    INCLUDE: TYPE cl_pfcg_menu_tools=>ty_fiori_menu_file.
TYPES: url_type_text TYPE agr_title
         , text          TYPE agr_hiert-text
         , url           TYPE agr_buffi-url
      , END OF ty_fiori_menu .
 TYPES:
    tt_fiori_menu TYPE STANDARD TABLE OF ty_fiori_menu .

 DATA:
    id_agr_name  TYPE agr_name ,
    id_title     TYPE agr_title,
    id_role_head TYPE agr_title.
 DATA:
    lv_agr_name TYPE agr_define-agr_name, "Role Name
    lv_role_text TYPE agr_texts-text,     "Short Role Description
    lv_url_type TYPE url_exits-url_type,  "Type of Menu Entry
    lv_disp_name TYPE agr_buffi-url.      "Name of the Menu Entry

SELECTION-SCREEN BEGIN OF BLOCK bk1  WITH FRAME TITLE TEXT-001.
PARAMETERS: p_agname TYPE agr_name DEFAULT 'ZOSWIN_TEST04' ,   "自动打包下载
            P_title TYPE agr_title DEFAULT 'title04',
            p_head TYPE agr_title DEFAULT 'Create by LCDP Platform'.

SELECT-OPTIONS: s_user FOR bapibname-bapibname.
SELECTION-SCREEN END OF BLOCK bk1 .




START-OF-SELECTION.
*  id_agr_name = p_agname.
*  id_title = P_title.
*  id_role_head = p_head.

*&*********创建单一角色*********&*
*  PERFORM create_single_role USING id_agr_name       "角色名称
*                              id_title        "描述
*                                id_role_head. "长文本

*&*********分配磁贴目录和组*********&*
  PERFORM assign_catalog_group USING lv_agr_name  "Role Name
                                      lv_role_text  "Short Role Description
                                        lv_url_type   "Type of Menu Entry
                                         lv_disp_name.  "Name of the Menu Entry

*&*********角色分配给用户*********&*
  BREAK-POINT.

  PERFORM assign_user TABLES s_user[]
                            USING id_agr_name.
*&---------------------------------------------------------------------*
*& Form create_role
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ID_AGR_NAME
*&      --> ID_TITLE
*&      --> ID_ROLE_HEAD
*&---------------------------------------------------------------------*
FORM create_single_role  USING p_id_agr_name
                          p_id_title
                          p_id_role_head.

 TYPES:
    tt_agr_texts TYPE STANDARD TABLE OF agr_texts WITH KEY agr_name .
 DATA:
    ld_new        TYPE c LENGTH 1,
    ls_agr_define TYPE agr_define,
    lr_agr_texts  TYPE REF TO tt_agr_texts,
    ld_msg_tx     TYPE string.
 DATA:
    n_agr_texts LIKE agr_texts OCCURS 0 WITH HEADER LINE.
 DATA:
    ed_result TYPE sy-subrc.

  CLEAR ed_result.
  "判断角色是否存在
  SELECT SINGLE agr_name FROM agr_define INTO ld_new
    WHERE agr_name = p_id_agr_name.
  IF sy-subrc = 0.
    CLEAR ld_new.
  ELSE.
    ld_new = abap_true.
  ENDIF.

* Authorization check for role creation and maintenance
  CALL FUNCTION 'PRGN_AUTH_ACTIVITY_GROUP'
    EXPORTING
      activity_group  = p_id_agr_name
      action_create   = ld_new
      action_change   = 'X'
      action_generate = 'X'
      message_output  = space
    EXCEPTIONS
      not_authorized  = 1
      OTHERS          = 2.
  IF sy-subrc NE 0.
    "没有创建角色的权限
    MESSAGE i420(s#) WITH p_id_agr_name INTO ld_msg_tx.
    ed_result = 1.
    RETURN.
  ENDIF.

  CALL FUNCTION 'PRGN_ACTIVITY_GROUP_ENQUEUE'
    EXPORTING
      activity_group = p_id_agr_name
    EXCEPTIONS
      foreign_lock   = 1
      OTHERS         = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO ld_msg_tx.
    ed_result = 3.
    RETURN.
  ENDIF.

  IF ld_new = abap_true.
    ls_agr_define-mandt      = sy-mandt.
    ls_agr_define-agr_name   = p_id_agr_name.
    ls_agr_define-create_dat = sy-datum.
    ls_agr_define-create_tim = sy-uzeit.
    ls_agr_define-create_usr = sy-uname.
    "角色定义表
    INSERT agr_define FROM ls_agr_define.

    CALL FUNCTION 'PRGN_CHANGEDOCUMENT_WRITE'
      EXPORTING
        activity_group = p_id_agr_name
        upd_agr_define = 'I'
        n_agr_define   = ls_agr_define.

    CALL FUNCTION 'PRGN_SET_COLLECTIVE_AGR_FLAG'
      EXPORTING
        activity_group      = p_id_agr_name
        collective_agr_flag = space.

    "Other attributes (except package), in particular original language
    CALL FUNCTION 'PRGN_SET_AGR_ATTRIBUTES'
      EXPORTING
        activity_group    = p_id_agr_name
        master_language   = sy-langu
        responsible_user  = sy-uname
        development_class = space.
  ENDIF.

  BREAK-POINT.
*  lr_agr_texts = create_role_header_tx( id_agr_name  = p_id_agr_name
*                                        id_title     = p_id_title
*                                        id_role_head = p_id_role_head ).

*  PERFORM create_role_header_tX USING p_id_agr_name p_id_title p_id_role_head lr_agr_texts.
  REFRESH: n_agr_texts.
  IF p_id_title IS NOT INITIAL.
    CLEAR n_agr_texts.
    n_agr_texts-mandt    = sy-mandt.
    n_agr_texts-agr_name = p_id_agr_name.
    n_agr_texts-spras    = sy-langu.
    n_agr_texts-line     = 0.
    n_agr_texts-text     = p_id_title.
    APPEND n_agr_texts.
  ENDIF.

  IF p_id_role_head IS NOT INITIAL.
   CLEAR n_agr_texts.
   n_agr_texts-mandt    = sy-mandt.
   n_agr_texts-agr_name = p_id_agr_name.
   n_agr_texts-spras    = sy-langu.
   n_agr_texts-line     = 1.
   n_agr_texts-text     = p_id_role_head.
   APPEND n_agr_texts.
  ENDIF.

  IF ld_new = abap_true.
*    INSERT agr_texts FROM TABLE lr_agr_texts->*.
    INSERT agr_texts FROM TABLE n_agr_texts.
  ELSE.
    DELETE FROM agr_texts WHERE agr_name = p_id_agr_name.
    CALL FUNCTION 'DB_COMMIT'.
*    INSERT agr_texts FROM TABLE lr_agr_texts->*.
    INSERT agr_texts FROM TABLE n_agr_texts.
  ENDIF.

  CALL FUNCTION 'PRGN_CHANGEDOCUMENT_WRITE'
    EXPORTING
      activity_group = p_id_agr_name
      upd_agr_texts  = 'I'
    TABLES
      n_agr_texts = n_agr_texts.
*      n_agr_texts    = lr_agr_texts->*.

  CALL FUNCTION 'PRGN_ACTIVITY_GROUP_DEQUEUE'
    EXPORTING
      activity_group = p_id_agr_name.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form assigned_to_user
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ID_AGR_NAME
*&      --> S_USER
*&---------------------------------------------------------------------*
FORM assign_user TABLES p_s_user
                        USING  p_id_agr_name.

  CHECK p_s_user[] IS NOT INITIAL.

  SELECT bname
    FROM usr02
    INTO TABLE @DATA(it_bname)
   WHERE bname IN @p_s_user.

  SELECT COUNT(*)
*    INTO TABLE @DATA(it_agr)
    FROM agr_define
   WHERE agr_name = p_id_agr_name.

*   SELECT bname
*    INTO TABLE it_bname
*    FROM usr02
*   WHERE bname IN s_user.
*
*  IF sy-subrc <> 0.
*    MESSAGE s001(00) WITH '输入的用户名在系统中不存在！' DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
*  SELECT agr_name
*    INTO TABLE it_agr
*    FROM agr_define
*   WHERE agr_name IN s_agr.
*
*  IF sy-subrc <> 0.
*    MESSAGE s001(00) WITH '输入的角色在系统中不存在！' DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

  DATA:
    wa_activity     TYPE bapiagr,
    wa_return       TYPE bapiret2,
*    wa_out          TYPE typ_out,
    it_new_activity TYPE TABLE OF bapiagr,
    it_old_activity TYPE TABLE OF bapiagr,
    it_return       TYPE TABLE OF bapiret2.



  REFRESH: it_return.
  LOOP AT it_bname ASSIGNING FIELD-SYMBOL(<fs_bname>).

    CLEAR wa_activity.
    wa_activity-agr_name = p_id_agr_name.
    wa_activity-from_dat = sy-datum.
    wa_activity-to_dat   = '99991231'.
    APPEND wa_activity TO it_new_activity.

    " 获取用户当前权限
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username       = <fs_bname>-bname
      TABLES
        activitygroups = it_old_activity
        return         = it_return.

    APPEND LINES OF it_new_activity TO it_old_activity.

    " 分配权限
    CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
      EXPORTING
        username       = <fs_bname>-bname
      TABLES
        activitygroups = it_old_activity
        return         = it_return.

    REFRESH: it_new_activity,it_old_activity.
  ENDLOOP.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<fs_return>) WHERE type <> 'S' AND type <> 'W'.
*      wa_out-message = <fs_return>-message.
*      wa_out-type    = <fs_return>-type.
*      EXIT.
    ENDLOOP.

    IF sy-subrc <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

*      wa_out-type = 'S'.
*      wa_out-message = <fs_bname>-bname && '权限分配成功！'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form assign_catalog_group
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ID_CATALOG
*&      --> ID_GROUP
*&---------------------------------------------------------------------*
FORM assign_catalog_group  USING  pv_agr_name  "Role Name
                                      pv_role_text  "Short Role Description
                                        pv_url_type   "Type of Menu Entry
                                         pv_disp_name.  "Name of the Menu Entry


  DATA: lr_fiori_menu       TYPE REF TO   ty_fiori_menu
        , lt_menu_4_role    TYPE          tt_fiori_menu
        , lv_title          TYPE          agr_title
        , lv_error          TYPE          char01
        , lv_abort          TYPE          char01
        , lt_messages       TYPE          cl_pfcg_menu_tools=>tt_pfcg_msg_log
        , lt_fiori_menu     TYPE          tt_fiori_menu
        .
   DATA: mt_outtab             TYPE          tt_fiori_menu,
         mv_delete_menu        TYPE          char01,
         mv_data_changed       TYPE          char01,
         mv_create_role        TYPE          char01.

*   X-SAP-UI2-CATALOGPAGE:zxuwp_test?DEST_FES=&ROLE_ORIGIN=HE4CLNT400&AUTH_DEFAULTS=X

  DATA: ct_fiori_menu TYPE tt_fiori_menu.
  DATA: type TYPE char01.

*  PERFORM set_outtab changing  ct_fiori_menu type tt_fiori_menu.

   mt_outtab = VALUE #( ( agr_name  = pv_agr_name  "ZOSWIN_TEST1
                          role_text = pv_role_text "测试
                          url_type  = pv_url_type  "CAT_PROVIDER    GROUP_PROVIDER
                          disp_name = pv_disp_name
*                          url_type_text =
*                          text =
                          url = 'X-SAP-UI2-CATALOGPAGE:' && pv_disp_name && '?DEST_FES=&ROLE_ORIGIN=HE4CLNT400&AUTH_DEFAULTS=X'
                           )
                      ).

*    data: lr_fiori_menu type ref to ty_fiori_menu.
    LOOP AT mt_outtab REFERENCE INTO lr_fiori_menu.
*      IF me->check_url_type( lr_fiori_menu->url_type ) EQ 'X'.
        lr_fiori_menu->url_type_text = cl_pfcg_menu_tools=>url_type_get_text( iv_url_type = lr_fiori_menu->url_type ).
*      ENDIF.

*      CALL METHOD me->check_appl
*        EXPORTING
*          iv_url_type  = lr_fiori_menu->url_type
*          iv_disp_name = lr_fiori_menu->disp_name
*        IMPORTING
*          ev_text      = lr_fiori_menu->text.

*      IF lr_fiori_menu->text IS NOT INITIAL.
*        CALL METHOD me->create_url
*          EXPORTING
*            iv_url_type  = lr_fiori_menu->url_type
*            iv_disp_name = lr_fiori_menu->disp_name
*          IMPORTING
*            ev_url       = lr_fiori_menu->url.
*      ENDIF.
    ENDLOOP.


    " Check also imported file
*    if me->check( ) eq 'X'.
*      return.
*    endif.

*    lt_fiori_menu = mt_outtab.
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
*        if mv_genprof eq 'X'.
*          perform create_auth using lr_fiori_menu->agr_name changing lt_messages lv_error lv_abort.
*        endif.
*        if lv_abort eq 'X'.
*          exit.
*        endif.

      ENDAT.

    ENDLOOP.

    IF lt_messages IS NOT INITIAL.
      CALL METHOD cl_pfcg_menu_tools=>message_log_display
        EXPORTING
          iv_popup    = 'X'
        CHANGING
          ct_messages = lt_messages.
    ENDIF.

    CLEAR: mv_data_changed.

  BREAK-POINT.

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
*
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
*                               it_menu_asg  type lcl_fiori_menu=>tt_fiori_menu
                               it_menu_asg  TYPE tt_fiori_menu
                               iv_del_menu  TYPE char01
                      CHANGING ct_messages  TYPE cl_pfcg_menu_tools=>tt_pfcg_msg_log
                               ev_error     TYPE char01
                               ev_abort     TYPE char01.

  DATA: lr_role         TYPE REF TO   cl_pfcg_menu_modify
      , lt_return       TYPE          bapirettab
*      , lr_fiori_menu   type ref to   lcl_fiori_menu=>ty_fiori_menu
      , lr_fiori_menu   TYPE REF TO   ty_fiori_menu
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
*form exit_popup using    iv_question type c
*                changing ev_answer   type char01.
*
*  data: lv_title(100)         type c
*      , lv_answer             type char01
*      .
*
*  lv_title = 'Changes will be lost'(026) ##TEXT_DIFF.
*
*  call function 'POPUP_TO_CONFIRM'
*    exporting
*      titlebar      = lv_title
*      text_question = iv_question
*    importing
*      answer        = lv_answer
*    exceptions
*      others        = 0.
*  case lv_answer.
*    when '1'.
*      ev_answer = 'Y'.
*    when '2'.
*      ev_answer = 'N'.
*    when others.
*      ev_answer = 'C'.
*  endcase.
*
*endform.
*&---------------------------------------------------------------------*
*& Form set_outtab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FIORI_MENU
*&      <-- TYPE
*&      <-- TT_FIORI_MENU
*&---------------------------------------------------------------------*
FORM set_outtab  CHANGING p_ct_fiori_menu
                          p_type
                          p_tt_fiori_menu.

ENDFORM.
