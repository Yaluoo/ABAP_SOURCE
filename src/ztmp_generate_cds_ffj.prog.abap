*&---------------------------------------------------------------------*
*& Report ZTMP_GENERATE_CDS_WC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztmp_generate_cds_ffj.

DATA:BEGIN OF gs_cds,
       cds_name         TYPE char30,
       label            TYPE string,
       view_name        TYPE char30,
       association_type TYPE char100,
     END OF gs_cds.

DATA:BEGIN OF gs_ztdatafields.
       INCLUDE TYPE ztdatafields.
DATA:  tabname TYPE tabname16,
     END OF gs_ztdatafields.
DATA:gt_ztdatafields LIKE TABLE OF  gs_ztdatafields.

DATA:BEGIN OF gs_ztfieldgroup.
       INCLUDE TYPE ztfieldgroup.
DATA:  tabname TYPE tabname16,
     END OF gs_ztfieldgroup.

DATA:gt_ztfieldgroup LIKE TABLE OF  gs_ztfieldgroup.
DATA:gt_cds_level TYPE TABLE OF ztcdslevel.
DATA:gt_cds_process TYPE TABLE OF ztcdspross.
DATA:gt_tab_code TYPE cl_cmp_composer=>tab_code,
     gs_view     TYPE zif_aps_cke_comm_types=>ts_view,
     gs_objmodel TYPE zif_aps_cke_comm_types=>ts_view-s_root-s_objectmodel,
     go_composer TYPE REF TO cl_cmp_composer.

DATA:gt_cdsassoc TYPE TABLE OF ztcdsassoc.

TYPES: BEGIN OF ty_ddl_name_source,
         name   TYPE ddlname,
         source TYPE ddddlsource,
         ddtext TYPE ddtext,
       END OF ty_ddl_name_source.
TYPES tt_ddl_name_source TYPE STANDARD TABLE OF ty_ddl_name_source WITH KEY name.
DATA: lt_names TYPE if_ddic_wb_ddls_secobj_manager=>ty_ddls_names.
DATA: it_ddl_source TYPE tt_ddl_name_source.
DATA: is_ddl_source TYPE ty_ddl_name_source.
DATA: lt_ddl_source TYPE TABLE OF ddddlsrcv.
DATA: lt_return TYPE TABLE OF bapiret2,
      ls_return TYPE bapiret2.
DATA: mo_ddl_handler TYPE REF TO if_wb_ddl_handler,
      mo_tr_adapter  TYPE REF TO if_cbo_transport_adapter.
DATA:gv_source TYPE string.
DATA: iv_package TYPE devclass VALUE '$TMP'.
DATA: iv_request  TYPE trkorr.
DATA: iv_state TYPE objstate VALUE 'N'.
*------获取DB数据------*
PERFORM frm_get_dbdata TABLES gt_ztfieldgroup
                              gt_ztdatafields .
* 获取CDS层级
PERFORM frm_get_cds_level TABLES gt_cds_level.

* 获取CDS执行过程
PERFORM frm_get_cds_process TABLES gt_cds_process.

PERFORM frm_create_hierarchy_cds.

PERFORM frm_update_db.

BREAK-POINT.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DBdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_dbdata TABLES pt_ztfieldgroup  LIKE gt_ztfieldgroup
                            pt_ztdatafields LIKE gt_ztdatafields .
  DATA:ls_ztfieldgroup  LIKE LINE OF pt_ztfieldgroup.
  DATA:ls_ztdatafields  LIKE LINE OF pt_ztdatafields.
  "获取组信息
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE pt_ztfieldgroup
    FROM ztfieldgroup.
  IF sy-subrc = 0.
    "获取数据字段表信息
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE pt_ztdatafields
      FROM ztdatafields
       FOR ALL ENTRIES IN pt_ztfieldgroup
     WHERE fieldgroup = pt_ztfieldgroup-fieldgroup.

    LOOP AT pt_ztfieldgroup INTO ls_ztfieldgroup.
      CASE ls_ztfieldgroup-fieldgroup.
        WHEN 'ZGROUP1'.
          ls_ztfieldgroup-tabname = 'ZTEKKO'.
        WHEN 'ZGROUP2'.
          ls_ztfieldgroup-tabname = 'ZTEKPO'.
        WHEN 'ZGROUP3'.
          ls_ztfieldgroup-tabname = 'ZTKONP'.
        WHEN OTHERS.
      ENDCASE.
      MODIFY pt_ztfieldgroup FROM ls_ztfieldgroup.
    ENDLOOP.

    LOOP AT pt_ztdatafields INTO ls_ztdatafields.
      CASE ls_ztdatafields-fieldgroup.
        WHEN 'ZGROUP1'.
          ls_ztdatafields-tabname = 'ZTEKKO'.
        WHEN 'ZGROUP2'.
          ls_ztdatafields-tabname = 'ZTEKPO'.
        WHEN 'ZGROUP3'.
          ls_ztdatafields-tabname = 'ZTKONP'.
        WHEN OTHERS.
      ENDCASE.
      MODIFY pt_ztdatafields FROM ls_ztdatafields.
    ENDLOOP.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_cds_view
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_ZTFIELDGROUP
*&      --> GT_ZTDATAFIELDS
*&      <-- GS_VIEW
*&---------------------------------------------------------------------*
FORM frm_get_cds_view  USING fu_ztfieldgroup LIKE gs_ztfieldgroup
                              fs_cds LIKE gs_cds
                              fs_datasource STRUCTURE zsdatasource
                         CHANGING p_view LIKE gs_view.

  DATA:lt_fields LIKE TABLE OF gs_ztdatafields.

  CLEAR:gs_objmodel.

  LOOP AT gt_ztdatafields INTO DATA(ls_field) WHERE fieldgroup = fu_ztfieldgroup-fieldgroup.
    APPEND ls_field TO lt_fields.
*    IF ls_field-techkey = abap_true.
*      IF gs_objmodel-semanticKey IS INITIAL.
*        gs_objmodel-semanticKey = |'| && ls_field-fieldname &&  |'|.
*      ELSE.
*        gs_objmodel-semanticKey = gs_objmodel-semanticKEY && |, '| && ls_field-fieldname &&  |'|.
*      ENDIF.
*    ENDIF.

    IF ls_field-buskeyflag = abap_true.
      IF gs_objmodel-semanticKey IS INITIAL.
        gs_objmodel-semanticKey = |'| && ls_field-fieldname &&  |'|.
      ELSE.
        gs_objmodel-semanticKey = gs_objmodel-semanticKEY && |, '| && ls_field-fieldname &&  |'|.
      ENDIF.
    ENDIF.
  ENDLOOP.

*------抬头信息------*
  PERFORM frm_get_root  USING fs_cds gs_objmodel CHANGING  p_view-s_root.

*------主表信息------*
  PERFORM frm_get_data_soure USING fs_datasource
                              CHANGING  p_view-t_primary_data_source.

*------字段名信息------*
  PERFORM frm_get_field TABLES lt_fields
                        CHANGING  p_view-t_field.
*------parameter------*
  PERFORM frm_get_parameter TABLES lt_fields
                            USING fu_ztfieldgroup
                            CHANGING  p_view-t_parameter.

*------获取CDS SOURCE------*
  PERFORM frm_get_source  USING p_view
                          CHANGING gt_tab_code[] .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_root
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_CDS
*&      <-- P_VIEW_S_ROOT
*&---------------------------------------------------------------------*
FORM frm_get_root  USING fs_cds  LIKE gs_cds
                         fs_objmodel LIKE gs_objmodel
                   CHANGING p_root TYPE zif_aps_cke_comm_types=>ts_view-s_root.

  p_root = VALUE #(
                 cds_name                = fs_cds-view_name
                 ignore_propagated_annos = abap_true
                 label = gs_cds-label
                 view_name = fs_cds-cds_name
                 association_type  = fs_cds-association_type
                 s_exposure = VALUE #( as_analytical_view_ind = abap_false as_service_ind = abap_true )
                 s_access_control = VALUE #( authorization_check = '#NOT_REQUIRED' )
                 s_objectmodel = VALUE #( semanticKey = fs_objmodel-semanticKey )
               ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_data_soure
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_ZTFIELDGROUP
*&      --> PT_ZTDATAFIELDS
*&      <-- P_VIEW_T_PRIMARY_DATA_SOURCE
*&---------------------------------------------------------------------*
FORM frm_get_data_soure USING fs_datasource STRUCTURE zsdatasource
                         CHANGING p_data_source TYPE zif_aps_cke_comm_types=>ts_view-t_primary_data_source.

  p_data_source  =  VALUE #( ( name = fs_DATASOURCE-tabname
                               alias =  fs_DATASOURCE-alias ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_ZTFIELDGROUP
*&      --> PT_ZTDATAFIELDS
*&      <-- P_VIEW_T_FIELD
*&---------------------------------------------------------------------*
FORM frm_get_field  TABLES  pt_ztdatafields STRUCTURE gs_ztdatafields
                    CHANGING p_t_field TYPE zif_aps_cke_comm_types=>ts_view-t_field.

  p_t_field = VALUE #( FOR <fs>  IN  pt_ztdatafields
                        ( alias             = <fs>-fieldname
                          field_type_c      = <fs>-field_type
                          data_source_field = VALUE #( data_source_alias = <fs>-fieldgroup
                                                       field_name        =  <fs>-fieldname  )
                                                       label             = VALUE #( value = <fs>-fieldname  ) ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_parameter
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_ZTFIELDGROUP
*&      --> PT_ZTDATAFIELDS
*&      <-- P_VIEW_T_PARAMETER
*&---------------------------------------------------------------------*
FORM frm_get_parameter  TABLES pt_ztdatafields STRUCTURE gs_ztdatafields
                        USING fu_ztfieldgroup  LIKE gs_ztfieldgroup
                        CHANGING p_parameter  TYPE zif_aps_cke_comm_types=>ts_view-t_parameter.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_association
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_ZTFIELDGROUP
*&      --> PT_ZTDATAFIELDS
*&      <-- P_VIEW_T_ASSOCIATION
*&---------------------------------------------------------------------*
FORM frm_create_association TABLES FT_cdsassoc STRUCTURE ztcdsassoc
                            CHANGING p_association TYPE zif_aps_cke_comm_types=>ts_view-t_association.

  DATA:ls_association TYPE  if_aps_cke_comm_types=>ts_association.

  LOOP AT FT_cdsassoc ASSIGNING FIELD-SYMBOL(<lfs_assoc>)
                      GROUP BY ( basic_alias = <lfs_assoc>-basic_alias
                                 target_alias = <lfs_assoc>-targeT_alias )
                      INTO DATA(lt_group).

    LOOP AT GROUP lt_group ASSIGNING FIELD-SYMBOL(<lfs_group>).
      ls_association-s_target_data_source = VALUE #( alias = <lfs_group>-TARGET_alias  name = <lfs_group>-TARGET_t_name  ).
      ls_association-s_cardinality = VALUE #( min = 0 max = 100 ).

      ls_association-t_on_condition = VALUE #( BASE ls_association-t_on_condition
                                     ( lhs_field = VALUE #( Data_source_alias = <lfs_group>-TARGET_alias
                                                            field_name        = <lfs_group>-TARGET_f_name )
                                       operator = '='
                                       rhs_field = VALUE #( data_source_alias = <lfs_group>-basic_alias
                                                            field_name        = <lfs_group>-BASIC_f_name )
                                       rhs_type = 'F' ) ).
    ENDLOOP.

    APPEND ls_association TO p_association.
    CLEAR ls_association.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_source
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_TAB_CODE[]
*&      --> GS_VIEW
*&---------------------------------------------------------------------*
FORM frm_get_source  USING    p_view      TYPE zif_aps_cke_comm_types=>ts_view
                     CHANGING p_tab_code  TYPE cl_cmp_composer=>tab_code.

  DATA: lo_composer TYPE REF TO cl_cmp_composer,
        lt_tab_code TYPE cl_cmp_composer=>tab_code.
  lo_composer = cl_cmp_composer=>s_create( ).
  lo_composer->add_var( i_name = 'view' i_value = p_view ).

  TRY.
      p_tab_code = lo_composer->build_code( i_template_name = 'Z_MB_CUST_CDS_VIEW' ).
    CATCH cx_cmp_failure cx_cmp_invalid_param  INTO DATA(lx_cmp).
      RAISE EXCEPTION TYPE cx_cbo_cds_generation
        EXPORTING
          previous = lx_cmp.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_HIERARCHY_CDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_create_hierarchy_cds.

  DATA:lt_process TYPE STANDARD TABLE OF  ztcdspross.

  SORT gt_cds_process BY cds_level.


  LOOP AT gt_cds_level ASSIGNING FIELD-SYMBOL(<Lfs_level>).

    AT NEW cds_level.
      CLEAR:lt_process.
    ENDAT.

    CLEAR:it_ddl_source,lt_return.
    LOOP AT gt_cds_process ASSIGNING FIELD-SYMBOL(<Lfs_process>) WHERE cds_level = <lfs_level>-cds_level.

      IF <Lfs_process>-view_name IS INITIAL.
        <Lfs_process>-sql_view_name  = CONV char30( 'ZCDS_' && <lfs_level>-cds_level && '_' && <lfs_process>-table_name ).
        <Lfs_process>-view_name = CONV char30( 'ZZV_' && <lfs_level>-cds_level && '_' && <lfs_process>-table_name ).
      ENDIF.

      IF <lfs_process>-view_status = 'X'.
        APPEND <lfs_process> TO lt_process.
      ELSE.
        CLEAR:gs_cds,
              gs_view.

        READ TABLE gt_ztfieldgroup INTO DATA(LS_current) WITH KEY tabname = <lfs_process>-table_name.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.

        "获取本次使用到的层级的CDS信息
        IF <LFS_level>-use_level IS NOT INITIAL.
          READ TABLE gt_cds_process INTO DATA(ls_process_use) WITH KEY table_name = <lfs_process>-table_name
                                                                       cds_level  = <lfs_level>-use_level.
          IF sy-subrc EQ 0.
            DATA(ls_datasource) = VALUE zsdatasource( tabname = ls_process_use-view_name
                                                      alias   = ls_current-fieldgroup ).
          ENDIF.
        ELSE.
          ls_datasource = VALUE #( tabname = ls_current-tabname
                                   alias   = ls_current-fieldgroup ).
        ENDIF.


*------创建CDS名字数据-----*
        gs_cds  = VALUE #( cds_name = <lfs_process>-sql_view_name
                           label    = 'Po Order'
                           view_name = <lfs_process>-view_name ).

        PERFORM frm_get_cds_view USING ls_current
                                      gs_cds
                                      ls_datasource
                              CHANGING gs_view.

*    TRY.
        CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
          RECEIVING
            uuid = <Lfs_process>-viewuuid.
*      CATCH cx_uuid_error.
*    ENDTRY.

        "调用激活视图
        CLEAR:gv_source.
        LOOP AT gt_tab_code INTO DATA(ls_code).
          gv_source = gv_source && | | && ls_code.
        ENDLOOP.

        is_ddl_source-name = <lfs_process>-view_name.
        is_ddl_source-source = gv_source.
        is_ddl_source-ddtext = 'text'.
        APPEND is_ddl_source TO it_ddl_source.
        CLEAR is_ddl_source.
*        it_ddl_source = VALUE #( ( name = <lfs_process>-view_name     "ddl name
*                           source = gv_source    "ddl source
*                           ddtext = 'text' ) ).  "ddl text

        <lfs_process>-view_status = 'X'.
        <lfs_process>-message     = 'Create Successfully!'.
        APPEND <lfs_process> TO lt_process.
      ENDIF.
    ENDLOOP.

* &*********Conversion*********&*
    PERFORM get_ddl_source TABLES it_ddl_source lt_ddl_source lt_names.
* &*********Generates an interface variable*********&*
    mo_ddl_handler = cl_wb_ddl_handler_factory=>create( ).
    mo_tr_adapter  = cl_cbo_tr_adapter_factory=>get_ddls_adapter( ).

*    iv_source_origin = ''.
    iv_package = 'Z001'.
    iv_request = 'YESK900696'.

    PERFORM mass_create.
    cl_demo_output=>display( lt_return ).

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    AT END OF cds_level.
      IF <lfs_level>-zassoc = 'X'.                "Create Association
        PERFORM frm_create_association_view TABLES lt_process
                                            USING <lfs_level>-cds_level.
      ENDIF.
    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_cds_level
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_CDS_LEVEL
*&---------------------------------------------------------------------*
FORM frm_get_cds_level  TABLES pt_cds_level LIKE gt_cds_level.
  SELECT *
    INTO TABLE pt_cds_level
    FROM ztcdslevel.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_cds_process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_CDS_PROCESS
*&---------------------------------------------------------------------*
FORM frm_get_cds_process  TABLES pt_cds_process LIKE gt_cds_process.
  IF gt_ztfieldgroup IS NOT INITIAL.
    SELECT *
      INTO TABLE pt_cds_process
      FROM ztcdspross
      FOR ALL ENTRIES IN gt_ztfieldgroup
     WHERE table_name = gt_ztfieldgroup-tabname.
  ENDIF.

  LOOP AT gt_cds_level ASSIGNING FIELD-SYMBOL(<lfs_level>).

    LOOP AT gt_ztfieldgroup ASSIGNING FIELD-SYMBOL(<lfs_group>).

      READ TABLE pt_cds_process WITH KEY table_name = <lfs_group>-tabname
                                         cds_level  = <lfs_level>-cds_level
                                         TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        pt_cds_process[] = VALUE #( BASE pt_cds_process[] (
                          table_name = <lfs_group>-tabname
                          cds_level  = <lfs_level>-cds_level ) ).
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_update_db
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_update_db .
  IF gt_cds_process IS NOT INITIAL.
    MODIFY ztcdspross FROM TABLE gt_cds_process.
  ENDIF.

  IF gt_cdsassoc IS NOT INITIAL.
    MODIFY ztcdsassoc FROM TABLE gt_cdsassoc.
  ENDIF.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_create_association_view
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_PROCESS
*&---------------------------------------------------------------------*
FORM frm_create_association_view  TABLES ft_process STRUCTURE ztcdspross
                                  USING fu_level.

  DATA:lt_cdsassoc TYPE TABLE OF ztcdsassoc.

  READ TABLE gt_cds_level INTO DATA(ls_level) WITH KEY cds_level = fu_level.

  CLEAR:it_ddl_source,lt_return.
  LOOP AT gt_cds_process ASSIGNING FIELD-SYMBOL(<lfs_process>) WHERE cds_level = fu_level
                                                                 AND assoc_status = space.

    READ TABLE gt_ztfieldgroup INTO DATA(ls_current) WITH KEY tabname = <lfs_process>-table_name.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    "获取本次使用到的层级的CDS信息
    IF ls_level-use_level IS NOT INITIAL.
      READ TABLE gt_cds_process INTO DATA(ls_process_use) WITH KEY table_name = <lfs_process>-table_name
                                                                   cds_level  = ls_level-use_level.
      IF sy-subrc EQ 0.
        DATA(ls_datasource) = VALUE zsdatasource( tabname = ls_process_use-view_name
                                                  alias   = ls_current-fieldgroup ).
      ENDIF.
    ELSE.
      ls_datasource = VALUE #( tabname = ls_current-tabname
                               alias   = ls_current-fieldgroup ).
    ENDIF.

    CLEAR:gs_cds,
          gs_view,
          LT_cdsassoc.

*------创建CDS名字数据-----*
    gs_cds = VALUE #( cds_name = <lfs_process>-SQL_view_name
                      label    = 'Po Order'
                      view_name = <lfs_process>-view_name ).

    "GET PARENT FIELD GROUP
    READ TABLE gt_ztfieldgroup INTO DATA(ls_parent) WITH KEY fieldgroup = ls_current-parentfieldgroup.
    IF sy-subrc EQ 0.
      READ TABLE ft_process INTO DATA(ls_parent_pro) WITH KEY table_name = ls_parent-tabname.
    ENDIF.

    "GET PARENT
    LOOP AT gt_ztdatafields INTO DATA(ls_field) WHERE fieldgroup = ls_current-fieldgroup
                                                  AND rollfieldname IS NOT INITIAL.

      LT_cdsassoc[] = VALUE #( BASE LT_cdsassoc[] ( assocuuid = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( )
                                                    view_name = <lfs_process>-view_name
                                                    basic_t_name = <lfs_process>-TABLE_name
                                                    basic_f_name = ls_field-fieldname
                                                    target_t_name = ls_parent_pro-view_name
                                                    target_f_name = ls_field-rollfieldname
                                                    basic_alias   = ls_current-fieldgroup
                                                    target_alias  = ls_parent-fieldgroup ) ).
      DATA(lv_parent_flg) = abap_true.
    ENDLOOP.

    "get child
    LOOP AT gt_ztfieldgroup INTO DATA(ls_child) WHERE parentfieldgroup = ls_current-fieldgroup.

      READ TABLE ft_process INTO DATA(ls_child_pro) WITH KEY table_name = ls_child-tabname.

      LOOP AT gt_ztdatafields INTO DATA(ls_child_field) WHERE fieldgroup = ls_child-fieldgroup
                                                          AND rollfieldname IS NOT INITIAL.

        LT_cdsassoc[] = VALUE #( BASE LT_cdsassoc[] ( assocuuid = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( )
                                                      view_name = <lfs_process>-view_name
                                                      basic_t_name = <lfs_process>-TABLE_name
                                                      basic_f_name = ls_CHILD_FIELD-fieldnamE
                                                      target_t_name = ls_child_pro-view_name
                                                      target_f_name = ls_child_field-rollfieldname
                                                      basic_alias   = ls_current-fieldgroup
                                                      target_alias  = ls_child-fieldgroup ) ).
        DATA(lv_child_flg) = abap_true.
      ENDLOOP.

    ENDLOOP.

    IF lv_parent_flg = abap_true AND lv_child_flg = abap_true.
      DATA(lv_assoc_type) = 3.
      gs_cds-association_type = '#TO_COMPOSITION_PARENT,#TO_COMPOSITION,#TO_COMPOSITION_CHILD'.
    ELSEIF lv_parent_flg = abap_true AND lv_child_flg = abap_false.
      lv_assoc_type = 1.
      gs_cds-association_type = '#TO_COMPOSITION_CHILD'.
    ELSEIF lv_parent_flg = abap_false AND lv_child_flg = abap_true.
      lv_assoc_type = 2.
      gs_cds-association_type = '#TO_COMPOSITION_PARENT,#TO_COMPOSITION_ROOT'.
    ENDIF.

    IF LT_cdsassoc[] IS NOT INITIAL.

      PERFORM frm_create_association TABLES LT_cdsassoc
                                     CHANGING gs_view-t_association.

      PERFORM frm_get_cds_view USING ls_current
                                     gs_cds
                                     ls_datasource
                             CHANGING gs_view.

      APPEND LINES OF lt_cdsassoc TO gt_cdsassoc.

      CLEAR:gv_source.
      LOOP AT gt_tab_code INTO DATA(ls_code).
        gv_source = gv_source && | | && ls_code.
      ENDLOOP.

      is_ddl_source-name = <lfs_process>-view_name.
      is_ddl_source-source = gv_source.
      is_ddl_source-ddtext = 'text'.
      APPEND is_ddl_source TO it_ddl_source.
      CLEAR is_ddl_source.
*        it_ddl_source = VALUE #( ( name = <lfs_process>-view_name     "ddl name
*                           source = gv_source    "ddl source
*                           ddtext = 'text' ) ).  "ddl text.

      <lfs_process>-assoc_status = 'X'.
    ENDIF.

    CLEAR:lv_parent_flg,
          lv_child_flg.

  ENDLOOP.

  "调用激活
* &*********Conversion*********&*
  PERFORM get_ddl_source TABLES it_ddl_source lt_ddl_source lt_names.
* &*********Generates an interface variable*********&*
  mo_ddl_handler = cl_wb_ddl_handler_factory=>create( ).
  mo_tr_adapter  = cl_cbo_tr_adapter_factory=>get_ddls_adapter( ).

*    iv_source_origin = ''.
  iv_package = 'Z001'.
  iv_request = 'YESK900696'.

  PERFORM mass_create.
  cl_demo_output=>display( lt_return ).

*   READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY TYPE = 'E'.
*   IF sy-subrc = 0.
*     CONTINUE.
*   ENDIF.
ENDFORM.
**mass cds
*&---------------------------------------------------------------------*
*& Form check_ddl_source
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DDL_SOURCE
*&---------------------------------------------------------------------*
FORM check_ddl_source TABLES pt_ddl_source LIKE lt_ddl_source
                                  pt_return LIKE lt_return
                                RAISING cx_cbo_cds_generation.

  DATA: results TYPE if_wb_ddl_handler=>error_list_sec_obj.
  DATA: rv_okay TYPE abap_bool.

  LOOP AT pt_ddl_source ASSIGNING FIELD-SYMBOL(<fs_source>).
*     TRANSLATE <fs_source>-ddlname TO UPPER CASE.
    "Check
    TRY.
        CLEAR: results,rv_okay.
        results = mo_ddl_handler->check(
          EXPORTING
            name       = <fs_source>-ddlname
            ddlsrcv_wa = <fs_source>
        ).
      CATCH cx_swb_exception INTO DATA(lx_check).
        RAISE EXCEPTION TYPE cx_cbo_cds_generation
          EXPORTING
            previous = lx_check.
    ENDTRY.
    rv_okay = boolc( NOT line_exists( results[ msgty = 'E' ] ) ).

    IF rv_okay = abap_false.
      "Delete false entries
      DELETE pt_ddl_source WHERE ddlname = <fs_source>-ddlname.

      "error messages
      LOOP AT results INTO DATA(ls_result) WHERE msgty = 'E'.
        CLEAR ls_return.
        ls_return-type = ls_result-msgty.
        MESSAGE ID ls_result-msgid
              TYPE ls_result-msgty
            NUMBER ls_result-msgno
              WITH ls_result-msgv1
                   ls_result-msgv2
                   ls_result-msgv3
                   ls_result-msgv4
              INTO ls_return-message.
        APPEND ls_return TO pt_return.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form mass_save_and_activate
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_PACKAGE
*&      --> IV_REQUEST
*&      --> LT_DDL_SOURCE
*&      --> LT_RETURN
*&---------------------------------------------------------------------*
FORM mass_save_and_activate TABLES pt_ddl_source LIKE lt_ddl_source
                                        pt_return LIKE lt_return
                                     USING pv_package pv_request pv_state.


  LOOP AT pt_ddl_source ASSIGNING FIELD-SYMBOL(<fs_source>).
    "Save New ddl Source-Table: DDDDLSRC,DDDDLSRC02BT,DDHEADANNO,DDLDEPENDENCY
    TRY.
        BREAK xuwp.
        mo_ddl_handler->save(
          name           = <fs_source>-ddlname
          put_state      = pv_state
          ddddlsrcv_wa   = <fs_source>
          prid           = -1
        ).
      CATCH cx_dd_ddl_save INTO DATA(lx_save).
        RAISE EXCEPTION TYPE cx_cbo_cds_generation
          EXPORTING
            previous = lx_save.
    ENDTRY.

    "Table-TADIR
    mo_tr_adapter->add_to_object_directory(
      EXPORTING
        iv_object_type   = if_cbo_transport_adapter=>co_trobjtype-ddls
        iv_object_name   = <fs_source>-ddlname
        iv_package       = pv_package
        iv_set_edtflag   = abap_false
    ).

    mo_tr_adapter->add_to_transport(
      EXPORTING
        iv_operation     = if_cbo_transport_adapter=>co_tr_ops-default
        iv_object_type   = if_cbo_transport_adapter=>co_trobjtype-ddls
        iv_object_name   = <fs_source>-ddlname
        iv_package       = pv_package
        iv_request       = pv_request ).

  ENDLOOP.

  "Active
  TRY.
      BREAK xuwp.
      DATA(lt_result) = mo_ddl_handler->mass_activate( VALUE #( FOR <fs_ddl> IN pt_ddl_source ( CONV #( <fs_ddl>-ddlname ) ) ) ).
    CATCH cx_dd_ddl_activate INTO DATA(lx_activation).
      RAISE EXCEPTION TYPE cx_cbo_cds_generation
        EXPORTING
          previous = lx_activation.
  ENDTRY.

  DATA lt_error TYPE if_wb_ddl_handler=>error_list_sec_obj.
  lt_error = VALUE #( FOR <fs_result> IN lt_result WHERE ( msgty = 'E' OR original_severity = 'E' ) ( <fs_result> ) ).

*    IF lt_error IS NOT INITIAL.
*      RAISE EXCEPTION TYPE cx_cbo_cds_generation
*        EXPORTING
*          error_list = lt_error.
*    ENDIF.

  LOOP AT lt_error INTO DATA(ls_error).
    CLEAR ls_return.
    ls_return-type = ls_error-msgty.
    MESSAGE ID ls_error-msgid
          TYPE ls_error-msgty
        NUMBER ls_error-msgno
          WITH ls_error-msgv1
               ls_error-msgv2
               ls_error-msgv3
               ls_error-msgv4
          INTO ls_return-message.
    APPEND ls_return TO pt_return.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_ddl_source
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_DDL_SOURCE
*&      --> LT_DDL_SOURCE
*&---------------------------------------------------------------------*
FORM get_ddl_source  TABLES  it_ddl_source LIKE it_ddl_source
                              lt_ddl_source LIKE lt_ddl_source
                                    lt_names LIKE lt_names.

  BREAK xuwp.
*  IF iv_del_flag EQ abap_false.
  lt_ddl_source[] = VALUE ddddlsrcvtab(
  FOR <fs> IN it_ddl_source (
    ddlname       = <fs>-name           "ddl name
    source        = <fs>-source         "ddl source
    ddlanguage    = sy-langu
*        source_origin = iv_source_origin
    as4user       = sy-uname
    ddtext        = <fs>-ddtext         "ddl text
    )
  ).
*  ELSE.
*      lt_names[] = VALUE #(
*        FOR <fs> IN it_ddl_source (
*          ddls_name = <fs>-name
*        )
*      ).
*  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form mass_delete
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DDL_SOURCE
*&      --> LT_RETURN
*&      --> IV_PACKAGE
*&      --> IV_REQUEST
*&---------------------------------------------------------------------*
FORM mass_delete  TABLES pt_names LIKE lt_names
                              pt_return LIKE lt_return
                         USING pv_package  pv_request.

  DATA lt_names_to_delete LIKE lt_names.
  DATA ls_ddl TYPE ddddlsrcv.

  LOOP AT pt_names ASSIGNING FIELD-SYMBOL(<fs_name>).
    BREAK xuwp.
    "Read ddl source
    CLEAR ls_ddl.
    mo_ddl_handler->read(
      EXPORTING
        name         = <fs_name>-ddls_name
      IMPORTING
        ddddlsrcv_wa = ls_ddl ).
    IF ls_ddl IS NOT INITIAL.
      INSERT <fs_name> INTO TABLE lt_names_to_delete.

      mo_tr_adapter->add_to_transport(
        iv_operation     = if_cbo_transport_adapter=>co_tr_ops-default
        iv_object_type   = if_cbo_transport_adapter=>co_trobjtype-ddls
        iv_object_name   = <fs_name>-ddls_name
        iv_package       = pv_package
        iv_request       = pv_request
      ).
    ENDIF.
  ENDLOOP.

  TRY.
      "mass delete
      DATA(lt_result) = mo_ddl_handler->mass_delete( lt_names_to_delete ).
    CATCH cx_dd_ddl_activate INTO DATA(lx_deletion).
      RAISE EXCEPTION TYPE cx_cbo_cds_generation
        EXPORTING
          previous = lx_deletion.
  ENDTRY.

  DATA lt_error TYPE if_wb_ddl_handler=>error_list_sec_obj.
  lt_error = VALUE #( FOR <fs> IN lt_result WHERE ( msgty = 'E' OR original_severity  = 'E' ) ( <fs> ) ).

  IF NOT lt_error IS INITIAL.
    LOOP AT lt_error INTO DATA(ls_error).
      CLEAR ls_return.
      ls_return-type = ls_error-msgty.
      MESSAGE ID ls_error-msgid
            TYPE ls_error-msgty
          NUMBER ls_error-msgno
            WITH ls_error-msgv1
                 ls_error-msgv2
                 ls_error-msgv3
                 ls_error-msgv4
            INTO ls_return-message.
      APPEND ls_return TO pt_return.
    ENDLOOP.
*      ls_return = VALUE #(  type = 'E' message = 'Delete DDL Source failed!' ).
*      RAISE EXCEPTION TYPE cx_cbo_cds_generation
*        EXPORTING
*          error_list = lt_error.
  ELSE.
    LOOP AT pt_names ASSIGNING <fs_name>.
      mo_tr_adapter->remove_from_object_directory(
       iv_object_type   = if_cbo_transport_adapter=>co_trobjtype-ddls
       iv_object_name   = <fs_name>-ddls_name
       iv_package       = pv_package
     ).
    ENDLOOP.
    ls_return = VALUE #(  type = 'S' message = 'Delete DDL Source Successful!' ).
    APPEND ls_return TO pt_return.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form mass_create
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM mass_create .
*&*********Check ddl Source*********&*
  CLEAR:lt_return.
  PERFORM check_ddl_source TABLES lt_ddl_source lt_return.

  IF lt_return IS NOT INITIAL .
*     cl_demo_output=>display( lt_return ).
    RETURN.
  ELSE.
*&*********Save ddl source and activate*********&*
    PERFORM mass_save_and_activate TABLES lt_ddl_source lt_return
                                     USING iv_package iv_request iv_state.

  ENDIF.
ENDFORM.
