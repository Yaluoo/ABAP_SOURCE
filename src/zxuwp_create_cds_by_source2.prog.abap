*&---------------------------------------------------------------------*
*& Report ZXUWP_CREATE_CDS_BY_SOURCE2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zxuwp_create_cds_by_source2.

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
  DATA: iv_source_origin TYPE ddddlsrcorigin  VALUE 3.
  DATA: ddddlsrcv_t TYPE TABLE OF ddddlsrcv.
  DATA: ddddlsrcv_wa TYPE ddddlsrcv.
  DATA: lt_return TYPE TABLE OF bapiret2,
        ls_return TYPE bapiret2.
  DATA: iv_package TYPE devclass VALUE '$TMP'.
  DATA: iv_request  TYPE trkorr.
  DATA: iv_del_flag TYPE c.
  DATA: iv_state TYPE objstate VALUE 'N'.

*测试流程
* 步骤：
*      iv_flag_del
*       -abap_false
*           1. check
*           2. save and active(Modify automatically update the status : ddddlsrcv-CHGFLAG)
*       -abap_ture
*           1. read
*           2. delete

*&*********DDL Source*********&*
  DATA(rs_ddl_source) =  VALUE ddddlsrcv(
  ddlname             =  'ZXUWP_I_SALESORDER_TP_D5'
  source              =  |@AbapCatalog.sqlViewName: 'ZXUWP_I_SOH_V5' |
                      && |@AccessControl.authorizationCheck: #NOT_REQUIRED |
                      && |@EndUserText.label: 'Sales order for transactional App-2 ' |
                      && |@Search.searchable: true |
                      && |@ObjectModel: \{ |
                      && |    semanticKey: 'SalesOrder', |
                      && |    compositionRoot: true, |
                      && |    transactionalProcessingEnabled: true, |
                      && |    createEnabled: true, |
                      && |    deleteEnabled: true, |
                      && |    updateEnabled: true, |
                      && |    writeActivePersistence: 'ZDEMO_SOH', |
                      && |    draftEnabled: true, |
                      && |    writeDraftPersistence: 'ZDEMO_SOH_D' |
                      && |\} |
                      && |define view ZXUWP_I_SalesOrder_TP_D5 |
                      && |  as select from ZDEMO_SOH as SalesOrder|
                      && |  /* Compositional associations */ |
                      && |  association [0..*] to ZDEMO_I_SalesOrderItem_TP_D   as _Item on |
                      && |    $projection.SalesOrderUUID = _Item.SalesOrderUUID |
                      && |  /* Cross BO associations       */ |
                      && |  association [0..1] to SEPM_I_BusinessPartner       as _BusinessPartner on |
                      && |    $projection.BusinessPartner = _BusinessPartner.BusinessPartner |
                      && |  /* Associations for value help */ |
                      && |  association [0..1] to Sepm_I_SalesOrdOverallStatus as _OverallStatus   on |
                      && |    $projection.OverallStatus = _OverallStatus.SalesOrderOverallStatus \{ |
                      && |  @ObjectModel.readOnly: true |
                      && |  key SalesOrder.salesorderuuid      as SalesOrderUUID, |
                      && |  @Search.defaultSearchElement: true |
                      && |  @ObjectModel.readOnly: true |
                      && |  SalesOrder.salesorder               as SalesOrder, |
                      && |  @ObjectModel.foreignKey.association: '_BusinessPartner' |
                      && |  SalesOrder.businesspartner          as BusinessPartner, |
                      && |  @Search.defaultSearchElement: true |
                      && |  @ObjectModel.foreignKey.association: '_OverallStatus' |
                      && |  SalesOrder.overallstatus            as OverallStatus, |
                      && |  /* Exposed associations */ |
                      && |  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD] |
                      && |  _Item, |
                      && |  /* Associations */ |
*                      && |  _BusinessPartner, |
                      && |  _OverallStatus |
                      && |\} |
  ).

 DATA(rs_ddl_source2) =  VALUE ddddlsrcv(
  ddlname             =  'ZXUWP_I_SALESORDER_TP_D6'
  source              =  |@AbapCatalog.sqlViewName: 'ZXUWP_I_SOH_V6' |
                      && |@AccessControl.authorizationCheck: #NOT_REQUIRED |
                      && |@EndUserText.label: 'Sales order for transactional App-3 ' |
                      && |@Search.searchable: true |
                      && |@ObjectModel: \{ |
                      && |    semanticKey: 'SalesOrder', |
                      && |    compositionRoot: true, |
                      && |    transactionalProcessingEnabled: true, |
                      && |    createEnabled: true, |
                      && |    deleteEnabled: true, |
                      && |    updateEnabled: true, |
                      && |    writeActivePersistence: 'ZDEMO_SOH', |
                      && |    draftEnabled: true, |
                      && |    writeDraftPersistence: 'ZDEMO_SOH_D' |
                      && |\} |
                      && |define view ZXUWP_I_SalesOrder_TP_D6 |
                      && |  as select from ZDEMO_SOH as SalesOrder|
                      && |  /* Compositional associations */ |
                      && |  association [0..*] to ZDEMO_I_SalesOrderItem_TP_D   as _Item on |
                      && |    $projection.SalesOrderUUID = _Item.SalesOrderUUID |
                      && |  /* Cross BO associations       */ |
                      && |  association [0..1] to SEPM_I_BusinessPartner       as _BusinessPartner on |
                      && |    $projection.BusinessPartner = _BusinessPartner.BusinessPartner |
                      && |  /* Associations for value help */ |
                      && |  association [0..1] to Sepm_I_SalesOrdOverallStatus as _OverallStatus   on |
                      && |    $projection.OverallStatus = _OverallStatus.SalesOrderOverallStatus \{ |
                      && |  @ObjectModel.readOnly: true |
                      && |  key SalesOrder.salesorderuuid      as SalesOrderUUID, |
                      && |  @Search.defaultSearchElement: true |
                      && |  @ObjectModel.readOnly: true |
                      && |  SalesOrder.salesorder               as SalesOrder, |
                      && |  @ObjectModel.foreignKey.association: '_BusinessPartner' |
                      && |  SalesOrder.businesspartner          as BusinessPartner, |
                      && |  @Search.defaultSearchElement: true |
                      && |  @ObjectModel.foreignKey.association: '_OverallStatus' |
                      && |  SalesOrder.overallstatus            as OverallStatus, |
                      && |  /* Exposed associations */ |
                      && |  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD] |
                      && |  _Item, |
                      && |  /* Associations */ |
                      && |  _BusinessPartner, |
                      && |  _OverallStatus |
                      && |\} |
  ).

*&*********Parameter*********&*
   it_ddl_source = VALUE #( ( name = rs_ddl_source-ddlname     "ddl name
                              source = rs_ddl_source-source    "ddl source
                              ddtext = rs_ddl_source-ddtext )  "ddl text
                            ( name = rs_ddl_source2-ddlname
                              source = rs_ddl_source2-source
                              ddtext = rs_ddl_source-ddtext )
                          ).

*   iv_source_origin = ''.
   iv_package = 'ZOSWIN'.
*   iv_request = 'YESK900519'.
   iv_request = 'YESK900662'.

   BREAK xuwp.
   CHECK it_ddl_source IS NOT INITIAL.

*&*********Conversion*********&*
   PERFORM get_ddl_source TABLES it_ddl_source lt_ddl_source lt_names.

*&*********Generates an interface variable*********&*
   DATA(mo_ddl_handler) = cl_wb_ddl_handler_factory=>create( ).
   DATA(mo_tr_adapter)  = cl_cbo_tr_adapter_factory=>get_ddls_adapter( ).

   IF iv_del_flag EQ abap_false.
*&*********Create and Modify ddl Source*********&*
     PERFORM mass_create.
   ELSE.
*&*********Delete ddl Source*********&*
     BREAK xuwp.
     PERFORM mass_delete TABLES lt_names lt_return
                           USING iv_package iv_request.
   ENDIF.

   CHECK lt_return IS NOT INITIAL.
   cl_demo_output=>display( lt_return ).


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
          iv_package       = iv_package
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
  IF iv_del_flag EQ abap_false.
      lt_ddl_source[] = VALUE ddddlsrcvtab(
      FOR <fs> IN it_ddl_source (
        ddlname       = <fs>-name           "ddl name
        source        = <fs>-source         "ddl source
        ddlanguage    = sy-langu
        source_origin = iv_source_origin
        as4user       = sy-uname
        ddtext        = <fs>-ddtext         "ddl text
        )
      ).
  ELSE.
      lt_names[] = VALUE #(
        FOR <fs> IN it_ddl_source (
          ddls_name = <fs>-name
        )
      ).
  ENDIF.


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
   PERFORM check_ddl_source TABLES lt_ddl_source lt_return.

   IF lt_return IS NOT INITIAL .
     cl_demo_output=>display( lt_return ).
   ELSE.
*&*********Save ddl source and activate*********&*
     PERFORM mass_save_and_activate TABLES lt_ddl_source lt_return
                                      USING iv_package iv_request iv_state.

   ENDIF.
ENDFORM.
