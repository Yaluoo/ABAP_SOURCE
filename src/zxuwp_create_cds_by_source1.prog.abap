*&---------------------------------------------------------------------*
*& Report ZXUWP_CREATE_CDS_BY_SOURCE1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zxuwp_create_cds_by_source1.

  TYPES: BEGIN OF ty_ddl_name_source,
        name   TYPE ddlname,
        source TYPE ddddlsource,
        ddtext TYPE ddtext,
      END OF ty_ddl_name_source.

  TYPES tt_ddl_name_source TYPE STANDARD TABLE OF ty_ddl_name_source WITH KEY name.
  DATA: it_ddl_source TYPE tt_ddl_name_source.
  DATA: is_ddl_source TYPE ty_ddl_name_source.
  DATA: iv_source_origin TYPE ddddlsrcorigin  VALUE 3.
  DATA: iv_package TYPE devclass VALUE '$TMP'.
  DATA: iv_request  TYPE trkorr.
  DATA: ddddlsrcv_t TYPE TABLE OF ddddlsrcv.
  DATA: ddddlsrcv_wa TYPE ddddlsrcv.
  DATA: lt_return TYPE TABLE OF bapiret2.
  DATA: results TYPE if_wb_ddl_handler=>error_list_sec_obj.
  DATA: rv_okay TYPE abap_bool.
  DATA: ls_return TYPE bapiret2.

*测试流程
*解决报错
* 步骤：
*      1. check
*      2. read
*         abap_true-del
*      3. save
*      4. active

*&*********DDL Source*********&*
*  DATA(rs_ddl_source) =  VALUE ddddlsrcv(
*  ddlname             =  'ZXUWP_I_SALESORDER_TP_D3'
*  source              =  |@AbapCatalog.sqlViewName: 'ZXUWP_I_SOH_V3' |
*                      && |@AccessControl.authorizationCheck: #NOT_REQUIRED |
*                      && |@EndUserText.label: 'Sales order for transactional App-7 ' |
*                      && |@Search.searchable: true |
*                      && |@ObjectModel: \{ |
*                      && |    semanticKey: 'SalesOrder', |
*                      && |    compositionRoot: true, |
*                      && |    transactionalProcessingEnabled: true, |
*                      && |    createEnabled: true, |
*                      && |    deleteEnabled: true, |
*                      && |    updateEnabled: true, |
*                      && |    writeActivePersistence: 'ZDEMO_SOH', |
*                      && |    draftEnabled: true, |
*                      && |    writeDraftPersistence: 'ZDEMO_SOH_D' |
*                      && |\} |
*                      && |define view ZXUWP_I_SalesOrder_TP_D3 |
*                      && |  as select from ZDEMO_SOH as SalesOrder|
*                      && |  /* Compositional associations */ |
*                      && |  association [0..*] to ZDEMO_I_SalesOrderItem_TP_D   as _Item on |
*                      && |    $projection.SalesOrderUUID = _Item.SalesOrderUUID |
*                      && |  /* Cross BO associations       */ |
*                      && |  association [0..1] to SEPM_I_BusinessPartner       as _BusinessPartner on |
*                      && |    $projection.BusinessPartner = _BusinessPartner.BusinessPartner |
*                      && |  /* Associations for value help */ |
*                      && |  association [0..1] to Sepm_I_SalesOrdOverallStatus as _OverallStatus   on |
*                      && |    $projection.OverallStatus = _OverallStatus.SalesOrderOverallStatus \{ |
*                      && |  @ObjectModel.readOnly: true |
*                      && |  key SalesOrder.salesorderuuid      as SalesOrderUUID, |
*                      && |  @Search.defaultSearchElement: true |
*                      && |  @ObjectModel.readOnly: true |
*                      && |  SalesOrder.salesorder               as SalesOrder, |
*                      && |  @ObjectModel.foreignKey.association: '_BusinessPartner' |
*                      && |  SalesOrder.businesspartner          as BusinessPartner, |
*                      && |  @Search.defaultSearchElement: true |
*                      && |  @ObjectModel.foreignKey.association: '_OverallStatus' |
*                      && |  SalesOrder.overallstatus            as OverallStatus, |
*                      && |  /* Exposed associations */ |
*                      && |  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD] |
*                      && |  _Item, |
*                      && |  /* Associations */ |
**                      && |  _BusinessPartner, |
*                      && |  _OverallStatus |
*                      && |\} |
*  ).
*
* DATA(rs_ddl_source2) =  VALUE ddddlsrcv(
*  ddlname             =  'ZXUWP_I_SALESORDER_TP_D4'
*  source              =  |@AbapCatalog.sqlViewName: 'ZXUWP_I_SOH_V4' |
*                      && |@AccessControl.authorizationCheck: #NOT_REQUIRED |
*                      && |@EndUserText.label: 'Sales order for transactional App-5 ' |
*                      && |@Search.searchable: true |
*                      && |@ObjectModel: \{ |
*                      && |    semanticKey: 'SalesOrder', |
*                      && |    compositionRoot: true, |
*                      && |    transactionalProcessingEnabled: true, |
*                      && |    createEnabled: true, |
*                      && |    deleteEnabled: true, |
*                      && |    updateEnabled: true, |
*                      && |    writeActivePersistence: 'ZDEMO_SOH', |
*                      && |    draftEnabled: true, |
*                      && |    writeDraftPersistence: 'ZDEMO_SOH_D' |
*                      && |\} |
*                      && |define view ZXUWP_I_SalesOrder_TP_D4 |
*                      && |  as select from ZDEMO_SOH as SalesOrder|
*                      && |  /* Compositional associations */ |
*                      && |  association [0..*] to ZDEMO_I_SalesOrderItem_TP_D   as _Item on |
*                      && |    $projection.SalesOrderUUID = _Item.SalesOrderUUID |
*                      && |  /* Cross BO associations       */ |
*                      && |  association [0..1] to SEPM_I_BusinessPartner       as _BusinessPartner on |
*                      && |    $projection.BusinessPartner = _BusinessPartner.BusinessPartner |
*                      && |  /* Associations for value help */ |
*                      && |  association [0..1] to Sepm_I_SalesOrdOverallStatus as _OverallStatus   on |
*                      && |    $projection.OverallStatus = _OverallStatus.SalesOrderOverallStatus \{ |
*                      && |  @ObjectModel.readOnly: true |
*                      && |  key SalesOrder.salesorderuuid      as SalesOrderUUID, |
*                      && |  @Search.defaultSearchElement: true |
*                      && |  @ObjectModel.readOnly: true |
*                      && |  SalesOrder.salesorder               as SalesOrder, |
*                      && |  @ObjectModel.foreignKey.association: '_BusinessPartner' |
*                      && |  SalesOrder.businesspartner          as BusinessPartner, |
*                      && |  @Search.defaultSearchElement: true |
*                      && |  @ObjectModel.foreignKey.association: '_OverallStatus' |
*                      && |  SalesOrder.overallstatus            as OverallStatus, |
*                      && |  /* Exposed associations */ |
*                      && |  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD] |
*                      && |  _Item, |
*                      && |  /* Associations */ |
*                      && |  _BusinessPartner, |
*                      && |  _OverallStatus |
*                      && |\} |
*  ).

   BREAK-POINT.
*&*********Parameter*********&*
*    DATA(rs_ddl_source) =  VALUE ddddlsrcv(
*    ddlname             =  'ZXUWP_I_ZZV_TEST100'
*    source              =  |@AbapCatalog.sqlViewName: 'ZZV_TEST100' |
*                        && |@AccessControl.authorizationCheck: #CHECK |
*                        && |@AbapCatalog.compiler.compareFilter: true |
*                        && |@OData.publish: true |
*                        && |define view ZTEST100 |
*                        && | as select from |
*                        && | ZTEKKO as ZGROUP1 |
*                        && |  association [0..100] |
*                        && | to ZTEKPO as ZGROUP2 |
*                        && | on |
*                        && |  ZGROUP2.EBELN_I = |
*                        && |   ZGROUP1.EBELN_H |
*                        && |  \{ |
*                        && |  @EndUserText.label: 'AEDAT' |
*                        && |  ZGROUP1.AEDAT as AEDAT, |
*                        && |  @EndUserText.label: 'WAERS' |
*                        && |  ZGROUP1.WAERS as WAERS, |
*                        && | @EndUserText.label: 'WKURS' |
*                        && | ZGROUP1.WKURS as WKURS |
*                        && |  , |
*                        && |  @EndUserText.label: 'EBELN_H' |
*                        && |  ZGROUP1.EBELN_H as EBELN_H |
*                        && |\} |
*    ).
*   is_ddl_source = VALUE ty_ddl_name_source( name = rs_ddl_source-ddlname
*                                             source = rs_ddl_source-source
*                                             ddtext = rs_ddl_source-ddtext ).
*   APPEND is_ddl_source to it_ddl_source.
*   rs_ddl_source =  VALUE ddddlsrcv(
*  ddlname             =  'ZXUWP_I_ZZV_TEST101'
*  source              =  |@AbapCatalog.sqlViewName: 'ZZV_TEST101' |
*                      && |@AccessControl.authorizationCheck: #CHECK |
*                      && |@AbapCatalog.compiler.compareFilter: true |
*                      && |@OData.publish: true |
*                      && |define view ZTEST101 |
*                      && | as select from |
*                      && | ZTEKPO as ZGROUP2 |
*                      && |  association [0..100] |
*                      && | to ZTEKKO as ZGROUP1 |
*                      && | on |
*                      && |  ZGROUP1.EBELN_H = |
*                      && |  ZGROUP2.EBELN_I |
*                      && |  association [0..100] |
*                      && |  to ZTKONP as ZGROUP3 |
*                      && |  on |
*                      && |  ZGROUP3.EBELN_P = |
*                      && |  ZGROUP2.EBELN_I |
*                      && |  and |
*                      && |  ZGROUP3.EBELP_P = |
*                      && |  ZGROUP2.EBELP |
*                      && |  \{ |
*                      && |  @EndUserText.label: 'MATNR' |
*                      && |  ZGROUP2.MATNR as MATNR, |
*                      && |  @EndUserText.label: 'MENGE' |
*                      && | ZGROUP2.MENGE as MENGE, |
*                      && | @EndUserText.label: 'MEINS' |
*                      && | ZGROUP2.MEINS as MEINS |
*                      && |  , |
*                      && |  @EndUserText.label: 'DRUHR' |
*                      && |  ZGROUP2.DRUHR as DRUHR, |
*                      && |  @EndUserText.label: 'EBELN_I' |
*                      && |  ZGROUP2.EBELN_I as EBELN_I, |
*                      && |  @EndUserText.label: 'EBELP' |
*                      && |  ZGROUP2.EBELP as EBELP |
*                      && |\} |
*  ).
*                         is_ddl_source = VALUE ty_ddl_name_source( name = rs_ddl_source-ddlname
*                                             source = rs_ddl_source-source
*                                             ddtext = rs_ddl_source-ddtext ).
*   APPEND is_ddl_source to it_ddl_source.
*   is_ddl_source = VALUE ty_ddl_name_source( name = rs_ddl_source-ddlname
*                                             source = rs_ddl_source-source
*                                             ddtext = rs_ddl_source-ddtext ).
*   APPEND is_ddl_source to it_ddl_source.
*   rs_ddl_source =  VALUE ddddlsrcv(
*  ddlname             =  'ZXUWP_I_ZZV_TEST102'
*  source              =  |@AbapCatalog.sqlViewName: 'ZZV_TEST102' |
*                      && |@AccessControl.authorizationCheck: #CHECK |
*                      && |@AbapCatalog.compiler.compareFilter: true |
*                      && |@OData.publish: true |
*                      && |define view ZTEST102 |
*                      && | as select from |
*                      && | ZTKONP as ZGROUP3 |
*                      && |  association [0..100] |
*                      && | to ZTEKPO as ZGROUP2 |
*                      && | on |
*                      && |  ZGROUP2.EBELN_I = |
*                      && |   ZGROUP3.EBELN_P |
*                      && |  and |
*                      && |  ZGROUP2.EBELP = |
*                      && |  ZGROUP3.EBELP_P |
*                      && |  \{ |
*                      && |  @EndUserText.label: 'EBELN_P' |
*                      && |  ZGROUP3.EBELN_P as EBELN_P, |
*                      && |  @EndUserText.label: 'KSCHL' |
*                      && |  ZGROUP3.KSCHL as KSCHL, |
*                      && | @EndUserText.label: 'EBELP_P' |
*                      && | ZGROUP3.EBELP_P as EBELP_P |
*                      && |  , |
*                      && |  @EndUserText.label: 'KOMXWR' |
*                      && |  ZGROUP3.KOMXWR as KOMXWR |
*                      && |\} |
*  ).
*                         is_ddl_source = VALUE ty_ddl_name_source( name = rs_ddl_source-ddlname
*                                             source = rs_ddl_source-source
*                                             ddtext = rs_ddl_source-ddtext ).
*   APPEND is_ddl_source to it_ddl_source.

    DATA(rs_ddl_source) =  VALUE ddddlsrcv(
    ddlname             =  'ZXUWP_I_ZZV_TEST103'
    source              =  |@AbapCatalog.sqlViewName: 'ZDEMO_I_SOH_V2' |
                        && |@AccessControl.authorizationCheck: #NOT_REQUIRED |
                        && |@EndUserText.label: 'Sales order for transactional app' |
                        && |@Search.searchable: true |
                        && |@ObjectModel: \{ |
                        && |semanticKey: 'SalesOrder', |
                        && | compositionRoot: true, |
                        && | createEnabled: true, |
                        && | deleteEnabled: true, |
                        && | updateEnabled: true |
                        && | \} |
                        && | define view ZDEMO_I_SalesOrder_TP_D2 |
                        && |   as select from zdemo_soh as SalesOrder |
                        && |  association [0..*] to ZDEMO_I_SalesOrderItem_TP_D2   as _Item on $projection.SalesOrderUUID = _Item.SalesOrderUUID |
                        && |  association [0..1] to SEPM_I_BusinessPartner       as _BusinessPartner on |
                        && |  $projection.BusinessPartner = _BusinessPartner.BusinessPartner |
                        && |  association [0..1] to Sepm_I_SalesOrdOverallStatus as _OverallStatus   on |
                        && |  $projection.OverallStatus = _OverallStatus.SalesOrderOverallStatus \{ |
                        && | key SalesOrder.salesorderuuid      as SalesOrderUUID, |
                        && | SalesOrder.salesorder               as SalesOrder, |
                        && | SalesOrder.businesspartner          as BusinessPartner, |
                        && | SalesOrder.overallstatus            as OverallStatus, |
                        && |  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD] |
                        && |_Item, |
                        && |_BusinessPartner, |
                        && |_OverallStatus |
                        && | \}|
    ).
   is_ddl_source = VALUE ty_ddl_name_source( name = rs_ddl_source-ddlname
                                             source = rs_ddl_source-source
                                             ddtext = rs_ddl_source-ddtext ).
   APPEND is_ddl_source to it_ddl_source.

    rs_ddl_source =  VALUE ddddlsrcv(
    ddlname             =  'ZXUWP_I_ZZV_TEST104'
    source              =  |@AbapCatalog.sqlViewName: 'ZDEMO_I_SOI_V2' |
                        && |@AccessControl.authorizationCheck: #NOT_REQUIRED |
                        && |@EndUserText.label: 'Sales Order Items - subnode BO view' |
                        && |@Search.searchable: true |
                        && |@ObjectModel: \{ |
                        && | semanticKey: [ 'SalesOrder', 'SalesOrderItem' ], |
                        && | createEnabled: true, |
                        && | deleteEnabled: true, |
                        && | updateEnabled: true |
                        && | \} |
                        && | define view ZDEMO_I_SalesOrderItem_TP_D2 |
                        && |   as select from zdemo_soitem as SalesOrderItem |
                        && |  association [1..1] to ZDEMO_I_SalesOrder_TP_D2 as _SalesOrder on $projection.SalesOrderUUID = _SalesOrder.SalesOrderUUID |
                        && |  association [0..1] to SEPM_I_Product_E        as _Product    on |
                        && |  $projection.Product = _Product.Product |
                        && |  association [0..1] to SEPM_I_Currency         as _Currency   on |
                        && |  $projection.CurrencyCode = _Currency.Currency \{ |
                        && | key SalesOrderItem.salesorderitemuuid as SalesOrderItemUUID, |
                        && | SalesOrderItem.salesorderuuid as SalesOrderUUID, |
                        && | SalesOrderItem.salesorder as SalesOrder, |
                        && | SalesOrderItem.salesorderitem as SalesOrderItem, |
                        && | SalesOrderItem.product as Product, |
                        && | SalesOrderItem.currencycode as CurrencyCode, |
                        && | SalesOrderItem.grossamount as GrossAmount, |
                        && | SalesOrderItem.quantity as Quantity, |
                        && | @ObjectModel.association.type: [#TO_COMPOSITION_PARENT, #TO_COMPOSITION_ROOT] |
                        && |_Product,|
                        && |_Currency |
                        && | \}|
    ).
   is_ddl_source = VALUE ty_ddl_name_source( name = rs_ddl_source-ddlname
                                             source = rs_ddl_source-source
                                             ddtext = rs_ddl_source-ddtext ).
   APPEND is_ddl_source to it_ddl_source.
*   it_ddl_source = VALUE #( ( name = rs_ddl_source-ddlname     "ddl name
*                              source = rs_ddl_source-source    "ddl source
*                              ddtext = rs_ddl_source-ddtext )  "ddl text
**                            ( name = rs_ddl_source2-ddlname
**                              source = rs_ddl_source2-source
**                              ddtext = rs_ddl_source-ddtext )
*                          ).

*   iv_source_origin = ''.
   iv_package = 'ZOSWIN'.
   iv_request = 'YESK900662'.

   CHECK it_ddl_source IS NOT INITIAL.

*&*********DDL Source*********&*
    DATA(lt_ddl_source) = VALUE ddddlsrcvtab(
      FOR <fs> IN it_ddl_source (
        ddlname       = <fs>-name
        source        = <fs>-source
        ddlanguage    = sy-langu
        source_origin = iv_source_origin
        as4user       = sy-uname
        ddtext        = <fs>-ddtext
      )
    ).

*&*********Generates an interface variable*********&*
   DATA(mo_ddl_handler) = cl_wb_ddl_handler_factory=>create( ).
   DATA(mo_tr_adapter)  = cl_cbo_tr_adapter_factory=>get_ddls_adapter( ).

*&*********DDL Source Changes in operating*********&*
   LOOP AT lt_ddl_source ASSIGNING FIELD-SYMBOL(<fs_source>).

     TRANSLATE <fs_source>-ddlname TO UPPER CASE.
     "Check
     TRY.
         BREAK-POINT.
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

*         CALL METHOD lx_check->if_message~get_text
*           RECEIVING
*             result = DATA(ls_result1)
*             .

     ENDTRY.
     rv_okay = boolc( NOT line_exists( results[ msgty = 'E' ] ) ).

     IF rv_okay = abap_false.
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
          APPEND ls_return TO lt_return.
       ENDLOOP.
       CONTINUE.
     ENDIF.

     BREAK-POINT.
     "Read
*     TRY.
*        CLEAR ddddlsrcv_wa.
*        mo_ddl_handler->read(
*          EXPORTING
*            name           = <fs_source>-ddlname
*          IMPORTING
*            ddddlsrcv_wa   = ddddlsrcv_wa ).
*      CATCH cx_dd_ddl_read INTO DATA(lx_read).
*        RAISE EXCEPTION TYPE cx_cbo_cds_generation
*          EXPORTING
*            previous = lx_read.
*      ENDTRY.
*
*      IF ddddlsrcv_wa IS NOT INITIAL.
*        "Del Old ddl Souce
**        REFRESH: ddddlsrcv_t.
**        APPEND ddddlsrcv_wa TO ddddlsrcv_t.
*        mo_tr_adapter->add_to_transport(
*          iv_operation     = if_cbo_transport_adapter=>co_tr_ops-default
*          iv_object_type   = if_cbo_transport_adapter=>co_trobjtype-ddls
*          iv_object_name   = ddddlsrcv_wa-ddlname
*          iv_package       = iv_package
*          iv_request       = iv_request
*        ).
*
*        TRY.
*            mo_ddl_handler->delete( ddddlsrcv_wa-ddlname ).
*        CATCH cx_swb_exception INTO DATA(lx_del).
*            RAISE EXCEPTION TYPE cx_cbo_cds_generation
*              EXPORTING
*                previous = lx_del.
*        ENDTRY.
*
*        mo_tr_adapter->remove_from_object_directory(
*          iv_object_type   = if_cbo_transport_adapter=>co_trobjtype-ddls
*          iv_object_name   = ddddlsrcv_wa-ddlname
*          iv_package       = iv_package
*        ).
*
*      ENDIF.

      "Save New ddl Source-Table: DDDDLSRC,DDDDLSRC02BT,DDHEADANNO,DDLDEPENDENCY
      TRY.
          mo_ddl_handler->save(
            name           = <fs_source>-ddlname
            put_state      = 'N'
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
          iv_package       = iv_package
          iv_request       = iv_request ).
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      cl_demo_output=>display( lt_return ).
      EXIT.
    ENDIF.

    "Active
    TRY.
        DATA(lt_result) = mo_ddl_handler->mass_activate( VALUE #( FOR <fs_ddl> IN lt_ddl_source ( CONV #( <fs_ddl>-ddlname ) ) ) ).
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
      CLEAR ls_result.
      ls_return-type = ls_error-msgty.
          MESSAGE ID ls_error-msgid
                TYPE ls_error-msgty
              NUMBER ls_error-msgno
                WITH ls_error-msgv1
                     ls_error-msgv2
                     ls_error-msgv3
                     ls_error-msgv4
                INTO ls_return-message.
       APPEND ls_return TO lt_return.
    ENDLOOP.

    cl_demo_output=>display( lt_return ).

*****************************************************************
