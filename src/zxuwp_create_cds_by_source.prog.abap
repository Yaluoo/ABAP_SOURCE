*&---------------------------------------------------------------------*
*& Report ZTMP_CREATE_CDS_BY_SOURCE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
"激活CDS生成BOPF对象和ODATA
REPORT zxuwp_create_cds_by_source.

BREAK-POINT.

DATA:
  et_msg      TYPE rtc_t_message,
  lt_ddl_name TYPE if_ddic_wb_ddls_secobj_manager=>ty_ddls_names,
  lx_root     TYPE REF TO cx_root.

*1. 找源码，
*2. 过程-增删改查，捕获异常
*3. 参数，复用，作用PERFORM
*4. 批量ddl处理，save->activity(那个先后，类比于SE11)






"DDL源数据
DATA(rs_ddl_source) =    VALUE ddddlsrcv(
  ddlname           =    'ZXUWP_I_SALESORDER_TP_D'
  source            =    |@AbapCatalog.sqlViewName: 'ZXUWP_I_SOH_V' |
                      && |@AccessControl.authorizationCheck: #NOT_REQUIRED |
                      && |@EndUserText.label: 'Sales order for transactional app' |
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
                      && |define view ZXUWP_I_SalesOrder_TP_D |
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

"生成接口变量
DATA(go_wb_ddl_api) =   cl_wb_ddl_handler_factory=>create( ).

"对象条目写入传输请求--三个对象写入
*CALL METHOD go_wb_ddl_api->write_trkorr
*  EXPORTING
*    trkorr     = 'YESK900679'
*    objectname = rs_ddl_source-ddlname
*    prid       = -1
*  IMPORTING
*    order      = DATA(lv_order)
*    task       = DATA(lv_task)
*    rc         = DATA(lv_rc).

TRY .
    TRANSLATE rs_ddl_source-ddlname TO UPPER CASE.

*增删改查
    "检查DDL源
    DATA(error) = go_wb_ddl_api->check(
     name = rs_ddl_source-ddlname
     ddlsrcv_wa = rs_ddl_source   "全部内容
    ).

   "保存DDL源代码：TABLE-DDDDLSRC,DDDDLSRC02BT,DDLDEPENDENCY
    go_wb_ddl_api->save(
      EXPORTING
        name           = rs_ddl_source-ddlname
        put_state      = 'N'
        ddddlsrcv_wa   = rs_ddl_source
    ).

    "写入资源库对象目录：TABLE-TADIR
    go_wb_ddl_api->write_tadir(
      EXPORTING
        objectname     = rs_ddl_source-ddlname
        devclass       = 'ZOSWIN'
        username       = sy-uname
        set_edtflag    = abap_false
        set_genflag    = abap_false
        prid           = -1
    ).

    APPEND rs_ddl_source-ddlname TO lt_ddl_name.

  CATCH cx_dd_ddl_save INTO lx_root.
    APPEND cl_rtc_comm_util=>extract_error_msg_to_msgclass( iv_msg_text = lx_root->get_text( ) ) TO et_msg.
ENDTRY.


TRY .
    "激活DDL源
    DATA(lt_error_list) = go_wb_ddl_api->mass_activate( names = lt_ddl_name ).
    IF 1 = 1. ENDIF.
  CATCH cx_root INTO lx_root.
    APPEND cl_rtc_comm_util=>extract_error_msg_to_msgclass( iv_msg_text = lx_root->get_text( ) ) TO et_msg.
ENDTRY.
