*&---------------------------------------------------------------------*
*& Report Z_MB_GENERATE_CDS_SOURCE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zxuwp_mb_generate_cds_source.

" cl_cbo_cds_composer was used as reference implementation (template CBO_CMP_NODE_VIEW)
DATA:
  ls_view     TYPE if_aps_cke_comm_types=>ts_view,
  lo_composer TYPE REF TO cl_cmp_composer,
  lt_tab_code TYPE cl_cmp_composer=>tab_code,
  lv_code     TYPE string.

BREAK-POINT.

ls_view = VALUE #(
  s_root = VALUE #(
              cds_name = 'YY1_SalesOrder' ignore_propagated_annos = abap_true label = 'Sales Order' view_name = 'YAGDJFL57547'
              s_exposure = VALUE #( as_analytical_view_ind = abap_false as_service_ind = abap_true )
           )
  t_primary_data_source = VALUE #( ( name = 'I_Aps_Cke_Epm_Salesorder' alias = '_Root'  ) )

  t_field = VALUE #(
    ( alias = 'OrderId' data_source_field = VALUE #( data_source_alias = '_Root' field_name = 'Id' ) label = VALUE #( value = |Sales Order's ID| ) )
    ( alias = 'TransactionCurrency' data_source_field = VALUE #( data_source_alias = '_Root' field_name = 'TransactionCurrency' ) label = VALUE #( value = 'Currency Code' ) )
    ( alias = 'GrossAmountInTransacCurrency' data_source_field = VALUE #( data_source_alias = '_Root' field_name = 'GrossAmountInTransacCurrency' ) label = VALUE #( value = 'Gross Amount' ) )
    ( alias = 'NetAmountInTransactionCurrency' data_source_field = VALUE #( data_source_alias = '_Root' field_name = 'NetAmountInTransactionCurrency' ) label = VALUE #( value = 'Net Amount' ) )
    ( alias = 'SalesOrderOverallStatus' data_source_field = VALUE #( data_source_alias = '_Root' field_name = 'SalesOrderOverallStatus' ) label = VALUE #( value = 'Overall Status' ) )
  )

  "传入参数
  t_parameter = VALUE #(
    ( name = 'P_Currency' data_type_name = 'SO_CURR' )
    ( name = 'P_ExchangeDate' data_type_name = 'CURR_DATE' )
  )

  t_association = VALUE #(
    (
      s_target_data_source = VALUE #( alias = '_Item' name = 'I_Aps_Cke_Epm_Salesorderitem' )
      s_cardinality = VALUE #( min = 0 max = 100 )
      t_on_condition = VALUE #(
        ( lhs_field = VALUE #( data_source_alias = '_Item' field_name = 'SalesOrderId' ) operator = '=' rhs_field = VALUE #( data_source_alias = '_Root' field_name = 'SalesOrderId' ) rhs_type = 'F' )
        ( lhs_field = VALUE #( data_source_alias = '_Item' field_name = 'TransactionCurrency' ) operator = '=' rhs_type = 'C' rhs_value = 'EUR' )
      )
    )
  )
) .

*DATA: lv_dauer    TYPE i,
*      lv_seconds  TYPE p DECIMALS 2,
*      lv_no_times TYPE i VALUE 100.


lo_composer = cl_cmp_composer=>s_create( ).
lo_composer->add_var( i_name = 'view' i_value = ls_view ).
*
*CLEAR: lv_dauer, lv_seconds .
*GET RUN TIME FIELD DATA(x1).
*
*DO lv_no_times TIMES.
  TRY.
  lt_tab_code = lo_composer->build_code( i_template_name = 'Z_MB_CUST_CDS_VIEW' ).
    CATCH cx_cmp_failure cx_cmp_invalid_param  INTO DATA(lx_cmp).
      RAISE EXCEPTION TYPE cx_cbo_cds_generation
        EXPORTING
          previous = lx_cmp.
  ENDTRY.
*ENDDO.
*GET RUN TIME FIELD DATA(x2).
*lv_seconds = ( x2 - x1 ) / 1000000.
*
*WRITE: /, 'Code composer: ', lv_seconds.
*
*CLEAR: lv_dauer, lv_seconds .
*GET RUN TIME FIELD x1.
*
*DO lv_no_times TIMES.
  lt_tab_code = lo_composer->build_code( i_template_name = 'Z_MB_CUST_CDS_VIEW_SINGLE' ).
*ENDDO.
*GET RUN TIME FIELD x2.
*lv_seconds = ( x2 - x1 ) / 1000000.
*
*WRITE: /, 'Code composer (single template): ', lv_seconds.


DATA(lo_ddl) = cl_aps_cke_ddl_factory=>get_instance( )->get_ddl(
    is_ddl              = ls_view
    it_assoc_privileged = VALUE #( )
).

*CLEAR: lv_dauer, lv_seconds .
*GET RUN TIME FIELD DATA(x3).
*
*DO lv_no_times TIMES.
  lv_code = lo_ddl->get_coding( ).
*ENDDO.
*GET RUN TIME FIELD DATA(x4).
*lv_seconds = ( x4 - x3 ) / 1000000.
*
*WRITE: /, 'String concatenation: ', lv_seconds.

*LOOP AT lt_tab_code INTO DATA(lv_line).
*  WRITE /  lv_line .
*ENDLOOP.
*
*lv_code = concat_lines_of( table = lt_tab_code sep = cl_abap_char_utilities=>cr_lf ).


BREAK-POINT.

DATA(lv_success) = CL_APS_CKE_CONTROLLER=>get_instance( )->save_draft(
  EXPORTING
    is_ddl       = ls_view
    iv_is_create = abap_true ).

DATA(lo_man) = CL_APS_CKE_CONTROLLER=>get_instance( ).


BREAK-POINT.
