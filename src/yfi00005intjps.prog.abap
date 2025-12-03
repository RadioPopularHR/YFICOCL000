*&---------------------------------------------------------------------*
*& Report  YFI00005INTJPS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT yfi00005intjps NO STANDARD PAGE HEADING MESSAGE-ID yac.

INCLUDE yfi00005intjps_top.
INCLUDE yfi00005intjps_frm.

START-OF-SELECTION.
  PERFORM clear_tables.
  PERFORM get_values.
  PERFORM get_values_sales.

END-OF-SELECTION.
  PERFORM sort_values.
  PERFORM validate_values.
  PERFORM validate_values_vendor.
  PERFORM create_data.
  PERFORM create_vendor.

  IF NOT gt_erro[] IS INITIAL.
    SORT gt_erro BY pernr ASCENDING.
    PERFORM list_values.
  ENDIF.
