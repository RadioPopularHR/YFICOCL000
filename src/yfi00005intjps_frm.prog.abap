*----------------------------------------------------------------------*
***INCLUDE YFI00005INTJPS_FRM .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CLEAR_TABLES
*&---------------------------------------------------------------------*
FORM clear_tables.

  REFRESH: gt_kna1, gt_knb1, gt_knvv, gt_lfa1, gt_lfbk,
           gt_lfb1, gt_erro.
  CLEAR:   wa_kna1, wa_knb1, wa_knvv, wa_lfa1, wa_lfbk,
           wa_lfb1, wa_erro.

ENDFORM.                    " CLEAR_TABLES
*&---------------------------------------------------------------------*
*&      Form  GET_VALUES
*&---------------------------------------------------------------------*
FORM get_values.

  DATA: lv_endda TYPE pa0002-endda,
        lv_locat TYPE pa0006-locat.

  CLEAR: wa_erro.

  SELECT endda pernr cname nachn nach2 vorna gblnd gesch famst gbdat
    INTO CORRESPONDING FIELDS OF pa0002
    FROM pa0002
    WHERE pernr IN s_pernr
      AND subty EQ ''
    ORDER BY endda DESCENDING.

    SELECT endda stras pstlz ort01 telnr locat land1 locat
      INTO (lv_endda, wa_kna1-stras, wa_kna1-pstlz, wa_kna1-mcod3,
            wa_kna1-telf1, wa_kna1-ort02, wa_kna1-land1, lv_locat)
      FROM pa0006
      WHERE pernr EQ pa0002-pernr
        AND subty EQ '1'
      ORDER BY endda DESCENDING.
      EXIT.
    ENDSELECT.

    IF wa_kna1-stras IS INITIAL.
      wa_kna1-stras = '.'.
      wa_erro-pernr = pa0002-pernr.
      wa_erro-message = 'Rua (STRAS) criado com "."'.
      APPEND wa_erro TO gt_erro. CLEAR wa_erro.
    ENDIF.
    IF wa_kna1-mcod3 IS INITIAL.
      IF NOT lv_locat IS INITIAL.
        wa_kna1-mcod3 = lv_locat.
      ELSE.
        wa_kna1-mcod3 ='.'.
        wa_erro-pernr = pa0002-pernr.
        wa_erro-message = 'Cidade (MCOD3) criado com "."'.
        APPEND wa_erro TO gt_erro. CLEAR wa_erro.
      ENDIF.
    ENDIF.

    SELECT endda finum
      INTO (lv_endda, wa_kna1-stceg)
      FROM pa0331
      WHERE pernr EQ pa0002-pernr
        AND subty EQ ''
      ORDER BY endda DESCENDING.
      EXIT.
    ENDSELECT.
* NIF para a empresa 4000
    IF wa_kna1-stceg IS INITIAL AND pa0002-pernr(1) EQ '4'.
      SELECT endda icnum
        INTO (lv_endda, wa_kna1-stceg)
        FROM pa0185
        WHERE pernr EQ pa0002-pernr
          AND subty EQ '04'
        ORDER BY endda DESCENDING.
        EXIT.
      ENDSELECT.
    ENDIF.
    IF wa_kna1-stceg IS INITIAL.
      wa_kna1-stceg = '000000000'.
      wa_erro-pernr = pa0002-pernr.
      wa_erro-message = 'NIF (STCEG) criado com zeros'.
      APPEND wa_erro TO gt_erro. CLEAR wa_erro.
    ENDIF.

    SELECT endda icnum
      INTO (lv_endda, wa_kna1-stcd2)
      FROM pa0185
      WHERE pernr EQ pa0002-pernr
        AND subty EQ '01'
      ORDER BY endda DESCENDING.
      EXIT.
    ENDSELECT.
    IF wa_kna1-stcd2 IS INITIAL.
      wa_kna1-stcd2 = '000000000'.
      wa_erro-pernr = pa0002-pernr.
      wa_erro-message = 'NIF2 (STCD2) criado com zeros'.
      APPEND wa_erro TO gt_erro. CLEAR wa_erro.
    ENDIF.

    SELECT endda nrdep mstat
      INTO CORRESPONDING FIELDS OF pa0331
      FROM pa0331
      WHERE pernr EQ pa0002-pernr
        AND subty EQ ''
      ORDER BY endda DESCENDING.
      EXIT.
    ENDSELECT.
    IF sy-subrc EQ 0.
      IF NOT pa0331-mstat IS INITIAL.
        ADD 1 TO pa0331-nrdep.
      ENDIF.
      wa_kna1-katr7 = pa0331-nrdep.
    ENDIF.

    SELECT endda prcat ausgr
      INTO (lv_endda, wa_kna1-katr9, wa_kna1-katr10)
      FROM pa0337
      WHERE pernr EQ pa0002-pernr
        AND subty EQ ''
      ORDER BY endda DESCENDING.
      EXIT.
    ENDSELECT.

    wa_kna1-kunnr = pa0002-pernr.
    IF NOT pa0002-cname IS INITIAL.
      wa_kna1-name1 = pa0002-cname.
    ELSE.
      CONCATENATE pa0002-vorna pa0002-nachn pa0002-nach2
        INTO wa_kna1-name1 SEPARATED BY space.
    ENDIF.
    wa_kna1-sortl = pa0002-nachn.

* vem de HR com 1 caracter, converter para 2 caracteres
    wa_kna1-katr3 = pa0002-gesch.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_kna1-katr3
      IMPORTING
        output = wa_kna1-katr3.
    wa_kna1-katr4 = pa0002-famst.
    wa_kna1-zzbdate = pa0002-gbdat.

    IF NOT wa_kna1-land1 IS INITIAL AND NOT wa_kna1-stceg IS INITIAL.
      CONCATENATE wa_kna1-land1 wa_kna1-stceg INTO wa_kna1-stceg.
    ENDIF.

    MOVE-CORRESPONDING wa_kna1 TO wa_lfa1.
    wa_lfa1-lifnr = wa_lfa1-kunnr = wa_kna1-kunnr.

    SELECT endda banks bankl bankn bkont
      INTO (lv_endda, wa_lfbk-banks, wa_lfbk-bankl,
            wa_lfbk-bankn, wa_lfbk-bkont)
      FROM pa0009
      WHERE pernr EQ pa0002-pernr
        AND subty EQ '0'
      ORDER BY endda DESCENDING.
      EXIT.
    ENDSELECT.
    wa_lfbk-lifnr = pa0002-pernr.

    PERFORM get_values_bukrs.

    IF gv_inactivo IS INITIAL.
      APPEND wa_lfa1 TO gt_lfa1.
      CLEAR wa_lfa1.

      APPEND wa_lfbk TO gt_lfbk.
      CLEAR wa_lfbk.

      APPEND wa_kna1 TO gt_kna1.
      CLEAR wa_kna1.
    ELSE.
      CLEAR: wa_lfa1, wa_lfbk, wa_kna1,
             gv_inactivo.
    ENDIF.
  ENDSELECT.

ENDFORM.                    " GET_VALUES

*&---------------------------------------------------------------------*
*&      Form  GET_VALUES_BUKRS
*&---------------------------------------------------------------------*
FORM get_values_bukrs.

  DATA: lv_endda LIKE pa0001-endda.

  SELECT endda bukrs persg
    INTO (lv_endda, pa0001-bukrs, pa0001-persg)
    FROM pa0001
    WHERE pernr EQ pa0002-pernr
      AND subty EQ ''
    ORDER BY endda DESCENDING.
    EXIT.
  ENDSELECT.

  IF sy-subrc EQ 0 AND pa0001-persg NE '4'.
    wa_knb1-bukrs = pa0001-bukrs.
    wa_knb1-kunnr = wa_knb1-pernr = pa0002-pernr.

    wa_knb1-akont = '2111000000'.
    wa_knb1-zuawa = '001'.
    wa_knb1-fdgrv = 'C02'.
    wa_knb1-zterm = 'C001'.
    wa_knb1-xzver = 'X'.
    wa_knb1-xverr = 'X'.

    MOVE-CORRESPONDING wa_knb1 TO wa_lfb1.
    wa_lfb1-lifnr = wa_knb1-kunnr.
    IF pa0001-persg EQ '2'.
      wa_lfb1-akont = '2629100000'.
    ELSEIF pa0001-persg NE '2'.
      wa_lfb1-akont = '2629200000'.
    ENDIF.
    wa_lfb1-fdgrv = 'F01'.
    wa_lfb1-zterm = 'F000'.
    wa_lfb1-reprf = 'X'.
    wa_lfb1-zwels = 'C'.

    APPEND wa_lfb1 TO gt_lfb1.
    CLEAR wa_lfb1.

    APPEND wa_knb1 TO gt_knb1.
    CLEAR wa_knb1.
  ELSEIF pa0001-persg EQ '4'.
    gv_inactivo = 'X'.
  ENDIF.

ENDFORM.                    " GET_VALUES_BUKRS

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_VALUES
*&---------------------------------------------------------------------*
FORM validate_values.

  DATA: lv_kna1 TYPE sytabix,
        lv_knb1 TYPE sytabix,
        lv_knvv TYPE sytabix.

* validar a criacao do cliente
  LOOP AT gt_kna1 INTO wa_kna1.
    lv_kna1 = sy-tabix.
    READ TABLE gt_knb1 INTO wa_knb1 WITH KEY kunnr = wa_kna1-kunnr.
    lv_knb1 = sy-tabix.
    IF sy-subrc NE 0.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    READ TABLE gt_knvv INTO wa_knvv WITH KEY kunnr = wa_kna1-kunnr.
    lv_knvv = sy-tabix.
    IF sy-subrc NE 0.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.

    CLEAR wa_erro.
    wa_erro-pernr = wa_kna1-kunnr.

    IF wa_kna1-name1 IS INITIAL.
      wa_erro-message = 'Nome (NAME1) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_kna1-land1 IS INITIAL.
      wa_erro-message = 'Pais (LAND1) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_kna1-stcd2 IS INITIAL.
      wa_erro-message = 'N.ºID Fiscal 2 (STCD2) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_kna1-stceg IS INITIAL.
      wa_erro-message = 'N.ºID Fiscal (STCEG) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.

    IF wa_knb1-akont IS INITIAL.
      wa_erro-message =
      'Conta Reconciliação (AKONT) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knb1-erro = 'X'.
      MODIFY gt_knb1 FROM wa_knb1 INDEX lv_knb1.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knb1-zuawa IS INITIAL.
      wa_erro-message =
      'Chave de Ordenação (ZUAWA) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knb1-erro = 'X'.
      MODIFY gt_knb1 FROM wa_knb1 INDEX lv_knb1.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knb1-fdgrv IS INITIAL.
      wa_erro-message =
      'Grp.Admin.Tesouraria (ZUAWA) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knb1-erro = 'X'.
      MODIFY gt_knb1 FROM wa_knb1 INDEX lv_knb1.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knb1-pernr IS INITIAL.
      wa_erro-message = 'Nº Pessoal (PERNR) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knb1-erro = 'X'.
      MODIFY gt_knb1 FROM wa_knb1 INDEX lv_knb1.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knb1-zterm IS INITIAL.
      wa_erro-message = 'Cond. Pagamento (ZTERM) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knb1-erro = 'X'.
      MODIFY gt_knb1 FROM wa_knb1 INDEX lv_knb1.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.

    IF wa_knvv-vkbur IS INITIAL.
      wa_erro-message =
      'Escritório de Vendas (VKBUR) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-vkgrp IS INITIAL.
      wa_erro-message = 'Equipe de Vendas (VKGRP) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-kdgrp IS INITIAL.
      wa_erro-message = 'Grupo Clientes (KDGRP) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-waers IS INITIAL.
      wa_erro-message = 'Moeda (WAERS) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-kalks IS INITIAL.
      wa_erro-message = 'Esquema Cliente (KALKS) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-versg IS INITIAL.
      wa_erro-message = 'GrupoEstatCliente (VERSG) é campo obrigatório.'
      .
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-lprio IS INITIAL.
      wa_erro-message =
      'Prioridade Remessa (LPRIO) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-vsbed IS INITIAL.
      wa_erro-message =
      'Condição Expedição (VSBED) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-vwerk IS INITIAL.
      wa_erro-message = 'Centro Fornecedor (VWERK) é campo obrigatório.'
      .
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-zterm IS INITIAL.
      wa_erro-message =
      'Condições Pagamento (ZTERM) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    IF wa_knvv-ktgrd IS INITIAL.
      wa_erro-message = 'GrpClassifCont (KTGRD) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_knvv-erro = 'X'.
      MODIFY gt_knvv FROM wa_knvv INDEX lv_knvv.
      wa_kna1-erro = 'X'.
      MODIFY gt_kna1 FROM wa_kna1 INDEX lv_kna1.
    ENDIF.
    CLEAR: wa_kna1, wa_lfb1, wa_knvv.
  ENDLOOP.

ENDFORM.                    " VALIDATE_VALUES

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_VALUES_VENDOR
*&---------------------------------------------------------------------*
FORM validate_values_vendor.

  DATA: lv_lfa1 TYPE sytabix,
        lv_lfb1 TYPE sytabix.
*        lv_lfbk TYPE sytabix.

  LOOP AT gt_lfa1 INTO wa_lfa1.
    lv_lfa1 = sy-tabix.
    READ TABLE gt_lfb1 INTO wa_lfb1 WITH KEY lifnr = wa_lfa1-lifnr.
    lv_lfb1 = sy-tabix.
    IF sy-subrc NE 0.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    READ TABLE gt_lfbk INTO wa_lfbk WITH KEY lifnr = wa_lfa1-lifnr.
*    lv_lfbk = sy-tabix.
    IF sy-subrc NE 0.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.

    CLEAR wa_erro.
    wa_erro-pernr = wa_lfa1-lifnr.

    IF wa_lfa1-name1 IS INITIAL.
      wa_erro-message = 'Nome Forn. (NAME1) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    IF wa_lfa1-sortl IS INITIAL.
      wa_erro-message =
      'Termo de Pesquisa Forn. (SORTL) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    IF wa_lfa1-stras IS INITIAL.
      wa_erro-message = 'Rua/nº Forn. (STRAS) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    IF wa_lfa1-mcod3 IS INITIAL.
      wa_erro-message = 'Cidade Forn. (MCOD3) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    IF wa_lfa1-land1 IS INITIAL.
      wa_erro-message = 'Pais Forn. (LAND1) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    IF wa_lfa1-stceg IS INITIAL.
      wa_erro-message =
      'Nº ID Fiscal Forn. (STCEG) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.

    IF wa_lfb1-akont IS INITIAL.
      wa_erro-message =
      'Conta Reconciliação Forn. (AKONT) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfb1-erro = 'X'.
      MODIFY gt_lfb1 FROM wa_lfb1 INDEX lv_lfb1.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    IF wa_lfb1-zuawa IS INITIAL.
      wa_erro-message =
      'Chave de Ordenação Forn. (ZUAWA) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfb1-erro = 'X'.
      MODIFY gt_lfb1 FROM wa_lfb1 INDEX lv_lfb1.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    IF wa_lfb1-fdgrv IS INITIAL.
      wa_erro-message =
      'Grp. Adm. Tesouraria Forn. (FDGRV) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfb1-erro = 'X'.
      MODIFY gt_lfb1 FROM wa_lfb1 INDEX lv_lfb1.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    IF wa_lfb1-zterm IS INITIAL.
      wa_erro-message =
      'Condição Pagamento Forn. (ZTERM) é campo obrigatório.'.
      APPEND wa_erro TO gt_erro.
      wa_lfb1-erro = 'X'.
      MODIFY gt_lfb1 FROM wa_lfb1 INDEX lv_lfb1.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    IF wa_lfb1-reprf IS INITIAL.
      wa_erro-message =
      'Verificação Factura Duplicada Forn. (REPRF) é campo obrigatório.'
      .
      APPEND wa_erro TO gt_erro.
      wa_lfb1-erro = 'X'.
      MODIFY gt_lfb1 FROM wa_lfb1 INDEX lv_lfb1.
      wa_lfa1-erro = 'X'.
      MODIFY gt_lfa1 FROM wa_lfa1 INDEX lv_lfa1.
    ENDIF.
    CLEAR: wa_lfa1, wa_lfb1, wa_lfbk.
  ENDLOOP.

ENDFORM.                    " VALIDATE_VALUES_VENDOR

*&---------------------------------------------------------------------*
*&      Form  SORT_VALUES
*&---------------------------------------------------------------------*
FORM sort_values .

  SORT gt_kna1 BY kunnr ASCENDING.
  SORT gt_knb1 BY kunnr ASCENDING.
  SORT gt_knvv BY kunnr ASCENDING.
  SORT gt_lfa1 BY lifnr ASCENDING.
  SORT gt_lfb1 BY lifnr ASCENDING.
  SORT gt_lfbk BY lifnr ASCENDING.

ENDFORM.                    " SORT_VALUES

*&---------------------------------------------------------------------*
*&      Form  CREATE_DATA
*&---------------------------------------------------------------------*
FORM create_data .

  DATA: lv_kun   LIKE kna1-kunnr,
        lv_kna1  TYPE kna1,
        lv_knb1  TYPE knb1,
        lv_knvv  TYPE knvv,
*        lv_kunnr LIKE kna1-kunnr,
        lv_okna1 LIKE kna1.
*        lv_done  TYPE c.

  DATA: lt_t_xknvi TYPE TABLE OF fknvi INITIAL SIZE 0,
        wa_t_xknvi TYPE fknvi.

  LOOP AT gt_kna1 INTO wa_kna1 WHERE erro NE 'X'.
    CLEAR: lv_kna1, lv_knb1, lv_knvv, lv_okna1, lt_t_xknvi.
    ", lv_kunnr, lv_done.
    REFRESH lt_t_xknvi.

*** validar se o colaborador ja foi criado como cliente.
    WRITE wa_kna1-kunnr TO lv_kun.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_kun
      IMPORTING
        output = lv_kun.
    SELECT SINGLE kunnr
      INTO CORRESPONDING FIELDS OF kna1
      FROM kna1
      WHERE kunnr EQ lv_kun.
    CHECK sy-subrc NE 0.
*** validar se o colaborador ja foi criado como cliente.

    lv_kna1-mandt = lv_knb1-mandt = lv_knvv-mandt = sy-mandt.
    lv_kna1-ktokd = 'Z004'.
    MOVE-CORRESPONDING wa_kna1 TO lv_kna1.

    READ TABLE gt_knb1 INTO wa_knb1 WITH KEY kunnr = wa_kna1-kunnr.
    IF sy-subrc EQ 0 AND wa_knb1-erro IS INITIAL.
      MOVE-CORRESPONDING wa_knb1 TO lv_knb1.
    ENDIF.

    READ TABLE gt_knvv INTO wa_knvv WITH KEY kunnr = wa_kna1-kunnr.
    IF sy-subrc EQ 0 AND wa_knvv-erro IS INITIAL.
      MOVE-CORRESPONDING wa_knvv TO lv_knvv.
    ENDIF.

    lv_knvv-vkorg = lv_knb1-bukrs.
*    DO 4 TIMES.
*      CASE sy-index.
*        WHEN 1.
*          lv_knb1-bukrs = lv_knvv-vkorg = 1000.
*        WHEN 2.
*          lv_knb1-bukrs = lv_knvv-vkorg = 2000.
*        WHEN 3.
*          lv_knb1-bukrs = lv_knvv-vkorg = 3000.
*        WHEN 4.
*          lv_knb1-bukrs = lv_knvv-vkorg = 4000.
*      ENDCASE.
* idioma
    SELECT SINGLE spras
      INTO lv_kna1-spras
      FROM t001
      WHERE bukrs EQ lv_knb1-bukrs.

    wa_t_xknvi-mandt = sy-mandt.
    wa_t_xknvi-kunnr = wa_kna1-kunnr.
    wa_t_xknvi-aland = wa_kna1-land1.
    wa_t_xknvi-tatyp = 'MWST'.
    wa_t_xknvi-taxkd = '1'.
    APPEND wa_t_xknvi TO lt_t_xknvi.
    wa_t_xknvi-tatyp = 'ZREG'.
    APPEND wa_t_xknvi TO lt_t_xknvi.

    CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
      EXPORTING
        i_kna1                  = lv_kna1
        i_knb1                  = lv_knb1
        i_knvv                  = lv_knvv
      IMPORTING
*        e_kunnr                 = lv_kunnr
        o_kna1                  = lv_okna1
*        e_sd_cust_1321_done     = lv_done
      TABLES
        t_xknvi                 = lt_t_xknvi
      EXCEPTIONS
        client_error            = 1
        kna1_incomplete         = 2
        knb1_incomplete         = 3
        knb5_incomplete         = 4
        knvv_incomplete         = 5
        kunnr_not_unique        = 6
        sales_area_not_unique   = 7
        sales_area_not_valid    = 8
        insert_update_conflict  = 9
        number_assignment_error = 10
        number_not_in_range     = 11
        number_range_not_extern = 12
        number_range_not_intern = 13
        account_group_not_valid = 14
        parnr_invalid           = 15
        bank_address_invalid    = 16
        tax_data_not_valid      = 17
        no_authority            = 18
        company_code_not_unique = 19
        dunning_data_not_valid  = 20
        knb1_reference_invalid  = 21
        cam_error               = 22
        OTHERS                  = 23.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      wa_erro-pernr = wa_kna1-kunnr.
      CONCATENATE 'Cliente criado com sucesso' lv_knb1-bukrs
        INTO wa_erro-message SEPARATED BY space.
      APPEND wa_erro TO gt_erro. CLEAR wa_erro.
      COMMIT WORK.
    ENDIF.
*    ENDDO.

  ENDLOOP.

ENDFORM.                    " CREATE_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_VALUES_SALES
*&---------------------------------------------------------------------*
FORM get_values_sales.

  DATA: lv_endda LIKE pa0001-endda.

  LOOP AT gt_kna1 INTO wa_kna1.
* tabela de equivalencias para determinar a loja correcta
    SELECT endda vwerk
      INTO (lv_endda, wa_knvv-vwerk)
      FROM ytfi00013 AS y INNER JOIN pa0001 AS p
        ON p~btrtl EQ y~btrtl
      WHERE p~pernr EQ wa_kna1-kunnr
        AND p~subty EQ ''
      ORDER BY endda DESCENDING.
      EXIT.
    ENDSELECT.

    wa_knvv-kunnr = wa_kna1-kunnr.
    wa_knvv-inco2 = wa_kna1-mcod3.
    wa_knvv-kunnr = wa_kna1-kunnr.
    wa_knvv-vkbur = '1020'.
    wa_knvv-vkgrp = '201'.
    wa_knvv-vtweg = '20'.
    wa_knvv-spart = '10'.
    wa_knvv-waers = 'EUR'.
    wa_knvv-kalks = '1'.
    wa_knvv-versg = '1'.
    wa_knvv-lprio = '01'.
    wa_knvv-vsbed = 'Z3'.
    wa_knvv-prfre = 'X'.
    wa_knvv-inco1 = 'CFR'.
    wa_knvv-kdgrp = '03'.
    wa_knvv-ktgrd = '01'.
    wa_knvv-zterm = 'C001'.
    APPEND wa_knvv TO gt_knvv.
    CLEAR wa_knvv.
  ENDLOOP.

ENDFORM.                    " GET_VALUES_SALES

*&---------------------------------------------------------------------*
*&      Form  CREATE_VENDOR
*&---------------------------------------------------------------------*
FORM create_vendor.

  DATA: lv_e1lfa1m   TYPE e1lfa1m,
        lv_e1lfb1m   TYPE e1lfb1m,
        lv_e1lfm1m   TYPE e1lfm1m,
        lv_e1wyt3m   TYPE e1wyt3m,
        fwa_idoc_ctl TYPE edi_dc40,
        ft_edidd40   TYPE TABLE OF edi_dd40 INITIAL SIZE 0,
        wa_edidd40   TYPE edi_dd40.
  DATA: lv_bukrs TYPE bukrs,
        lv_ekorg TYPE ekorg,
        lv_lif   TYPE lifnr.
* Dados básicos de Idoc
  SELECT SINGLE logsys FROM t000 INTO fwa_idoc_ctl-rcvprn WHERE
                                 mandt = sy-mandt .
  CHECK sy-subrc = 0 .

  fwa_idoc_ctl-direct    =  '2'.
  fwa_idoc_ctl-idoctyp   =  'CREMAS05'.
  fwa_idoc_ctl-mestyp    =  'CREMAS'.
  fwa_idoc_ctl-sndpor    =  'SAP_EDI'.
  fwa_idoc_ctl-sndprt    =  'LS'.
  fwa_idoc_ctl-sndprn    =  'SA'.
  CONCATENATE 'SAP' sy-sysid INTO fwa_idoc_ctl-rcvpor.
  fwa_idoc_ctl-rcvprt    =  'LS'.

  LOOP AT gt_lfa1 INTO wa_lfa1 WHERE erro EQ ''.
* limpar variaveis para criacao do fornecedor
    CLEAR: lv_e1lfa1m, lv_e1lfb1m, lv_e1lfm1m.
    REFRESH: ft_edidd40.

*** validar se o colaborador ja foi criado como fornecedor
    WRITE wa_lfa1-lifnr TO lv_lif.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_lif
      IMPORTING
        output = lv_lif.
    SELECT SINGLE lifnr
      INTO CORRESPONDING FIELDS OF lfa1
      FROM lfa1
      WHERE lifnr EQ lv_lif.
    CHECK sy-subrc NE 0.
*** validar se o colaborador ja foi criado como fornecedor

* LFA1
    wa_edidd40-segnam = 'E1LFA1M'.
    lv_e1lfa1m-lifnr = wa_lfa1-lifnr.
    lv_e1lfa1m-name1 = wa_lfa1-name1.
    lv_e1lfa1m-kunnr = wa_lfa1-kunnr.
    lv_e1lfa1m-sortl = wa_lfa1-sortl.
    lv_e1lfa1m-stras = wa_lfa1-stras.
    lv_e1lfa1m-pstlz = wa_lfa1-pstlz.
    lv_e1lfa1m-ort01 = wa_lfa1-mcod3.
    lv_e1lfa1m-land1 = wa_lfa1-land1.
    lv_e1lfa1m-regio = wa_lfa1-regio.
    lv_e1lfa1m-telf1 = wa_lfa1-telf1.
    IF wa_lfa1-stceg(2) EQ 'ES'.
      wa_lfa1-stceg = 'ESA00000000'.
    ENDIF.
    lv_e1lfa1m-stceg = wa_lfa1-stceg.
    lv_e1lfa1m-ktokk = 'Z004'.
    lv_e1lfa1m-spras = wa_lfa1-land1.
    wa_edidd40-sdata = lv_e1lfa1m.
    APPEND wa_edidd40 TO ft_edidd40.

* LFB1
    READ TABLE gt_lfb1 INTO wa_lfb1 WITH KEY lifnr = wa_lfa1-lifnr.
    IF sy-subrc EQ 0.
      lv_bukrs = wa_lfb1-bukrs.
*      DO 4 TIMES.
*        CASE sy-index.
*          WHEN 1.
*            lv_bukrs = 1000.
*          WHEN 2.
*            lv_bukrs = 2000.
*          WHEN 3.
*            lv_bukrs = 3000.
*          WHEN 4.
*            lv_bukrs = 4000.
*        ENDCASE.
      wa_edidd40-segnam = 'E1LFB1M'.
      lv_e1lfb1m-lifnr = wa_lfb1-lifnr.
      lv_e1lfb1m-bukrs = lv_bukrs.
      lv_e1lfb1m-akont = wa_lfb1-akont.
      lv_e1lfb1m-zuawa = wa_lfb1-zuawa.
      lv_e1lfb1m-fdgrv = wa_lfb1-fdgrv.
      lv_e1lfb1m-pernr = wa_lfb1-pernr.
      lv_e1lfb1m-zterm = wa_lfb1-zterm.
      lv_e1lfb1m-reprf = wa_lfb1-reprf.
      lv_e1lfb1m-zwels = wa_lfb1-zwels.
      lv_e1lfb1m-xverr = 'X'.
      wa_edidd40-sdata = lv_e1lfb1m.
      APPEND wa_edidd40 TO ft_edidd40.
*      ENDDO.
    ENDIF.

    READ TABLE gt_lfb1 INTO wa_lfb1 WITH KEY lifnr = wa_lfa1-lifnr.
    IF sy-subrc EQ 0.
      lv_ekorg = wa_lfb1-bukrs.
*      DO 4 TIMES.
*        CASE sy-index.
*          WHEN 1.
*            lv_ekorg = 1000.
*          WHEN 2.
*            lv_ekorg = 2000.
*          WHEN 3.
*            lv_ekorg = 3000.
*          WHEN 4.
*            lv_ekorg = 4000.
*        ENDCASE.
      wa_edidd40-segnam = 'E1LFM1M'.
      lv_e1lfm1m-lifnr = wa_lfb1-lifnr.
      lv_e1lfm1m-ekorg = lv_ekorg.
*        lv_e1lfm1m-zterm = wa_lfb1-zterm.
      lv_e1lfm1m-waers = 'EUR'.
      wa_edidd40-sdata = lv_e1lfm1m.
      APPEND wa_edidd40 TO ft_edidd40.
*      ENDDO.
    ENDIF.

* dados para a funcao de parceiro
*    DO 4 TIMES.
*      CASE sy-index.
*        WHEN 1.
*          lv_ekorg = 1000.
*        WHEN 2.
*          lv_ekorg = 2000.
*        WHEN 3.
*          lv_ekorg = 3000.
*        WHEN 4.
*          lv_ekorg = 4000.
*      ENDCASE.
    wa_edidd40-segnam = 'E1WYT3M'.
    lv_e1wyt3m-lifnr = wa_lfb1-lifnr.
    lv_e1wyt3m-ekorg = lv_ekorg.
    lv_e1wyt3m-parvw = 'EP'.
    lv_e1wyt3m-lifn2 = wa_lfb1-lifnr.
    lv_e1wyt3m-ernam = sy-uname.
    lv_e1wyt3m-erdat = sy-datum.
    wa_edidd40-sdata = lv_e1wyt3m.
    APPEND wa_edidd40 TO ft_edidd40.
    lv_e1wyt3m-parvw = 'FO'.
    wa_edidd40-sdata = lv_e1wyt3m.
    APPEND wa_edidd40 TO ft_edidd40.
    lv_e1wyt3m-parvw = 'EF'.
    wa_edidd40-sdata = lv_e1wyt3m.
    APPEND wa_edidd40 TO ft_edidd40.
*    ENDDO.

    IF NOT ft_edidd40[] IS INITIAL .
      CALL FUNCTION 'IDOC_INBOUND_SINGLE'
        EXPORTING
          pi_idoc_control_rec_40  = fwa_idoc_ctl
        TABLES
          pt_idoc_data_records_40 = ft_edidd40
        EXCEPTIONS
          idoc_not_saved          = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        wa_erro-pernr = wa_lfa1-lifnr.
        CONCATENATE 'Fornecedor criado com sucesso' wa_lfb1-bukrs
          INTO wa_erro-message SEPARATED BY space.
        APPEND wa_erro TO gt_erro. CLEAR wa_erro.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CREATE_VENDOR

*&---------------------------------------------------------------------*
*&      Form  LIST_VALUES
*&---------------------------------------------------------------------*
FORM list_values.

  PERFORM layout_init USING gs_layout.
  PERFORM fill_fieldcat USING gs_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gs_fieldcat
    TABLES
      t_outtab           = gt_erro[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " LIST_VALUES

*&---------------------------------------------------------------------*
*&      Form  layout_init
*&---------------------------------------------------------------------*
FORM layout_init USING rs_layout TYPE slis_layout_alv.

  rs_layout-detail_popup      = 'X'.
  rs_layout-zebra             = 'X'.
  rs_layout-get_selinfos      = 'X'.
  rs_layout-colwidth_optimize = 'X'.

ENDFORM.                    "layout_init

*&---------------------------------------------------------------------*
*&      Form  fill_fieldcat
*&---------------------------------------------------------------------*
FORM fill_fieldcat USING gs_fieldcat TYPE slis_t_fieldcat_alv.

  DATA wa_fieldcat LIKE LINE OF gs_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'GT_ERRO'
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = gs_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT gs_fieldcat INTO wa_fieldcat.
    wa_fieldcat-key = ' '.
    CASE wa_fieldcat-fieldname.
      WHEN 'PERNR'.
        wa_fieldcat-seltext_l = 'Colaborador'.
        wa_fieldcat-seltext_m = 'Colaborador'.
        wa_fieldcat-seltext_s = 'Colaborador'.
        CLEAR: wa_fieldcat-ref_tabname,
               wa_fieldcat-reptext_ddic.
      WHEN 'MESSAGE'.
        wa_fieldcat-seltext_l = 'Mensagem'.
        wa_fieldcat-seltext_m = 'Mensagem'.
        wa_fieldcat-seltext_s = 'Mensagem'.
        CLEAR: wa_fieldcat-ref_tabname,
               wa_fieldcat-reptext_ddic.
    ENDCASE.
    MODIFY gs_fieldcat FROM wa_fieldcat.
  ENDLOOP.

ENDFORM.                    " fill_fieldcat
