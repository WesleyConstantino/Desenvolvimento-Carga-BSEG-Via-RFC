*&---------------------------------------------------------------------*
*& Include          ZI_CLASSES
*&---------------------------------------------------------------------*
*Definições:
CLASS lcl_rfc DEFINITION.
  PUBLIC SECTION.
    METHODS:
      chamar_rfc
        IMPORTING
          i_periodo TYPE  gjahr
          i_empresa TYPE  bukrs,
      atualiza_bseg.

  PROTECTED SECTION.
    METHODS:
      popula_it_options,
      append_tabela_z,
      show_log.

    DATA: r_bseg     TYPE TABLE OF bseg,
          it_options TYPE TABLE OF rfc_db_opt,
          empresa    TYPE bukrs,
          periodo    TYPE gjahr.
ENDCLASS.


*Implementações:
CLASS lcl_rfc IMPLEMENTATION.
  "Faz o select da BSEG via RFC
  METHOD chamar_rfc.

    me->empresa = i_empresa.
    me->periodo = i_periodo.

    me->popula_it_options( ).

    CALL FUNCTION 'RFC_READ_TABLE' DESTINATION 'Z_ECC_PRD'
      EXPORTING
        query_table          = 'BSEG'
*       ROWSKIPS             = 0
*       ROWCOUNT             = 0
        get_sorted           = 'X'
      TABLES
       OPTIONS              = it_options
*       FIELDS               =
        data                 = r_bseg
      EXCEPTIONS
        table_not_available  = 1
        table_without_data   = 2
        option_not_valid     = 3
        field_not_valid      = 4
        not_authorized       = 5
        data_buffer_exceeded = 6
        OTHERS               = 7.

    IF sy-subrc <> 0.

    ENDIF.

  ENDMETHOD.

  "Passa as os filtros da cláusula WHERE para it_options
  METHOD popula_it_options.
    DATA wa_options TYPE rfc_db_opt.

    CONCATENATE 'BUKERS =' empresa 'AND GJAHR =' periodo  INTO wa_options SEPARATED BY space.
    APPEND wa_options TO me->it_options.
    CLEAR wa_options.

  ENDMETHOD.

  "Appenda os campos Z da BSEG numa tabela Z
  METHOD append_tabela_z.

  ENDMETHOD.

  "Insere os dados dos campos Z na BSEG
  METHOD atualiza_bseg.

  ENDMETHOD.

  "Mostra log com registros que foram alterados
  METHOD show_log.

  ENDMETHOD.
ENDCLASS.
