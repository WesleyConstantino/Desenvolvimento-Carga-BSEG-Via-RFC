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
      popula_it_fields,
      append_tabela_z,
      show_log.

    TYPES:
      BEGIN OF ty_log,
        campo1 TYPE char20,
        campo2 TYPE char20,
        campo3 TYPE char20,
      END OF ty_log.

    DATA: r_bseg     TYPE TABLE OF bseg,
          it_options TYPE TABLE OF rfc_db_opt,
          it_log     TYPE TABLE OF ty_log,
          it_fields  TYPE TABLE OF rfc_db_fld,
          wa_fields  TYPE rfc_db_fld,
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
    me->popula_it_fields( ).

    CALL FUNCTION 'RFC_READ_TABLE' DESTINATION 'Z_ECC_PRD'
      EXPORTING
        query_table          = 'BSEG'
*       ROWSKIPS             = 0
*       ROWCOUNT             = 0
        get_sorted           = 'X'
      TABLES
        options              = it_options
        fields               = it_fields
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
      EXIT.
    ENDIF.

    IF r_bseg IS NOT INITIAL.
      me->append_tabela_z( ).
    ENDIF.

  ENDMETHOD.

  "Passa as os filtros da cláusula WHERE para it_options
  METHOD popula_it_options.
    DATA wa_options TYPE rfc_db_opt.

    CONCATENATE 'BUKERS =' empresa 'AND GJAHR =' periodo  INTO wa_options SEPARATED BY space.
    APPEND wa_options TO me->it_options.
    CLEAR wa_options.

  ENDMETHOD.


  "Passa os campos da BSEG que deverão ser trazidos na hora da seleção
  METHOD popula_it_fields.
    DATA it_fields_bseg  TYPE TABLE OF dfies.

    "Recupera todos os campos da BSEG
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = 'BSEG'
      TABLES
        dfies_tab      = it_fields_bseg
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    IF sy-subrc EQ 0.
      "Appenda o nome de todos os campos na it_fields
      LOOP AT it_fields_bseg INTO DATA(wa_fields_bseg).
        wa_fields-fieldname = wa_fields_bseg-fieldname.
        APPEND wa_fields TO it_fields.
        CLEAR: wa_fields,
               wa_fields_bseg.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  "Appenda os campos Z da BSEG numa tabela Z
  METHOD append_tabela_z.

    INSERT zbseg FROM TABLE r_bseg.

    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE 'Dados inseridos com sucesso na tabela ZBSEG.' TYPE 'S'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE 'Erro ao inserir dados na tabela ZBSEG.' TYPE 'E'.
    ENDIF.

  ENDMETHOD.

  "Insere os dados dos campos Z na BSEG
  METHOD atualiza_bseg.

  ENDMETHOD.

  "Mostra log com registros que foram alterados
  METHOD show_log.
    DATA: lo_table   TYPE REF TO cl_salv_table,
          lo_header  TYPE REF TO cl_salv_form_layout_grid,
          lo_columns TYPE REF TO cl_salv_columns_table.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table
                                CHANGING t_table = it_log ).

        lo_table->get_functions( )->set_all( abap_true ). "Ativar met codes

        "Mudar nome das colunas do ALV
        lo_table->get_columns( )->get_column( 'CAMPO1' )->set_short_text( 'Campo 1' ).
        lo_table->get_columns( )->get_column( 'CAMPO1' )->set_medium_text( 'Campo 1' ).
        lo_table->get_columns( )->get_column( 'CAMPO1' )->set_long_text( 'Campo 1' ).

        CREATE OBJECT lo_header.

        "título do header
        lo_header->create_header_information( row = 1 column = 1 text = 'Log de dados alterados' ).

        lo_header->add_row( ).

        lo_table->get_display_settings( )->set_striped_pattern( abap_true ).

        lo_table->set_top_of_list( lo_header ).

        lo_columns = lo_table->get_columns( ). "Ajustar tamanho dos subtítulos
        lo_columns->set_optimize( abap_true ). "Ajustar tamanho dos subtítulos

        lo_table->display( ) . "O dispay é fundamental para a exibição do ALV

      CATCH cx_salv_msg
            cx_root.
        MESSAGE s398(00) WITH 'Erro ao exibir tabela de log!' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
