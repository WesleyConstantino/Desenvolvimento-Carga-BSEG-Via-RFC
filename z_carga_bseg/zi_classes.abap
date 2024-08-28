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
        mensagem TYPE char50,
        bukrs    TYPE bukrs,
        belnr    TYPE belnr,
        gjahr    TYPE gjahr,
        buzei    TYPE buzei,
      END OF ty_log,

      BEGIN OF ty_log_tab_z,
        mensagem(50) TYPE c,
        num_loop(20) TYPE c,
      END OF ty_log_tab_z.

    DATA: r_bseg       TYPE TABLE OF bseg,
          it_options   TYPE TABLE OF rfc_db_opt,
          it_log       TYPE TABLE OF ty_log,
          it_log_tab_z TYPE TABLE OF ty_log_tab_z,
          it_fields    TYPE TABLE OF rfc_db_fld,
          wa_fields    TYPE rfc_db_fld,
          empresa      TYPE bukrs,
          periodo      TYPE gjahr,
          v_count      TYPE c VALUE 200,
          v_skip       TYPE c VALUE 0,
          v_total      TYPE c,
          v_num_loop   TYPE c,
          v_show_log_z TYPE c VALUE 'X'.
ENDCLASS.


CLASS lcl_tela DEFINITION.
  PUBLIC SECTION.
    METHODS: modifica_tela
    IMPORTING
      i_rfc TYPE c
      i_upd TYPE c.
ENDCLASS.


*Implementações:
CLASS lcl_tela IMPLEMENTATION.
  "Modifica tela de seleção de acordo com os raiobuttons
  METHOD modifica_tela.
      LOOP AT SCREEN.
    "RFC
    IF i_rfc EQ 'X'.
      IF screen-group1 EQ 'RFC'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ENDIF.
    ENDIF.

    "UPD
    IF i_upd EQ 'X'.
      IF screen-group1 EQ 'RFC'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_rfc IMPLEMENTATION.
  "Faz o select da BSEG via RFC
  METHOD chamar_rfc.

    me->empresa = i_empresa.
    me->periodo = i_periodo.

    me->popula_it_options( ).
    me->popula_it_fields( ).

    DO.
      CLEAR r_bseg.
      v_num_loop = v_num_loop + 1.

      CALL FUNCTION 'RFC_READ_TABLE' DESTINATION 'Z_ECC_PRD'
        EXPORTING
          query_table          = 'BSEG'
          rowskips             = v_skip
          rowcount             = v_count
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
        MESSAGE 'Não foi possível acessar os dados pela RFC.' TYPE 'E'.
        EXIT.
      ENDIF.

      IF r_bseg IS NOT INITIAL.
        me->append_tabela_z( ).
      ENDIF.

      "Sai do loop se o número de registros retornados for menor que 200 (último lote)
      DESCRIBE TABLE r_bseg LINES v_total.
      IF v_total < v_count.
        EXIT.
      ENDIF.

    ENDDO.

    me->show_log( ).

  ENDMETHOD.

  "Passa as os filtros da cláusula WHERE para it_options
  METHOD popula_it_options.
    DATA wa_options TYPE rfc_db_opt.

    CONCATENATE 'BUKRS =' empresa 'AND GJAHR =' periodo  INTO wa_options SEPARATED BY space.
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
    DATA wa_log_tab_z LIKE LINE OF it_log_tab_z.

    INSERT zbseg FROM TABLE r_bseg.

    IF sy-subrc = 0.
      "Incrementa o número de registros a pular
      ADD v_count TO v_skip.

      COMMIT WORK.
      wa_log_tab_z-mensagem = 'Dados inseridos com sucesso na tabela ZBSEG!'.
      wa_log_tab_z-num_loop = v_num_loop.
      APPEND wa_log_tab_z TO it_log_tab_z.
      CLEAR wa_log_tab_z.
    ELSE.
      ROLLBACK WORK.
      wa_log_tab_z-mensagem = 'Erro ao inserir dados na tabela ZBSEG!'.
      wa_log_tab_z-num_loop = v_num_loop.
      APPEND wa_log_tab_z TO it_log_tab_z.
      CLEAR wa_log_tab_z.
    ENDIF.

  ENDMETHOD.

  "Insere os dados dos campos Z na BSEG
  METHOD atualiza_bseg.
    DATA wa_log LIKE LINE OF it_log.
    v_show_log_z = ' '.

    "Seleciona todos os registros da ZBSEG
    SELECT * FROM zbseg
      INTO TABLE @DATA(it_zbseg).

    IF it_zbseg IS NOT INITIAL AND sy-subrc EQ 0.
      "Seleciona todos os registros da BSEG
      SELECT * FROM bseg
       INTO TABLE @DATA(it_bseg).

      LOOP AT it_zbseg INTO DATA(wa_zbseg).
        "Verifica se o registro da linha atual de ZBSEG existe na BSEG
        READ TABLE it_bseg WITH KEY bukrs = wa_zbseg-bukrs
                                    belnr = wa_zbseg-belnr
                                    gjahr = wa_zbseg-gjahr
                                    buzei = wa_zbseg-buzei
                    TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          "Modifica a linha da BSEG onde as chaves forem iguais as de wa_zbseg
          MODIFY bseg FROM wa_zbseg.

          IF sy-subrc = 0.
            COMMIT WORK.
            wa_log-mensagem = 'Linha atualizada com sucesso na tabela BSEG!'.
            wa_log-bukrs = wa_zbseg-bukrs.
            wa_log-belnr = wa_zbseg-belnr.
            wa_log-gjahr = wa_zbseg-gjahr.
            wa_log-buzei = wa_zbseg-buzei.
            APPEND wa_log TO it_log.
            CLEAR wa_log.
          ELSE.
            ROLLBACK WORK.
            wa_log-mensagem = 'Erro ao tentar atualizar linha na tabela BSEG!'.
            wa_log-bukrs = wa_zbseg-bukrs.
            wa_log-belnr = wa_zbseg-belnr.
            wa_log-gjahr = wa_zbseg-gjahr.
            wa_log-buzei = wa_zbseg-buzei.
            APPEND wa_log TO it_log.
            CLEAR wa_log.
          ENDIF.

        ELSE.
          CONTINUE.
        ENDIF.

      ENDLOOP.
      me->show_log( ).
    ELSE.
      MESSAGE 'Não é possível fazer a carga na BSEG, pois nenhum registro foi encontrado na ZBSEG!.' TYPE 'E'.
    ENDIF.

  ENDMETHOD.

  "Mostra log com registros que foram alterados
  METHOD show_log.
    DATA: lo_table   TYPE REF TO cl_salv_table,
          lo_header  TYPE REF TO cl_salv_form_layout_grid,
          lo_columns TYPE REF TO cl_salv_columns_table.

    TRY.
        IF v_show_log_z IS NOT INITIAL.
          cl_salv_table=>factory( IMPORTING r_salv_table = lo_table
                                  CHANGING t_table = it_log_tab_z ).
        ELSE.
          cl_salv_table=>factory( IMPORTING r_salv_table = lo_table
                                  CHANGING t_table = it_log ).
        ENDIF.

        "Ativa os met codes
        lo_table->get_functions( )->set_all( abap_true ).

        "Mudar nome das colunas do ALV
        lo_table->get_columns( )->get_column( 'MENSAGEM' )->set_short_text( 'Msg.' ).
        lo_table->get_columns( )->get_column( 'MENSAGEM' )->set_medium_text( 'Mensagem' ).
        lo_table->get_columns( )->get_column( 'MENSAGEM' )->set_long_text( 'Mensagem' ).

        IF v_show_log_z IS NOT INITIAL.
          "Mudar nome das colunas do ALV
          lo_table->get_columns( )->get_column( 'NUM_LOOP' )->set_short_text( 'Núm. Loop' ).
          lo_table->get_columns( )->get_column( 'NUM_LOOP' )->set_medium_text( 'Número do loop' ).
          lo_table->get_columns( )->get_column( 'NUM_LOOP' )->set_long_text( 'Número do loop' ).
        ENDIF.

        CREATE OBJECT lo_header.

        "título do header
        lo_header->create_header_information( row = 1 column = 1 text = 'Log' ).

        lo_header->add_row( ).

        lo_table->get_display_settings( )->set_striped_pattern( abap_true ).

        lo_table->set_top_of_list( lo_header ).

        lo_columns = lo_table->get_columns( ). "Ajusta tamanho dos subtítulos
        lo_columns->set_optimize( abap_true ). "Ajusta tamanho dos subtítulos

        "Exibe o ALV
        lo_table->display( ) .

      CATCH cx_salv_msg
            cx_root.
        MESSAGE s398(00) WITH 'Erro ao exibir tabela de log!' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
