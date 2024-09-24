*&---------------------------------------------------------------------*
*& Include          ZI_CLASSES
*&---------------------------------------------------------------------*
*Definições:
*lcl_rfc
CLASS lcl_rfc DEFINITION.
  PUBLIC SECTION.
    METHODS:
      chamar_rfc
        IMPORTING
          i_periodo TYPE  gjahr
          i_empresa TYPE  bukrs,
      display_pop_up.

  PROTECTED SECTION.
    METHODS:
      popula_it_options,
      popula_it_fields,
      append_tabela_z,
      show_log,
      atualiza_bseg,
      trata_r_beseg,
      reculpera_v_skip.

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

    DATA: r_bseg             TYPE TABLE OF /bods/tab2048,
          it_bseg            TYPE TABLE OF bseg,
          it_options         TYPE TABLE OF rfc_db_opt,
          it_log             TYPE TABLE OF ty_log,
          it_log_tab_z       TYPE TABLE OF ty_log_tab_z,
          it_fields          TYPE TABLE OF rfc_db_fld,
          wa_fields          TYPE rfc_db_fld,
          empresa            TYPE bukrs,
          periodo            TYPE gjahr,
          v_count            TYPE soid-accnt VALUE 200,
          v_skip             TYPE soid-accnt VALUE 0,
          v_total            TYPE soid-accnt,
          v_num_loop         TYPE soid-accnt,
          v_show_log_z       TYPE c VALUE 'X',
          it_fields_bseg     TYPE TABLE OF dfies,
          it_fields_bseg_aux TYPE TABLE OF dfies.
ENDCLASS.

*lcl_tela
CLASS lcl_tela DEFINITION.
  PUBLIC SECTION.
    METHODS: modifica_tela
      IMPORTING
        i_rfc TYPE c
        i_upd TYPE c.
ENDCLASS.


*Implementações:
*lcl_tela
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

*lcl_rfc
CLASS lcl_rfc IMPLEMENTATION.
  "Reculpera o valor de v_skip caso a execução do programa seja interrompida antes do loop acabar
  METHOD reculpera_v_skip.

    SELECT COUNT(*)
    FROM ZBSEG
    INTO v_skip
      WHERE BUKRS EQ me->empresa AND
            GJAHR EQ me->periodo.

  ENDMETHOD.

  "Faz o tratamento da R_BSEG (dados advindos da RFC)
  METHOD trata_r_beseg.

    DATA obj_data TYPE REF TO data.

    CREATE DATA obj_data TYPE TABLE OF ('BSEG').
    FIELD-SYMBOLS : <fs_table> TYPE STANDARD TABLE.
    ASSIGN obj_data->* TO <fs_table>.
    FREE : obj_data.

    CREATE DATA obj_data TYPE ('BSEG').

    FIELD-SYMBOLS : <fs_wa> TYPE any.
    ASSIGN obj_data->* TO <fs_wa>.

    DATA :wa_bseg LIKE LINE OF r_bseg.
    FIELD-SYMBOLS : <lv_field> TYPE any.
    FIELD-SYMBOLS : <fs> TYPE /bods/tab2048.

    DATA lv_fields_bseg TYPE i.
    DESCRIBE TABLE it_fields_bseg_aux LINES lv_fields_bseg.

    LOOP AT r_bseg INTO wa_bseg.

      DO lv_fields_bseg TIMES.
        READ TABLE it_fields INTO DATA(ls_fld) INDEX sy-index.
        IF sy-subrc IS INITIAL.
          ASSIGN COMPONENT ls_fld-fieldname OF STRUCTURE <fs_wa> TO <lv_field>.

          IF <lv_field> IS ASSIGNED AND sy-subrc IS INITIAL.

            READ TABLE it_fields_bseg_aux INTO DATA(w_fields_ddic) WITH KEY fieldname = ls_fld-fieldname.
            IF sy-subrc EQ 0.
              IF w_fields_ddic-datatype EQ 'TIMS'.
                DATA lv_tmp_field    TYPE char8.
                MOVE :  wa_bseg-wa+ls_fld-offset(ls_fld-length) TO lv_tmp_field.
                REPLACE ALL OCCURRENCES OF ':' IN lv_tmp_field WITH ''.
                MOVE: lv_tmp_field TO <lv_field>.
              ELSE.
                MOVE:  wa_bseg-wa+ls_fld-offset(ls_fld-length) TO <lv_field>.
              ENDIF.
            ELSE.
              MOVE: wa_bseg-wa+ls_fld-offset(ls_fld-length) TO <lv_field>.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
      APPEND : <fs_wa> TO <fs_table>.
      CLEAR : wa_bseg-wa.
    ENDLOOP.

    APPEND LINES OF <fs_table> TO it_bseg.

  ENDMETHOD.

  "Faz o select da BSEG via RFC
  METHOD chamar_rfc.

    me->empresa = i_empresa.
    me->periodo = i_periodo.

    me->popula_it_options( ).
    me->popula_it_fields( ).
    me->reculpera_v_skip( ).

    DO.
      CLEAR r_bseg.
      v_num_loop = v_num_loop + 1.

*      CALL FUNCTION 'RFC_READ_TABLE' DESTINATION 'Z_ECC_PRD'
      CALL FUNCTION '/BODS/RFC_READ_TABLE' DESTINATION 'Z_ECC_PRD'
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
        me->trata_r_beseg( ).
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

    CONCATENATE 'BUKRS = ''' empresa '''' INTO wa_options.
    APPEND wa_options TO me->it_options.
    CONCATENATE 'AND GJAHR = ''' periodo ''''  INTO wa_options.
    APPEND wa_options TO me->it_options.
    CLEAR wa_options.

  ENDMETHOD.

  "Passa os campos da BSEG que deverão ser trazidos na hora da seleção
  METHOD popula_it_fields.

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
        "Filtra apenas os campos que iniciam com Z e campos chave da BSEG para o append
        IF wa_fields_bseg-fieldname+0(1) EQ 'Z' OR wa_fields_bseg-fieldname EQ 'BUKRS'
        OR wa_fields_bseg-fieldname EQ 'BELNR' OR wa_fields_bseg-fieldname EQ 'GJAHR'
        OR wa_fields_bseg-fieldname EQ 'BUZEI' OR wa_fields_bseg-fieldname EQ 'MANDT'.
          wa_fields-fieldname = wa_fields_bseg-fieldname.
          APPEND wa_fields TO it_fields.
          CLEAR wa_fields.

          APPEND wa_fields_bseg TO it_fields_bseg_aux.

        ENDIF.
        CLEAR wa_fields_bseg.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  "Appenda os campos Z da BSEG numa tabela Z
  METHOD append_tabela_z.
    DATA wa_log_tab_z LIKE LINE OF it_log_tab_z.

    MODIFY zbseg FROM TABLE it_bseg.

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

    CLEAR it_bseg[].

  ENDMETHOD.

  "Chama pop up de confirmação
  METHOD display_pop_up.
    DATA: lv_resposta TYPE c.

    "Mostra pop up de confirmação
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Essa ação poderá modificar dados da tabela BSEG, tem certeza que deseja continuar?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = 'X'
      IMPORTING
        answer                = lv_resposta.

    IF lv_resposta = '1'.
      "Sim
      me->atualiza_bseg( ).
    ELSE.
      " Não
      EXIT.
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

      LOOP AT it_zbseg INTO DATA(wa_zbseg).

          "Atualiza a linha da BSEG onde as chaves forem iguais as de wa_zbseg
          UPDATE bseg
            SET zumsk = wa_zbseg-zumsk
                zuonr = wa_zbseg-zuonr
                zinkz = wa_zbseg-zinkz
                zfbdt = wa_zbseg-zfbdt
                zterm = wa_zbseg-zterm
                zbd1t = wa_zbseg-zbd1t
                zbd2t = wa_zbseg-zbd2t
                zbd3t = wa_zbseg-zbd3t
                zbd1p = wa_zbseg-zbd1p
                zbd2p = wa_zbseg-zbd2p
                zlsch = wa_zbseg-zlsch
                zlspr = wa_zbseg-zlspr
                zbfix = wa_zbseg-zbfix
                zollt = wa_zbseg-zollt
                zolld = wa_zbseg-zolld
                zekkn = wa_zbseg-zekkn
                zzcta_corr = wa_zbseg-zzcta_corr
                zztp_desp = wa_zbseg-zztp_desp
                zzsegrega = wa_zbseg-zzsegrega
                zzhisto = wa_zbseg-zzhisto
                zztpsgt = wa_zbseg-zztpsgt
                zzfil_risc = wa_zbseg-zzfil_risc
                zzcorretor = wa_zbseg-zzcorretor
                zztomador = wa_zbseg-zztomador
                zzcongener = wa_zbseg-zzcongener
                zzproduto = wa_zbseg-zzproduto
                zzbusiness = wa_zbseg-zzbusiness
                zzfonte = wa_zbseg-zzfonte
                zznmfav = wa_zbseg-zznmfav
                zzbanks = wa_zbseg-zzbanks
                zzbankl = wa_zbseg-zzbankl
                zzbankn = wa_zbseg-zzbankn
                zzbkont = wa_zbseg-zzbkont
                zzcgebenef = wa_zbseg-zzcgebenef
                zzfinalted = wa_zbseg-zzfinalted
                zzcodtrans = wa_zbseg-zzcodtrans
                zzcgereque = wa_zbseg-zzcgereque
                zzswiftben = wa_zbseg-zzswiftben
                zzababenef = wa_zbseg-zzababenef
                zzswiftint = wa_zbseg-zzswiftint
                zzabainter = wa_zbseg-zzabainter
                zznominter = wa_zbseg-zznominter
                zzconinter = wa_zbseg-zzconinter
                zzrefinvoi = wa_zbseg-zzrefinvoi
                zzconbanco = wa_zbseg-zzconbanco
                zzanomegps = wa_zbseg-zzanomegps
                zznumdarf = wa_zbseg-zznumdarf
                zzcodrecei = wa_zbseg-zzcodrecei
                zzdttesour = wa_zbseg-zzdttesour
                zzdtventri = wa_zbseg-zzdtventri
                zzcodidcon = wa_zbseg-zzcodidcon
                zzidcontri = wa_zbseg-zzidcontri
                zzfaedt = wa_zbseg-zzfaedt
                zzmonli = wa_zbseg-zzmonli
                zzmonliint = wa_zbseg-zzmonliint
                zzvlinss = wa_zbseg-zzvlinss
                zzvlencarg = wa_zbseg-zzvlencarg
                zzvloutra = wa_zbseg-zzvloutra
                zzwfapini = wa_zbseg-zzwfapini
                zzwfaprep = wa_zbseg-zzwfaprep
                zzcomit = wa_zbseg-zzcomit
                zzgrossit = wa_zbseg-zzgrossit
                zzlivro = wa_zbseg-zzlivro
                zznit = wa_zbseg-zznit
                zzimp = wa_zbseg-zzimp
                zzndoc = wa_zbseg-zzndoc
          WHERE bukrs = wa_zbseg-bukrs AND
                belnr = wa_zbseg-belnr AND
                gjahr = wa_zbseg-gjahr AND
                buzei = wa_zbseg-buzei.

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
