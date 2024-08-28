*&---------------------------------------------------------------------*
*& Report Z_CARGA_BSEG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_carga_bseg.

*Includes:
INCLUDE zi_top.
INCLUDE zi_classes.
INCLUDE zi_tela.

"Evento de cliques

AT SELECTION-SCREEN OUTPUT.

  DATA ol_tela TYPE REF TO lcl_tela.
  CREATE OBJECT ol_tela.
  ol_tela->modifica_tela( i_rfc = rb_rfc i_upd = rb_upd ).

*Início da execução
START-OF-SELECTION.
  CASE rb_rfc.
    WHEN 'X'.
      IF p_empr IS INITIAL OR p_peri IS INITIAL.
        MESSAGE s398(00) WITH 'Preencha todos os parâmetros!' DISPLAY LIKE 'E'.
      ELSE.
        DATA ol_rfc TYPE REF TO lcl_rfc.
        CREATE OBJECT ol_rfc.
        "Chama RFC e faz seleção na BSEG
        ol_rfc->chamar_rfc( i_periodo = p_peri i_empresa = p_empr ).
      ENDIF.
    WHEN ' '.
      "Atualiza a BSEG
      ol_rfc->display_pop_up( ).
  ENDCASE.
