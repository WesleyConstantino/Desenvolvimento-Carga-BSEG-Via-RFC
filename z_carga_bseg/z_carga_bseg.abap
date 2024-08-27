*&---------------------------------------------------------------------*
*& Report Z_CARGA_BSEG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_carga_bseg.

*Includes:
INCLUDE zi_top.
INCLUDE zi_classes.

*Tela de seleção:
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_empr TYPE bukrs,
              p_peri TYPE gjahr.
SELECTION-SCREEN END OF BLOCK b1.

*Início da execução
START-OF-SELECTION.
  IF p_empr IS INITIAL OR p_peri IS INITIAL.
    MESSAGE s398(00) WITH 'Preencha todos os parâmetros!' DISPLAY LIKE 'E'.
  ELSE.
    DATA ol_rfc TYPE REF TO lcl_rfc.
    CREATE OBJECT ol_rfc.
    "Chama RFC e faz seleção na BSEG
    ol_rfc->chamar_rfc( i_periodo = p_peri i_empresa = p_empr ).

    "Atualiza a BSEG
    "ol_rfc->atualiza_bseg( ).
  ENDIF.
