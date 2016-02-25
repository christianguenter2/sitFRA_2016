*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZRSTPDA_SCRIPT_STATEMENT_BREAK</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Debugger Skript: Default Template</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super.
  PUBLIC SECTION.
    METHODS: prologue  REDEFINITION,
      init      REDEFINITION,
      script    REDEFINITION,
      end       REDEFINITION.

  PRIVATE SECTION.

    DATA: filter     TYPE tpda_range_it,
          returncode TYPE char01.
ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.

  METHOD prologue.
  ENDMETHOD.                    "prolog

  METHOD init.

    DATA: fields TYPE STANDARD TABLE OF sval.

    INSERT VALUE #( tabname   = 'DD03L'
                    fieldname = 'FIELDNAME'
                    fieldtext = 'Pattern'(001)
                    field_obl = abap_true )
           INTO TABLE fields
           ASSIGNING FIELD-SYMBOL(<filter>).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = TEXT-001    " Text der Titelzeile
      IMPORTING
        returncode      = returncode    " Antwort des Anwenders
      TABLES
        fields          = fields    " Tabellenfelder, Werte und Attribute
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    CHECK sy-subrc = 0.

    INSERT VALUE #( sign   = 'I'
                    option = 'CP'
                    low    = |*{ <filter>-value }*| ) INTO TABLE filter.
  ENDMETHOD.                    "init

  METHOD script.
    DATA: source_info TYPE tpda_curr_source_pos,
          source      TYPE tpda_ast_src_it.

    CHECK: returncode IS INITIAL,
           lines( filter ) > 0.

    TRY.
        cl_tpda_script_abapdescr=>get_abap_src_info(
           IMPORTING
             p_prg_info  = source_info-prg_info
             p_dynp_info = source_info-dynp_info ).
      CATCH cx_tpda_src_info.
        CLEAR source_info.
    ENDTRY.

    zcl_bc_debugger_scripts=>get_source_info_extended(
      EXPORTING
        i_source_info  = source_info    " TPDA; Sttaische Source Position für Navigation
        i_filter       = filter    " TPDA: Generische Range Tabelle
      CHANGING
        ct_source      = source ).

    IF lines( source ) > 0.
      me->break( ).
    ENDIF.
  ENDMETHOD.                    "script

  METHOD end.
  ENDMETHOD.                    "end

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>