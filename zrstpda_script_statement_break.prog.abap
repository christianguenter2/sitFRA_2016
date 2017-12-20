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

CLASS lcl_source_code_info DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_source_info_extended
        IMPORTING
          !i_source_info TYPE tpda_curr_source_pos
          !i_filter      TYPE tpda_range_it OPTIONAL
        CHANGING
          !ct_source     TYPE tpda_ast_src_it .

  PRIVATE SECTION.

    CLASS-METHODS:
      _get_source
        IMPORTING
          !i_program     TYPE csequence
        EXPORTING
          !et_source     TYPE stringtab
          !et_tokens     TYPE stokesx_tab
          !et_statements TYPE sstmnt_tab .

    TYPES: BEGIN OF ty_scan,
             program    TYPE tpda_program,
             source     TYPE stringtab,
             tokens     TYPE stokesx_tab,
             statements TYPE sstmnt_tab,
           END OF ty_scan,
           tty_scan TYPE HASHED TABLE OF ty_scan
               WITH UNIQUE KEY program.

    CLASS-DATA: scan_buffer      TYPE tty_scan.

ENDCLASS.



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
        popup_title     = TEXT-001
      IMPORTING
        returncode      = returncode
      TABLES
        fields          = fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    CHECK sy-subrc = 0.

    INSERT VALUE #( sign   = 'I'
                    option = 'CP'
                    low    = |*{ to_upper( <filter>-value ) }*| ) INTO TABLE filter.

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

    lcl_source_code_info=>get_source_info_extended(
      EXPORTING
        i_source_info  = source_info
        i_filter       = filter
      CHANGING
        ct_source      = source ).

    IF lines( source ) > 0.
      me->break( ).
    ENDIF.

  ENDMETHOD.                    "script

  METHOD end.
  ENDMETHOD.                    "end

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION

CLASS lcl_source_code_info IMPLEMENTATION.

  METHOD get_source_info_extended.

    DATA: include_source TYPE stringtab,
          tokens         TYPE stokesx_tab,
          statements     TYPE sstmnt_tab,
          source_trace   LIKE LINE OF ct_source,
          tabix          TYPE syst-tabix,
          line           TYPE i,
          found          TYPE abap_bool.

    FIELD-SYMBOLS: <start_statement> LIKE LINE OF statements,
                   <end_statement>   LIKE LINE OF statements,
                   <source_line>     LIKE LINE OF include_source.

    _get_source(
      EXPORTING
        i_program     = i_source_info-include
      IMPORTING
        et_source     = include_source
        et_tokens     = tokens
        et_statements = statements ).

    " get statement before current line
    WHILE found = abap_false.

      line = i_source_info-line - sy-index.

      IF line <= 0.
        EXIT.
      ENDIF.

      READ TABLE statements ASSIGNING <start_statement>
                            WITH KEY trow = line
                            BINARY SEARCH.
      IF sy-subrc = 0.
        found = abap_true.
        tabix = sy-tabix.
      ENDIF.

    ENDWHILE.

    CHECK <start_statement> IS ASSIGNED.

    ADD 1 TO tabix.

    READ TABLE statements ASSIGNING <end_statement> INDEX tabix.
    CHECK sy-subrc = 0.

    line = <start_statement>-trow.

    DO <end_statement>-trow - <start_statement>-trow TIMES.
      ADD 1 TO line.
      READ TABLE tokens TRANSPORTING NO FIELDS
                        WITH KEY row = line
                        BINARY SEARCH.
      CHECK sy-subrc = 0.

      READ TABLE include_source ASSIGNING <source_line>
                                INDEX line.
      CHECK sy-subrc = 0.

      source_trace-statement    = <source_line>.

      IF i_filter IS NOT INITIAL.
        CHECK <source_line> IN i_filter.
      ENDIF.
      source_trace-program      = i_source_info-prg_info-program.
      source_trace-include      = i_source_info-prg_info-include.
      source_trace-line         = line.
      INSERT source_trace INTO TABLE ct_source.
    ENDDO.

  ENDMETHOD.


  METHOD _get_source.

    DATA: scan_buffer_line LIKE LINE OF scan_buffer.

    FIELD-SYMBOLS: <scan_buffer_line> LIKE LINE OF scan_buffer.

    READ TABLE scan_buffer ASSIGNING <scan_buffer_line>
                           WITH TABLE KEY program = i_program.
    IF sy-subrc = 0.
      et_source     = <scan_buffer_line>-source.
      et_tokens     = <scan_buffer_line>-tokens.
      et_statements = <scan_buffer_line>-statements.
      RETURN.
    ENDIF.

    scan_buffer_line-program = i_program.

    READ REPORT scan_buffer_line-program INTO scan_buffer_line-source.

    SCAN ABAP-SOURCE scan_buffer_line-source
         TOKENS INTO scan_buffer_line-tokens
         STATEMENTS INTO scan_buffer_line-statements
         WITH ANALYSIS.

    INSERT scan_buffer_line INTO TABLE scan_buffer.

    et_source     = scan_buffer_line-source.
    et_tokens     = scan_buffer_line-tokens.
    et_statements = scan_buffer_line-statements.

  ENDMETHOD.
ENDCLASS.


*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
