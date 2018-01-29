*<SCRIPT:PERSISTENT>
REPORT zrstpda_script_statement_break.

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
    METHODS:
      prologue REDEFINITION,
      init     REDEFINITION,
      script   REDEFINITION,
      end      REDEFINITION.

  PRIVATE SECTION.

    DATA: m_filter     TYPE string,
          m_returncode TYPE char01,
          m_is_regex   TYPE abap_bool.

ENDCLASS.                    "lcl_debugger_script DEFINITION

CLASS lcl_source_code_info DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_source_info_extended
        IMPORTING
          !i_source_info TYPE tpda_curr_source_pos
          !i_filter      TYPE csequence OPTIONAL
          i_is_regex     TYPE abap_bool OPTIONAL
        CHANGING
          !ct_source     TYPE tpda_ast_src_it .

  PRIVATE SECTION.

    CLASS-METHODS:
      get_source
        IMPORTING
          i_program     TYPE csequence
        EXPORTING
          et_source     TYPE stringtab
          et_tokens     TYPE stokesx_tab
          et_statements TYPE sstmnt_tab ,

      get_start_statement
        IMPORTING
          i_start_source_line TYPE i
          i_statements        TYPE sstmnt_tab
        EXPORTING
          e_start_row         TYPE i
          e_start_index       TYPE i.

    TYPES: BEGIN OF ty_scan,
             program    TYPE tpda_program,
             source     TYPE stringtab,
             tokens     TYPE stokesx_tab,
             statements TYPE sstmnt_tab,
           END OF ty_scan,
           tty_scan TYPE HASHED TABLE OF ty_scan
               WITH UNIQUE KEY program.

    CLASS-DATA: scan_buffer TYPE tty_scan.

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

    INSERT VALUE #( tabname   = 'CAWN'
                    fieldname = 'ATWRT'
                    fieldtext = 'Pattern'(001)
                    field_obl = abap_true )
           INTO TABLE fields
           ASSIGNING FIELD-SYMBOL(<filter>).

    INSERT VALUE #( tabname   = 'DD03L'
                    fieldname = 'KEYFLAG'
                    fieldtext = 'Regex?'(002) )
           INTO TABLE fields
           ASSIGNING FIELD-SYMBOL(<regex>).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = TEXT-001
      IMPORTING
        returncode      = m_returncode
      TABLES
        fields          = fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc <> 0 OR m_returncode IS NOT INITIAL.
      end( ).
    ENDIF.

    CHECK sy-subrc = 0.

    m_filter = <filter>-value.
    m_is_regex = <regex>-value.

  ENDMETHOD.                    "init

  METHOD script.

    DATA: source_info TYPE tpda_curr_source_pos,
          source      TYPE tpda_ast_src_it.

    IF m_filter IS INITIAL.
      end( ).
    ENDIF.

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
        i_filter       = m_filter
        i_is_regex     = m_is_regex
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
          start_tabix    TYPE syst-tabix,
          start_row      TYPE stmnt_trow,
          lt_filter      TYPE tpda_range_it.

    FIELD-SYMBOLS:  <end_statement>   LIKE LINE OF statements.

    get_source(
      EXPORTING
        i_program     = i_source_info-include
      IMPORTING
        et_source     = include_source
        et_tokens     = tokens
        et_statements = statements ).

    get_start_statement(
     EXPORTING
       i_start_source_line = i_source_info-line
       i_statements        = statements
     IMPORTING
       e_start_row         = start_row
       e_start_index       = start_tabix ).

    CHECK start_row IS NOT INITIAL.

    READ TABLE statements ASSIGNING <end_statement> INDEX start_tabix + 1.
    CHECK sy-subrc = 0.

    DATA(whole_statement_source) = REDUCE string( INIT result = ||
                                                  FOR index = start_row + 1
                                                  WHILE index <= <end_statement>-trow
                                                  LET source_line = include_source[ index ]
                                                  IN
                                                  NEXT result = result && source_line ).

    IF i_is_regex = abap_true.

      IF find( val   = whole_statement_source
               regex = i_filter  ) = -1.
        RETURN.
      ENDIF.

    ELSE.

      INSERT VALUE #( sign   = 'I'
                      option = 'CP'
                      low    = |*{ to_upper( i_filter ) }*| ) INTO TABLE lt_filter.
      IF NOT whole_statement_source IN lt_filter.
        RETURN.
      ENDIF.

    ENDIF.

    source_trace-statement    = whole_statement_source.
    source_trace-program      = i_source_info-prg_info-program.
    source_trace-include      = i_source_info-prg_info-include.
    source_trace-line         = i_source_info-line.
    INSERT source_trace INTO TABLE ct_source.

  ENDMETHOD.


  METHOD get_source.

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

  METHOD get_start_statement.

    DATA: found TYPE abap_bool,
          line  TYPE i.

    CLEAR: e_start_index,
           e_start_row.

    " get statement before current line
    WHILE found = abap_false.

      line = i_start_source_line - sy-index.

      IF line <= 0.
        EXIT.
      ENDIF.

      READ TABLE i_statements ASSIGNING FIELD-SYMBOL(<start_statement>)
                              WITH KEY trow = line
                              BINARY SEARCH.
      IF sy-subrc = 0.
        found = abap_true.
        e_start_index = sy-tabix.
      ENDIF.

    ENDWHILE.

    IF <start_statement> IS ASSIGNED.
      e_start_row = <start_statement>-trow.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
