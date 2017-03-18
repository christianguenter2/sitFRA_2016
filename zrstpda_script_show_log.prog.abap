*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZRSTPDA_SCRIPT_SHOW_LOG</SCRIPTNAME>
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
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super  .

  PUBLIC SECTION.
    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      _get_name_of_log
        RETURNING
          VALUE(r_name_of_log) TYPE string,

      _map_line_to_log
        IMPORTING
          it_component TYPE abap_compdescr_tab
          i_line       TYPE any
        RETURNING
          VALUE(r_log) TYPE bal_s_msg,
      _display
        IMPORTING
          i_log_handles TYPE bal_t_logh.

    DATA: name_of_log TYPE string.

ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.
  METHOD prologue.
*** generate abap_source (source handler for ABAP)
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.
*** insert your initialization code here
  ENDMETHOD.                    "init

  METHOD script.

    DATA: messages TYPE bal_tt_msg.

    DATA(lo_log) = NEW cl_sbal_logger( i_category    = 'WEBDYNPRO'
                                       i_subcategory = 'RUNTIME' ).

    FIELD-SYMBOLS: <table_clone> TYPE ANY TABLE.

    DATA(name_of_log) = _get_name_of_log( ).

    CHECK name_of_log IS NOT INITIAL.

    TRY.

        DATA(table_descr) = CAST cl_tpda_script_tabledescr( cl_tpda_script_data_descr=>factory( to_upper( name_of_log && '->MT_MESSAGES' ) ) ).

        DATA(struct_descr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'BAL_S_MSG' ) ).

      CATCH cx_tpda_varname INTO DATA(error).

        MESSAGE error TYPE 'I'.

    ENDTRY.

    DATA(table_clone) = table_descr->elem_clone( ).

    ASSIGN table_clone->* TO <table_clone>.

    LOOP AT <table_clone> ASSIGNING FIELD-SYMBOL(<line>).

      _map_line_to_log(
        EXPORTING
          it_component = struct_descr->components
          i_line       = <line>
        RECEIVING
          r_log        = DATA(log) ).

      lo_log->if_logger~add_message(
        EXPORTING
          i_msgid  = log-msgid    " Message Class
          i_msgno  = log-msgno    " Text field
          i_probcl = log-probclass    " Application Log: Message Problem Class
          i_msgty  = log-msgty    " Message Type
          i_msgv1  = log-msgv1    " Message Variable
          i_msgv2  = log-msgv2    " Message Variable
          i_msgv3  = log-msgv3    " Message Variable
          i_msgv4  = log-msgv4 ).   " Message Variable

    ENDLOOP.

    _display( lo_log->if_sbal_logger~get_log_handles( ) ).

  ENDMETHOD.

  METHOD _display.

    DATA display_profile TYPE bal_s_prof.

    CHECK lines( i_log_handles ) > 0.

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = display_profile.

    display_profile-use_grid = abap_true.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = display_profile    " Display Profile
        i_t_log_handle       = i_log_handles
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  "script

  METHOD end.
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here

  ENDMETHOD.                    "end

  METHOD _get_name_of_log.
    TYPES: sval_tab TYPE STANDARD TABLE OF sval
                         WITH NON-UNIQUE EMPTY KEY.

    IF name_of_log IS NOT INITIAL.
      r_name_of_log = name_of_log.
      RETURN.
    ENDIF.

    DATA(fields) = VALUE sval_tab( ( tabname   = 'DD03P'
                                     fieldname = 'FIELDNAME'
                                     field_obl = abap_true
                                     fieldtext = 'Name of log reference variable'
                                     value     = r_name_of_log ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Name of log reference variable'
      TABLES
        fields          = fields    " Tabellenfelder, Werte und Attribute
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    CHECK sy-subrc = 0.

    r_name_of_log = VALUE #( fields[ 1 ]-value OPTIONAL ).

    name_of_log = r_name_of_log.

  ENDMETHOD.



  METHOD _map_line_to_log.

    LOOP AT it_component ASSIGNING FIELD-SYMBOL(<component>).

      CHECK <component>-type_kind <> cl_abap_typedescr=>typekind_struct1
        AND <component>-type_kind <> cl_abap_typedescr=>typekind_struct2.

      ASSIGN COMPONENT <component>-name OF STRUCTURE i_line TO FIELD-SYMBOL(<field>).
      ASSIGN COMPONENT <component>-name OF STRUCTURE r_log TO FIELD-SYMBOL(<destination>).

      <destination> = <field>.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
