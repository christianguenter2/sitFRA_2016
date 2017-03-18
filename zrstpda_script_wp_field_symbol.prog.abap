*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZRSTPDA_SCRIPT_WP_FIELD_SYMBOL</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
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
    DATA: returncode   TYPE char01,
          field_symbol TYPE string,
          old_value    TYPE tpda_var_value.

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
                    fieldtext = 'Field-Symbol'(001)
                    field_obl = abap_true )
           INTO TABLE fields
           REFERENCE INTO DATA(ref_field_symbol).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = text-001
      IMPORTING
        returncode      = returncode
      TABLES
        fields          = fields    " Table fields, values and attributes
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF returncode <> space OR sy-subrc <> 0.
      me->break( ).
      RETURN.
    ENDIF.

    field_symbol = ref_field_symbol->value.

  ENDMETHOD.                    "init

  METHOD script.

    CHECK field_symbol IS NOT INITIAL.

    TRY.
        DATA(new_value) = cl_tpda_script_data_descr=>get_simple_value( field_symbol ).

        IF new_value <> old_value.
          DATA(text) = |Watchpoint { field_symbol } reached|.
          MESSAGE text TYPE 'S'.
          me->break( ).
        ENDIF.

        old_value = new_value.

      CATCH cx_tpda INTO DATA(error).
    ENDTRY.

  ENDMETHOD.                    "script

  METHOD end.

  ENDMETHOD.                    "end

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
