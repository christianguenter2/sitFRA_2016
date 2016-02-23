REPORT z_sitfra_2016.

















" For demonstration purposes we extent the standard class
" usually you'll have a custom z-class
CLASS lcl_log DEFINITION INHERITING FROM cl_sbal_logger.

  PUBLIC SECTION.
    METHODS:
      if_logger~add_message REDEFINITION,
      add_text IMPORTING i_text TYPE csequence.


    ALIASES: add_message FOR if_logger~add_message.

  PRIVATE SECTION.
    DATA: mt_messages TYPE bal_tt_msg.

ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD if_logger~add_message.

    INSERT VALUE #( msgty     = i_msgty
                    msgid     = i_msgid
                    msgno     = i_msgno
                    msgv1     = i_msgv1
                    msgv2     = i_msgv2
                    msgv3     = i_msgv3
                    msgv4     = i_msgv4
                    probclass = i_probcl ) INTO TABLE mt_messages.

    super->if_logger~add_message(
      EXPORTING
        i_msgid  = i_msgid    " Message Class
        i_msgno  = i_msgno    " Text field
        i_probcl = i_probcl    " Application Log: Message Problem Class
        i_msgty  = i_msgty    " Message Type
        i_msgv1  = i_msgv1    " Message Variable
        i_msgv2  = i_msgv2    " Message Variable
        i_msgv3  = i_msgv3    " Message Variable
        i_msgv4  = i_msgv4 ).

  ENDMETHOD.

  METHOD add_text.

    add_message( i_msgid  = 'BC'
                 i_msgno  = '701'
                 i_msgty  = 'S'
                 i_msgv1  = i_text ).

  ENDMETHOD.

ENDCLASS.


















CLASS sit_fra_debugging_tips DEFINITION.
  PUBLIC SECTION.
    METHODS: start_presentation,
      constructor.

  PRIVATE SECTION.
    METHODS:
      _0__intro,
      _0_5_why,
      _1__improve_your_workflow,
      _2__debugger_steps,
      _3__traces,
      _4__layer_aware_debugging,
      _5__auto_variable_tab,
      _6__breakpoint_condition,
      _7__watchpoint_condition,
      _8__debugger_scripting,
      _9__abap_in_eclipse,
      _10_automate,
      _11_do_debug_retrospectives,
      _12_educate_yourself,
      _13_the_end.

    METHODS:
      _watchpoint_at_field_symbol,
      _bp_at_select_db_tab,
      _static_select,
      _dynamic_select,
      _select_with_joins,

      _bp_at_matching_pattern,

      _inspect_log,

      _1__test RETURNING VALUE(r_success) TYPE abap_bool,
      _2__test RETURNING VALUE(r_text) TYPE string.

    METHODS:
      who_am_i
        IMPORTING
          name                        TYPE string OPTIONAL
          age                         TYPE int4   OPTIONAL
          company                     TYPE string OPTIONAL
          location                    TYPE string OPTIONAL
          number_of_sap_inside_tracks TYPE int4   OPTIONAL.

    METHODS:
      the_topic_is_about
        IMPORTING
          what TYPE string.

    METHODS:
      the_goal_is
        IMPORTING
          that_everyone TYPE string.

    CONSTANTS: learns_at_least_one_new_thing TYPE string VALUE ''.

    METHODS:
      what_do_we_while_debugging
        IMPORTING
          we_use_the TYPE string.

    CONSTANTS: scientific_method TYPE string VALUE ''.

    METHODS:
      start_directly_from_debugger
        IMPORTING
          trace TYPE string.

    CONSTANTS: st05 TYPE string VALUE '',
               se30 TYPE string VALUE ''.



    CONSTANTS: there_are_no_shortcuts TYPE string VALUE ''.

    METHODS:
      use_your_keyboard,

      learn_the_shortcuts,

      use_alt_and_letter_shortcuts
        IMPORTING
          when TYPE string,
      automate_the_debugger_with
        IMPORTING
          with TYPE string.

    CONSTANTS: autohotkey                 TYPE string VALUE '',
               autoit                     TYPE string VALUE '',
               sap_gui_scripting          TYPE string VALUE '',
               and_other_automation_tools TYPE string VALUE ''.

    METHODS:
      you_should_watch
        IMPORTING
          title     TYPE csequence
          sub_title TYPE csequence
          url       TYPE csequence
          cost      TYPE csequence.

    CONSTANTS:
      debugging                    TYPE string VALUE '',
      and_the_process_of_debugging TYPE string VALUE '',
      after_a_successful_session   TYPE string VALUE ''.

    METHODS:
      think_about
        IMPORTING
          what_to_think_about TYPE string.

    METHODS:
      who_has_learned
        IMPORTING
          what TYPE string.

    CONSTANTS: at_least_one_new_thing TYPE string VALUE ''.

    METHODS: thank_you,

      automate
        IMPORTING
          what            TYPE string OPTIONAL
          everything_that TYPE string OPTIONAL
            PREFERRED PARAMETER what.

    CONSTANTS: the_boring_stuff TYPE string VALUE '',
               breaks_your_flow TYPE string VALUE ''.

    TYPES: BEGIN OF ty_data,
             i TYPE i,
             s TYPE string,
           END OF ty_data,
           tty_data TYPE HASHED TABLE OF ty_data
                         WITH UNIQUE KEY i.

    DATA: itab TYPE tty_data.

    METHODS:
      do_some_logging
        RETURNING VALUE(ro_log) TYPE REF TO lcl_log.
ENDCLASS.



















"        .__  __ _____________________    _____    _______________  ____  ________
"   _____|__|/  |\_   _____/\______   \  /  _  \   \_____  \   _  \/_   |/  _____/
"  /  ___/  \   __\    __)   |       _/ /  /_\  \   /  ____/  /_\  \|   /   __  \
"  \___ \|  ||  | |     \    |    |   \/    |    \ /       \  \_/   \   \  |__\  \
" /____  >__||__| \___  /    |____|_  /\____|__  / \_______ \_____  /___|\_____  /
"      \/             \/            \/         \/          \/     \/           \/
"
"                 10 Tipps to improve your ABAP debugging workflow
"

CLASS sit_fra_debugging_tips IMPLEMENTATION.
  METHOD start_presentation.

    _0__intro( ).
    _1__improve_your_workflow( ).
    _2__debugger_steps( ).
    _3__traces( ).
    _4__layer_aware_debugging( ).
    _5__auto_variable_tab( ).
    _6__breakpoint_condition( ).
    _7__watchpoint_condition( ).
    _8__debugger_scripting( ).
    _9__abap_in_eclipse( ).
    _10_automate( ).
    _11_do_debug_retrospectives( ).
    _12_educate_yourself( ).
    _13_the_end( ).

  ENDMETHOD.



























  METHOD _0__intro.

    who_am_i(:
      name = 'Christian Günter'       ),
      age = 29                        ),
      company = 'Hansgrohe SE'        ),
      location = 'Black Forest'       ),
      number_of_sap_inside_tracks = 6 ).


    the_topic_is_about(:
       debugging ),
       and_the_process_of_debugging ).


    the_goal_is( that_everyone = learns_at_least_one_new_thing ).

  ENDMETHOD.





























  METHOD _0_5_why.

  ENDMETHOD.


























  METHOD _1__improve_your_workflow.

    what_do_we_while_debugging(

        we_use_the = scientific_method ).


    "
    "         THE SCIENTIFIC METHOD
    "
    "
    "             +-----------+
    "             |Observation|         observe failure
    "             +----+------+
    "                  |
    "                  |
    "                  v
    "             +----+-----+
    "     +------>+Hypothesis|          invent hypothesis which is consistent with observation
    "     |       +----+-----+
    "     |            |
    "     |            |
    "     |            v
    "     |       +----+-----+
    "     |       |Prediction|          make a prediction
    "     |       +----+-----+
    "     |            |
    "     |            |
    "     |            v
    "     |       +----+-----+          test the hypothesis by experiments
    "     +-------+Experiment|          refine the hypothesis
    "             +----+-----+          create alternative hypothesis
    "                  |
    "                  |
    "                  v
    "               +--+---+
    "               |Theory|            the hypothesis can no longer be refined
    "               +------+



    " Be aware that you can improve your debug workflow!
    " It's a skill like each other and it can be honed


  ENDMETHOD.






















  METHOD _2__debugger_steps.
    "Stepwise debugging of statements

    IF    matches( regex = '\d{8}'
                   val   = sy-datum )
      AND contains_any_of( val   = |Hello World!|
                           start = sy-abcde )
      AND substring_after( val   = 'This a Test'
                           regex = `a` ) = ' Test'
      AND nmax( val1 = 123
                val2 = '123.1' ) = 123
      AND itab = VALUE tty_data( ( i = 1 s = 'Test'  )
                                 ( i = 2 s = `1234 ` ) ).
      cl_demo_output=>display( 'Condition is true' ).
    ENDIF.

    IF 1 = 1.

    ENDIF.










    TYPES: BEGIN OF simple_data,
             i TYPE i,
             s TYPE string,
           END OF simple_data.

    TYPES: BEGIN OF complex_data,
             i      TYPE i,
             s      TYPE string,
             c      TYPE char01,
             d      TYPE datum,
             simple TYPE simple_data,
           END OF complex_data,
           t_complex_data TYPE HASHED TABLE OF complex_data
                               WITH UNIQUE KEY i.


    DATA(complex_tab) = VALUE t_complex_data(
                          LET x = sy-datum * 2
                              i = REDUCE i( INIT sum = 0
                                            FOR n IN VALUE int_tab1( FOR j = 1 WHILE j < 10 ( j )  )
                                            NEXT sum = sum + 1 )
                          IN ( i = 0 s = |Test| c = 'X' d = x
                               simple = VALUE #( LET z = COND #( WHEN sy-datum < '20160209' THEN 1
                                                                 ELSE 2 )
                                                 IN i = cl_abap_random_int=>create( seed = CONV #( sy-datum )
                                                                                                     min  = 0
                                                                                                     max  = 42 )->get_next( )
                                                    s = z ) )
                             ( i = i ) ).










  ENDMETHOD.




















  METHOD _8__debugger_scripting.

    " Stop when selecting specific DB-Table
    _bp_at_select_db_tab( ).

    _watchpoint_at_field_symbol( ).

    " Stop when matching a specific pattern in the Source-Code line
    _bp_at_matching_pattern( ).

    _inspect_log( ).

  ENDMETHOD.


























  METHOD _watchpoint_at_field_symbol.

    DATA(numbers) = VALUE int_tab1( ( 1 )
                                    ( 1 )
                                    ( 2 )
                                    ( 2 )
                                    ( 2 )
                                    ( 3 ) ).

    FIELD-SYMBOLS: <number> LIKE LINE OF numbers.

    LOOP AT numbers ASSIGNING <number>.

    ENDLOOP.

  ENDMETHOD.





























  METHOD _bp_at_select_db_tab.

    _static_select( ).

    _dynamic_select( ).

    _select_with_joins( ).

  ENDMETHOD.















  METHOD _static_select.

    SELECT * FROM t100 INTO TABLE @DATA(lt_data)
             UP TO 10 ROWS.

  ENDMETHOD.


















  METHOD _dynamic_select.

    DATA: lt_data TYPE STANDARD TABLE OF t100
                       WITH NON-UNIQUE EMPTY KEY.
    DATA(table) = |T100|.

    SELECT * INTO TABLE lt_data
             FROM (table)
             UP TO 10 ROWS.

  ENDMETHOD.

















  METHOD _select_with_joins.

    SELECT * FROM e070
             INNER JOIN e071 ON e070~trkorr = e071~trkorr
             INTO TABLE @DATA(lt_data)
             UP TO 10 ROWS.

  ENDMETHOD.






















  METHOD _bp_at_matching_pattern.

    DATA(booking_code) = '0123'.

    booking_code = '0815'.
    booking_code = '0816'.
    booking_code = '0817'.
    booking_code = '0813'.
    booking_code = '0812'.
    booking_code = '0811'.
    booking_code = '0810'.

  ENDMETHOD.





















  METHOD _inspect_log.

    DATA(log) = do_some_logging( ).

  ENDMETHOD.
























  METHOD _4__layer_aware_debugging.

    DATA: lt_data TYPE STANDARD TABLE OF t100
                       WITH NON-UNIQUE DEFAULT KEY.

    TRY .

        cl_salv_table=>factory(
              IMPORTING
                r_salv_table   = DATA(alv)
              CHANGING
                t_table        = lt_data ).

      CATCH cx_salv_msg INTO DATA(alv_error).

    ENDTRY.

  ENDMETHOD.


























  METHOD _5__auto_variable_tab.

    IF _1__test( ) = abap_true AND
       _2__test( ) = 'Hallo Welt!'.

      cl_demo_output=>display( 'Condition is true' ).

    ENDIF.

  ENDMETHOD.
























  METHOD _1__test.

    r_success = abap_true.

  ENDMETHOD.






















  METHOD _2__test.

    r_text = |Hallo Welt|.

  ENDMETHOD.
























  METHOD _6__breakpoint_condition.

    DATA(itab) = VALUE int_tab1( FOR n = 1
                                 WHILE n <= 100
                                 ( n ) ).

    LOOP AT itab ASSIGNING FIELD-SYMBOL(<i>).
      IF 1 = 1.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.



























  METHOD _7__watchpoint_condition.

    DO 100 TIMES.

    ENDDO.

  ENDMETHOD.





















  METHOD _3__traces.

    start_directly_from_debugger(:

      trace = st05 ),

      trace = se30 ).

  ENDMETHOD.






















  METHOD _9__abap_in_eclipse.
    TYPES: BEGIN OF ty_data,
             i TYPE i,
             s TYPE string,
           END OF ty_data,
           tty_data TYPE STANDARD TABLE OF ty_data WITH NON-UNIQUE DEFAULT KEY.

    DATA(rnd) = cl_abap_random_int=>create( seed = CONV #( sy-uzeit )
                                            min  = 0
                                            max  = 100 ).

    DATA(random_numbers) = VALUE tty_data( FOR n = 0
                                           WHILE n < 10
                                           ( i = rnd->get_next( )
                                             s = 'Test' ) ).

  ENDMETHOD.


























  METHOD _10_automate.

    use_your_keyboard( ).

    learn_the_shortcuts( ).

    use_alt_and_letter_shortcuts( when = there_are_no_shortcuts ).

    automate_the_debugger_with(:
      autohotkey ),
      autoit ),
      sap_gui_scripting ),
      and_other_automation_tools ).

    automate(:

        the_boring_stuff ),

        everything_that = breaks_your_flow ).

  ENDMETHOD.
























  METHOD _11_do_debug_retrospectives.

    think_about(:

        debugging ),

        and_the_process_of_debugging ),

        after_a_successful_session ).

  ENDMETHOD.


























  METHOD _12_educate_yourself.

    you_should_watch(
      title     = 'Software Debugging'
      sub_title = 'Automating the Boring Tasks'
      url       = 'https://www.udacity.com/courses/cs259'
      cost      = 'free' ).

  ENDMETHOD.




















  METHOD _13_the_end.


    "        __  .__                                .___
    "      _/  |_|  |__   ____     ____   ____    __| _/
    "      \   __\  |  \_/ __ \  _/ __ \ /    \  / __ |
    "       |  | |   Y  \  ___/  \  ___/|   |  \/ /_/ |
    "       |__| |___|  /\___  >  \___  >___|  /\____ |
    "                 \/     \/       \/     \/      \/
    "


    who_has_learned( at_least_one_new_thing ).

    thank_you( ).

  ENDMETHOD.






















  METHOD who_am_i.

  ENDMETHOD.

  METHOD the_topic_is_about.

  ENDMETHOD.

  METHOD the_goal_is.

  ENDMETHOD.

  METHOD what_do_we_while_debugging.

  ENDMETHOD.

  METHOD start_directly_from_debugger.
    SELECT * FROM t100
             INTO TABLE @DATA(t100_tab)
             UP TO 10 ROWS.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).
      IF <t100>-sprsl = sy-langu.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD use_your_keyboard.

  ENDMETHOD.

  METHOD learn_the_shortcuts.

  ENDMETHOD.

  METHOD use_alt_and_letter_shortcuts.

  ENDMETHOD.

  METHOD automate_the_debugger_with.

  ENDMETHOD.

  METHOD think_about.

  ENDMETHOD.

  METHOD you_should_watch.

  ENDMETHOD.

  METHOD who_has_learned.

  ENDMETHOD.

  METHOD thank_you.

  ENDMETHOD.

  METHOD automate.

  ENDMETHOD.

  METHOD constructor.
    itab = VALUE #( ( i = 1 s = 'Test' )
                    ( i = 2 s = `1234` ) ).
  ENDMETHOD.

  METHOD do_some_logging.

    ro_log = NEW lcl_log( i_category    = 'sitFRA'
                          i_subcategory = 'Logging' ).

    ro_log->add_text( 'sitFRA' ).
    ro_log->add_text( '2016' ).
    ro_log->add_text( 'rocks!!!' ).
    ro_log->add_text( ';-)' ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW sit_fra_debugging_tips( )->start_presentation( ).