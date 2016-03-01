REPORT z_sitfra_2016.

INCLUDE z_sitfra_2016_.

"        .__  __ _____________________    _____    _______________  ____  ________
"   _____|__|/  |\_   _____/\______   \  /  _  \   \_____  \   _  \/_   |/  _____/
"  /  ___/  \   __\    __)   |       _/ /  /_\  \   /  ____/  /_\  \|   /   __  \
"  \___ \|  ||  | |     \    |    |   \/    |    \ /       \  \_/   \   \  |__\  \
" /____  >__||__| \___  /    |____|_  /\____|__  / \_______ \_____  /___|\_____  /
"      \/             \/            \/         \/          \/     \/           \/
"
"                 10 tips to improve your ABAP debugging workflow
"

CLASS sit_fra_debugging_tips DEFINITION.
  PUBLIC SECTION.
    METHODS:
      start_presentation,
      constructor.

  PRIVATE SECTION.
    METHODS:
      _0__intro,
      _0_5__why_improve_debugging,
      _1__debugger_steps,
      _2__traces,
      _3__layer_aware_debugging,
      _4__auto_variable_tab,
      _5__conditional_watchpoint,
      _6__conditional_breakpoint,
      _7__debugger_scripting,
      _8__abap_in_eclipse,
      _9__automate,
      _10_do_debug_retrospectives,
      _11_educate_yourself,
      _12_the_end.

    METHODS:

      _watchpoint_at_field_symbol,

      _bp_at_select_db_tab,

      _static_select,

      _dynamic_select,

      _select_with_joins,

      _bp_at_pattern_match,

      _inspect_log,

      _test_auto_1
        RETURNING
          VALUE(r_success) TYPE abap_bool,

      _test_auto_2
        RETURNING
          VALUE(r_text) TYPE string,

      who_am_i
        IMPORTING
          name                        TYPE string OPTIONAL
          age                         TYPE int4   OPTIONAL
          company                     TYPE string OPTIONAL
          location                    TYPE string OPTIONAL
          job_title                   TYPE string OPTIONAL
          number_of_sap_inside_tracks TYPE int4   OPTIONAL,

      the_topic_is_about
        IMPORTING
          what TYPE string,

      the_goal_is
        IMPORTING
          that_everyone TYPE string,

      apply_the_scientific_method,

      start_directly_from_debugger
        IMPORTING
          trace TYPE string,

      use_your_keyboard,

      learn_the_shortcuts,

      use_alt_and_letter_shortcuts
        IMPORTING
          when TYPE string,

      context_menu,

      automate_the_debugger_with
        IMPORTING
          with TYPE string,

      you_should_watch
        IMPORTING
          title    TYPE csequence
          subtitle TYPE csequence
          url      TYPE csequence
          cost     TYPE csequence,

      you_should_read
        IMPORTING
          title    TYPE csequence
          subtitle TYPE csequence
          url      TYPE csequence
          cost     TYPE csequence,

      think_about
        IMPORTING
          what_to_think_about TYPE string,

      who_has_learned
        IMPORTING
          what TYPE string,

      thank_you,

      automate
        IMPORTING
          what            TYPE string OPTIONAL
          everything_that TYPE string OPTIONAL
            PREFERRED PARAMETER what,

      but_it_is_very_personal,

      do_some_logging
        RETURNING VALUE(ro_log) TYPE REF TO lcl_log,

      chain
        IMPORTING
          in_1            TYPE csequence
          in_2            TYPE csequence
          in_3            TYPE csequence
        RETURNING
          VALUE(r_result) TYPE string,

      chain_in_1
        RETURNING
          VALUE(r_result) TYPE string,

      chain_in_2
        RETURNING
          VALUE(r_result) TYPE string,

      chain_in_3
        RETURNING
          VALUE(r_result) TYPE string,

      do_complex_stuff
        IMPORTING
          in TYPE i
        EXCEPTIONS
          something_went_wrong,

      do_other_stuff
        IMPORTING
          in TYPE i
        EXCEPTIONS
          something_went_wrong
          fatal_error,

      do_stuff,

      learning_by_doing,

      access_the_sources
        IMPORTING
          url TYPE csequence,

      what_makes_a_good_debugger
        IMPORTING ingredient TYPE string,

      we_focus_on_the_tools
        IMPORTING
          know_your_tools              TYPE abap_bool
          apply_your_tools_efficiently TYPE abap_bool.

    CONSTANTS:
      experience TYPE string VALUE '',
      intuition  TYPE string VALUE '',
      tools      TYPE string VALUE '',
      luck       TYPE string VALUE ''.

    TYPES: BEGIN OF ty_data,
             i TYPE i,
             s TYPE string,
           END OF ty_data,
           tty_data TYPE HASHED TABLE OF ty_data
                         WITH UNIQUE KEY i.

    DATA: itab TYPE tty_data.

    CONSTANTS:

      learns_at_least_one_new_thing TYPE string VALUE '',
      st05                          TYPE string VALUE '',
      se30                          TYPE string VALUE '',
      there_are_no_shortcuts        TYPE string VALUE '',
      autohotkey                    TYPE string VALUE '',
      autoit                        TYPE string VALUE '',
      debugger_scripting            TYPE string VALUE '',
      sap_gui_scripting             TYPE string VALUE '',
      and_other_automation_tools    TYPE string VALUE '',
      debugging                     TYPE string VALUE '',
      and_the_process_of_debugging  TYPE string VALUE '',
      after_a_successful_session    TYPE string VALUE '',
      at_least_one_new_thing        TYPE string VALUE '',
      the_boring_stuff              TYPE string VALUE '',
      breaks_your_flow              TYPE string VALUE ''.

ENDCLASS.



















"        .__  __ _____________________    _____    _______________  ____  ________
"   _____|__|/  |\_   _____/\______   \  /  _  \   \_____  \   _  \/_   |/  _____/
"  /  ___/  \   __\    __)   |       _/ /  /_\  \   /  ____/  /_\  \|   /   __  \
"  \___ \|  ||  | |     \    |    |   \/    |    \ /       \  \_/   \   \  |__\  \
" /____  >__||__| \___  /    |____|_  /\____|__  / \_______ \_____  /___|\_____  /
"      \/             \/            \/         \/          \/     \/           \/
"
"                 10 tips to improve your ABAP debugging workflow
"

CLASS sit_fra_debugging_tips IMPLEMENTATION.
  METHOD start_presentation.

    _0__intro( ).
    _0_5__why_improve_debugging( ).
    _1__debugger_steps( ).
    _2__traces( ).
    _3__layer_aware_debugging( ).
    _4__auto_variable_tab( ).
    _5__conditional_watchpoint( ).
    _6__conditional_breakpoint( ).
    _7__debugger_scripting( ).
    _8__abap_in_eclipse( ).
    _9__automate( ).
    _10_do_debug_retrospectives( ).
    _11_educate_yourself( ).
    _12_the_end( ).

  ENDMETHOD.



























  METHOD _0__intro.

    who_am_i(:
      name = 'Christian Günter'            ),
      age = 29                             ),
      company = 'Hansgrohe SE'             ),
      location = 'Black Forest'            ),
      job_title = 'SAP software developer' ),
      number_of_sap_inside_tracks = 6      ).


    the_topic_is_about(:
       debugging ),
       and_the_process_of_debugging ).


    the_goal_is( that_everyone = learns_at_least_one_new_thing ).

  ENDMETHOD.






















































  METHOD _0_5__why_improve_debugging.


    what_makes_a_good_debugger(:

        experience ),
        intuition ),
        tools ),
        luck ).




    we_focus_on_the_tools(

        know_your_tools              = abap_true

        apply_your_tools_efficiently = abap_true ).










    apply_the_scientific_method( ).


    "
    "         THE SCIENTIFIC METHOD
    "
    "
    "             +-----------+
    "             |Observation|         observe failure (bug)
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
    "             +----+-----+
    "             |root Cause|          the hypothesis can no longer be refined
    "             +----------+


  ENDMETHOD.






















  METHOD _1__debugger_steps.

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




















  METHOD _7__debugger_scripting.

    " Stop when selecting specific DB-Table
    _bp_at_select_db_tab( ).

    _watchpoint_at_field_symbol( ).

    " Stop when matching a specific pattern in the Source-Code line
    _bp_at_pattern_match( ).

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






















  METHOD _bp_at_pattern_match.

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
























  METHOD _3__layer_aware_debugging.



  ENDMETHOD.


























  METHOD _4__auto_variable_tab.

    IF _test_auto_1( ) = abap_true AND
       _test_auto_2( ) = 'Hallo Welt!'.

      cl_demo_output=>display( 'Condition is true' ).

    ENDIF.

    DATA(chain_result) = chain( in_1 = chain_in_1( )
                                in_2 = chain_in_2( )
                                in_3 = chain_in_3( ) ).

  ENDMETHOD.
























  METHOD _test_auto_1.

    r_success = abap_true.

  ENDMETHOD.






















  METHOD _test_auto_2.

    r_text = |Hallo Welt|.

  ENDMETHOD.


















  METHOD _5__conditional_watchpoint.

    do_stuff( ).

  ENDMETHOD.

















  METHOD _6__conditional_breakpoint.

    do_stuff( ).

  ENDMETHOD.



































  METHOD _2__traces.

    start_directly_from_debugger(:

      trace = st05 ),

      trace = se30 ).

  ENDMETHOD.






















  METHOD _8__abap_in_eclipse.

    " Dynamic Logpoints in AiE

  ENDMETHOD.


























  METHOD _9__automate.

    use_your_keyboard( ).

    learn_the_shortcuts( ).

    use_alt_and_letter_shortcuts( when = there_are_no_shortcuts ).

    context_menu( ).

    automate_the_debugger_with(:
      debugger_scripting ),
      sap_gui_scripting ),
      autohotkey ),
      and_other_automation_tools ).

    automate(:

        the_boring_stuff ),

        everything_that = breaks_your_flow ).

    but_it_is_very_personal( ).

  ENDMETHOD.
























  METHOD _10_do_debug_retrospectives.

    think_about(:

        debugging ),

        and_the_process_of_debugging ),

        after_a_successful_session ).

  ENDMETHOD.


























  METHOD _11_educate_yourself.

    you_should_watch(
      title     = 'Software Debugging'
      subtitle = 'Automating the Boring Tasks'
      url       = 'https://www.udacity.com/courses/cs259'
      cost      = 'free' ).


    you_should_read(
      title     = 'Debug it!'
      subtitle  = 'Find, Repair, and Prevent Bugs in Your Code (Pragmatic Programmers)'
      url       = 'https://pragprog.com/book/pbdp/debug-it'
      cost      = '~35 EUR'  ).

    learning_by_doing( ).

    access_the_sources( url = 'https://github.com/christianguenter2/sitFRA_2016' ).

  ENDMETHOD.




















  METHOD _12_the_end.


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

  METHOD apply_the_scientific_method.

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

  METHOD context_menu.

  ENDMETHOD.

  METHOD automate_the_debugger_with.

  ENDMETHOD.

  METHOD think_about.

  ENDMETHOD.

  METHOD you_should_watch.

    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url    = url
      EXCEPTIONS
        OTHERS = 6.

  ENDMETHOD.

  METHOD who_has_learned.

  ENDMETHOD.

  METHOD thank_you.

  ENDMETHOD.

  METHOD automate.

  ENDMETHOD.

  METHOD but_it_is_very_personal.

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

  METHOD you_should_read.

    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url    = url
      EXCEPTIONS
        OTHERS = 6.
  ENDMETHOD.


  METHOD chain.

    r_result = |{ in_1 } { in_2 } { in_3 }|.

  ENDMETHOD.


  METHOD chain_in_1.

    r_result = |sitFRA 2016|.

  ENDMETHOD.


  METHOD chain_in_2.

    r_result = |is really|.

  ENDMETHOD.


  METHOD chain_in_3.

    r_result = |cool!|.

  ENDMETHOD.


  METHOD do_complex_stuff.

    IF in MOD 33 = 0.

      RAISE something_went_wrong.

    ENDIF.

  ENDMETHOD.


  METHOD do_other_stuff.

    IF in MOD 55 = 0.

      RAISE fatal_error.

    ENDIF.

    IF in MOD 5 = 0.

      RAISE something_went_wrong.

    ENDIF.

  ENDMETHOD.


  METHOD do_stuff.

    DATA(itab) = VALUE int_tab1( FOR n = 1
                                 WHILE n <= 100
                                 ( n ) ).

    LOOP AT itab ASSIGNING FIELD-SYMBOL(<i>).

      do_complex_stuff(
        EXPORTING
           in = <i>
        EXCEPTIONS
           something_went_wrong = 1 ).

      IF sy-subrc <> 0.

      ENDIF.

      do_other_stuff(
        EXPORTING
            in = <i>
        EXCEPTIONS
            something_went_wrong = 1
            fatal_error          = 2 ).

      IF sy-subrc <> 0.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD learning_by_doing.

  ENDMETHOD.


  METHOD access_the_sources.

    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url    = url
      EXCEPTIONS
        OTHERS = 6.

  ENDMETHOD.

  METHOD we_focus_on_the_tools.

  ENDMETHOD.

  METHOD what_makes_a_good_debugger.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW sit_fra_debugging_tips( )->start_presentation( ).