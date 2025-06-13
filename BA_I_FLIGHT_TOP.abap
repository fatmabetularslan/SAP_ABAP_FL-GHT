*&---------------------------------------------------------------------*
*& Include          ZBS_I_FLIGHT_CLS
*&---------------------------------------------------------------------*
CLASS cl_flight DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create_alv
        CHANGING
          co_container TYPE REF TO cl_gui_custom_container
          co_grid      TYPE REF TO cl_gui_alv_grid
          cr_table     TYPE REF TO data
          ct_fieldcat  TYPE lvc_t_fcat
          cs_layout    TYPE lvc_s_layo
          cv_structure TYPE dd02l-tabname
          cv_cont_name TYPE char10.
    METHODS:
      split_alv
        CHANGING
          co_container TYPE REF TO cl_gui_custom_container
          "co_grid      TYPE REF TO cl_gui_alv_grid
          cr_table     TYPE REF TO data
          cr_table2    TYPE REF TO data
          ct_fieldcat  TYPE lvc_t_fcat
          cs_layout    TYPE lvc_s_layo
          cv_structure TYPE dd02l-tabname
          cv_grid1_title TYPE char20
          cv_grid2_title TYPE char20
          cv_cont_name TYPE char10.

    METHODS handle_double_click  "bir sat#r#n double t#klanma özelli#i
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        e_row
        e_column
        es_row_no.
ENDCLASS.

CLASS cl_flight IMPLEMENTATION.
  METHOD create_alv.
    IF co_container IS INITIAL.
      CREATE OBJECT co_container
        EXPORTING
          container_name              = cv_cont_name
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.

    ENDIF.
    IF co_grid IS INITIAL.
      CREATE OBJECT co_grid
        EXPORTING
          i_parent = co_container.

      IF ct_fieldcat IS INITIAL AND cv_structure IS NOT INITIAL.
        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            i_structure_name = cv_structure
          CHANGING
            ct_fieldcat      = ct_fieldcat.
      ENDIF.

      FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
      ASSIGN cr_table->* TO <lt_table>.

    ELSE.
      co_grid->refresh_table_display( ).
    ENDIF.

    CALL METHOD co_grid->set_table_for_first_display
      EXPORTING
        is_layout       = cs_layout
      CHANGING
        it_outtab       = <lt_table>
        it_fieldcatalog = ct_fieldcat.
  ENDMETHOD.

  METHOD handle_double_click.
    READ TABLE gt_airports INDEX e_row-index INTO DATA(ls_data).
    PERFORM get_flights USING ls_data-airportcode.
  ENDMETHOD.

  METHOD split_alv.
  DATA: splitter_1  TYPE REF TO cl_gui_splitter_container,
          container_1 TYPE REF TO cl_gui_container,
          container_2 TYPE REF TO cl_gui_container,
          g_container TYPE scrfname VALUE 'CC_ALV'.
    IF co_container IS INITIAL.
      CREATE OBJECT co_container
        EXPORTING
          container_name              = cv_cont_name
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.

    ENDIF.
    IF splitter_1 IS INITIAL.
      CREATE OBJECT splitter_1
        EXPORTING
          parent            = co_container
          rows              = 1
          columns           = 2
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.

      CALL METHOD splitter_1->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = container_1.

      CALL METHOD splitter_1->get_container
        EXPORTING
          row       = 1
          column    = 2
        RECEIVING
          container = container_2.
    ENDIF.

    CREATE OBJECT co_container
      EXPORTING
        container_name              = g_container
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    IF grid1 IS INITIAL.

      CREATE OBJECT grid1
        EXPORTING
          i_parent          = container_1
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

      IF ct_fieldcat IS INITIAL AND cv_structure IS NOT INITIAL.
        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            i_structure_name = cv_structure
          CHANGING
            ct_fieldcat      = ct_fieldcat.
      ENDIF.

      FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
      ASSIGN cr_table->* TO <lt_table>.

    ELSE.
      grid1->refresh_table_display( ).
    ENDIF.

    cs_layout-grid_title = cv_grid1_title.
    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_layout       = cs_layout
      CHANGING
        it_outtab       = <lt_table>
        it_fieldcatalog = ct_fieldcat.

    CREATE OBJECT co_container
      EXPORTING
        container_name              = g_container
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    IF grid2 IS INITIAL.

      CREATE OBJECT grid2
        EXPORTING
          i_parent          = container_2
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

      IF ct_fieldcat IS INITIAL AND cv_structure IS NOT INITIAL.
        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            i_structure_name = cv_structure
          CHANGING
            ct_fieldcat      = ct_fieldcat.
      ENDIF.

      FIELD-SYMBOLS: <lt_table2> TYPE STANDARD TABLE.
      ASSIGN cr_table2->* TO <lt_table2>.

    ELSE.
      grid2->refresh_table_display( ).
    ENDIF.
    cs_layout-grid_title = cv_grid2_title.
    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout       = cs_layout
      CHANGING
        it_outtab       = <lt_table2>
        it_fieldcatalog = ct_fieldcat.

  ENDMETHOD.
ENDCLASS.
