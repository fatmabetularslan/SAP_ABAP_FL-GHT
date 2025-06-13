*&---------------------------------------------------------------------*
*& Include          ZBS_I_FLIGHT_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .
  SELECT *
    FROM zbs_t_flight AS a
    INNER JOIN zbs_t_seat AS b ON b~flightid = a~flightid AND upper( b~seatstatus ) = 'AVAILABLE'
    WHERE arrivalairport IN @so_arr
    AND departureairport IN @so_dep
    AND departuredate IN @so_gidis
    AND departuredate GT @sy-datum
    INTO CORRESPONDING FIELDS OF TABLE @gt_gidis.
  DELETE ADJACENT DUPLICATES FROM gt_gidis COMPARING flightid.
  IF r_gdonus EQ 'X'.
    SELECT *
      FROM zbs_t_flight AS a
      INNER JOIN zbs_t_seat AS b ON b~flightid = a~flightid AND upper( b~seatstatus ) = 'AVAILABLE'
      WHERE arrivalairport IN @so_dep
      AND departureairport IN @so_arr
      AND departuredate IN @so_donus
      AND departuredate GT @sy-datum
      INTO CORRESPONDING FIELDS OF TABLE @gt_donus.
    DELETE ADJACENT DUPLICATES FROM gt_donus COMPARING flightid.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_fcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_fcat .
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZBS_FLIGHT_STR'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    IF <fs_fcat>-fieldname EQ 'FLIGHTID' OR <fs_fcat>-fieldname EQ 'TOTALSEATS' OR <fs_fcat>-fieldname EQ 'AVAILABLESEATS' OR <fs_fcat>-fieldname EQ 'CHECK_ROW'.
      <fs_fcat>-no_out = abap_true.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'FLIGHTNUM'.
      <fs_fcat>-scrtext_s = |Flight Num|.
      <fs_fcat>-scrtext_m = |Flight Number|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'AIRLINE'.
      <fs_fcat>-scrtext_s = |Airline|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'DEPARTUREAIRPORT'.
      <fs_fcat>-scrtext_s = |Departure|.
      <fs_fcat>-scrtext_l = |Departure Airport|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'ARRIVALAIRPORT'.
      <fs_fcat>-scrtext_s = |Arrival|.
      <fs_fcat>-scrtext_l = |Arrival Airport|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'DEPARTUREDATE'.
      <fs_fcat>-scrtext_s = |Dep. Date|.
      <fs_fcat>-scrtext_l = |Departure Date|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'DEPARTURETIME'.
      <fs_fcat>-scrtext_s = |Dep. Time|.
      <fs_fcat>-scrtext_l = |Departure Time|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'ARRIVALDATE'.
      <fs_fcat>-scrtext_s = |Arr. Date|.
      <fs_fcat>-scrtext_l = |Arrival Date|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'ARRIVALTIME'.
      <fs_fcat>-scrtext_s = |Arr. Time|.
      <fs_fcat>-scrtext_l = |Arrival Time|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'PRICE'.
      <fs_fcat>-scrtext_s = |Price|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'CURRENCY'.
      <fs_fcat>-scrtext_s = |Currency|.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv.
  CLEAR: go_custom_container,go_grid,gr_data,gt_fcat,gt_gidis,gv_cont,gv_st_name.
  PERFORM set_fcat.
  PERFORM get_data.
  gv_cont = 'CC_ALV'.
  gv_st_name = 'ZBS_FLIGHT_STR'.
  CREATE DATA gr_data TYPE TABLE OF zbs_flight_str.
  ASSIGN gr_data->* TO <gt_data>.
  APPEND LINES OF gt_gidis TO <gt_data>.
  CREATE OBJECT go_cl_helper.
  IF r_gidis = 'X'.
    go_cl_helper->create_alv(
        CHANGING
          co_container =  go_custom_container
          co_grid      = go_grid
          cr_table     = gr_data
          ct_fieldcat  = gt_fcat
          cs_layout    = gs_layout
          cv_structure = gv_st_name
          cv_cont_name = gv_cont
      ).
  ELSE.
    IF gt_gidis IS NOT INITIAL.
      DATA: lv_grid1title TYPE char20,
            lv_grid2title TYPE char20.
      CREATE DATA gr_data2 TYPE TABLE OF zbs_flight_str.
      ASSIGN gr_data2->* TO <gt_data2>.
      APPEND LINES OF gt_donus TO <gt_data2>.

      lv_grid1title = 'Departure'.
      lv_grid2title = 'Return'.

      go_cl_helper->split_alv(
        CHANGING
          co_container = go_custom_container
          cr_table     = gr_data
          cr_table2    = gr_data2
          ct_fieldcat  = gt_fcat
          cs_layout    = gs_layout
          cv_structure = gv_st_name
          cv_grid1_title = lv_grid1title
          cv_grid2_title = lv_grid2title
          cv_cont_name = gv_cont
      ).
    ELSE.
      MESSAGE i000(zbs_message) WITH 'Cannot find departure flight!' DISPLAY LIKE 'E'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_airports
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_airports .
  CLEAR: go_custom_container,go_grid,gr_data,gt_fcat,gt_airports,gv_cont,gv_st_name.
  SELECT * FROM zbs_t_airport INTO CORRESPONDING FIELDS OF TABLE @gt_airports.
  PERFORM set_fcat_air.
  gv_cont = 'CC_ALV2'.
  gv_st_name = 'ZBS_AIRPORT_STR'.
  CREATE DATA gr_data TYPE TABLE OF zbs_airport_str.
  ASSIGN gr_data->* TO <gt_data>.
  APPEND LINES OF gt_airports TO <gt_data>.
  CREATE OBJECT go_cl_helper.
  go_cl_helper->create_alv(
    CHANGING
      co_container =  go_custom_container
      co_grid      = go_grid
      cr_table     = gr_data
      ct_fieldcat  = gt_fcat
      cs_layout    = gs_layout
      cv_structure = gv_st_name
      cv_cont_name = gv_cont
  ).
  SET HANDLER go_cl_helper->handle_double_click FOR go_grid.
  CALL SCREEN 0200.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form set_layo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_layo .
  CLEAR: gs_layout.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-no_toolbar = abap_true.
  gs_layout-box_fname = 'CHECK_ROW'.
  gs_layout-sel_mode = 'A'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_fcat_air
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_fcat_air .
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZBS_AIRPORT_STR'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    IF <fs_fcat>-fieldname EQ 'AIRPORTCODE'.
      <fs_fcat>-scrtext_s = |Code|.
      <fs_fcat>-scrtext_m = |Airport Code|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'AIRPORTNAME'.
      <fs_fcat>-scrtext_s = |Airport|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'AIRPORTCITY'.
      <fs_fcat>-scrtext_s = |City|.
      <fs_fcat>-scrtext_l = |Airport City|.
    ENDIF.
    IF <fs_fcat>-fieldname EQ 'AIRPORTCOUNTRY'.
      <fs_fcat>-scrtext_s = |Country|.
      <fs_fcat>-scrtext_l = |Airport Country|.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_flights
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_AIRPORTCODE
*&---------------------------------------------------------------------*
FORM get_flights  USING    p_airportcode.
  CLEAR: go_custom_container,go_grid,gr_data,gt_fcat,gt_gidis,gv_cont,gv_st_name.
  FREE: go_custom_container, go_cl_helper, go_grid.
  SELECT * FROM zbs_t_flight
    INTO CORRESPONDING FIELDS OF TABLE @gt_gidis
    WHERE departureairport EQ @p_airportcode
    AND departuredate GT @sy-datum.
  PERFORM set_fcat.
  gv_cont = 'CC_ALV3'.
  gv_st_name = 'ZBS_FLIGHT_STR'.
  CREATE DATA gr_data TYPE TABLE OF zbs_flight_str.
  ASSIGN gr_data->* TO <gt_data>.
  REFRESH: <gt_data>.
  APPEND LINES OF gt_gidis TO <gt_data>.
  FREE go_cl_helper.
  CREATE OBJECT go_cl_helper.
  go_cl_helper->create_alv(
    CHANGING
      co_container =  go_custom_container
      co_grid      = go_grid
      cr_table     = gr_data
      ct_fieldcat  = gt_fcat
      cs_layout    = gs_layout
      cv_structure = gv_st_name
      cv_cont_name = gv_cont
  ).
  CALL  SCREEN 0300.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_selected_rows
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_selected_rows .
  IF r_gidis = 'X'.
    CALL METHOD go_grid->get_selected_rows
      IMPORTING
        et_row_no = gt_sel_rows.
  ELSE.
    CALL METHOD grid1->get_selected_rows
      IMPORTING
        et_row_no = gt_sel_rows.
    CALL METHOD grid2->get_selected_rows
      IMPORTING
        et_row_no = gt_sel_rows2.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form book_flight
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM book_flight USING p_flightid p_way.
  CLEAR: gt_res, gt_user.
  SELECT SINGLE * FROM zbs_t_seat WHERE flightid EQ @p_flightid AND upper( seatstatus ) EQ 'AVAILABLE' INTO @DATA(ls_seat) .
  IF ls_seat IS INITIAL.
    MESSAGE i000(zbs_message) WITH 'No seat available' DISPLAY LIKE 'E'.
  ELSE.
    PERFORM generate_pnr.
    SELECT MAX( userid ) FROM zbs_t_user INTO @DATA(lv_userid).
    lv_userid += 1.
    APPEND VALUE #( userid = lv_userid username = gv_name userlastname = gv_surname usermail = gv_mail userphone = gv_phone userpassno = '' usernation = '' userbirthdate = gv_birthday ) TO gt_user.
    APPEND VALUE #( reservationid = gv_pnr userid = lv_userid flightid = p_flightid seatid = ls_seat-seatid status = 'Booked' createdat = sy-datum seatclass = ls_seat-seatclass ) TO gt_res.
    MODIFY zbs_t_res FROM TABLE gt_res.
    MODIFY zbs_t_user FROM TABLE gt_user.
    IF sy-subrc EQ 0.
      ls_seat-seatstatus = 'Booked'.
      MODIFY zbs_t_seat FROM ls_seat.
      COMMIT WORK.
      MESSAGE i000(zbs_message) WITH 'Booked.' p_way 'PNR Code:' gv_pnr  DISPLAY LIKE 'S'.
      SET SCREEN 0.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000(zbs_message) WITH 'Error' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form generate_pnr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM generate_pnr .
  CALL FUNCTION 'GENERAL_GET_RANDOM_PWD'
    EXPORTING
      number_chars = 6
    IMPORTING
      random_pwd   = gv_pnr.
  TRANSLATE gv_pnr TO UPPER CASE.
  SELECT SINGLE * FROM zbs_t_res INTO @DATA(ls_res) WHERE reservationid EQ @gv_pnr.
  IF ls_res IS NOT INITIAL.
    PERFORM generate_pnr.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_ticket
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_ticket .
  DATA: lv_fname    TYPE  rs38l_fnam,
        ls_out_opt  TYPE  ssfcompop,
        ls_control  TYPE  ssfctrlop,
        ls_job_info TYPE  ssfcrescl.
  SELECT SINGLE airportcity FROM zbs_t_airport INTO @DATA(lv_departure) WHERE airportcode EQ @gv_departure.
  SELECT SINGLE airportcity FROM zbs_t_airport INTO @DATA(lv_arrival) WHERE airportcode EQ @gv_arrival.
  SELECT SINGLE gate, flightid  FROM zbs_t_flight INTO @DATA(lv_gate) WHERE flightnum EQ @gv_flightcode.
  SELECT SINGLE reservationid FROM zbs_t_res INTO @DATA(lv_pnr) WHERE flightid EQ @lv_gate-flightid AND userid EQ @gv_userid.
  gs_sf_data = VALUE #( passenger = gv_passenger departure = lv_departure arrival = lv_arrival deptime = gv_deptime depdate = gv_departuredate flightnum = gv_flightcode
  gate = lv_gate seat = gv_seat pnr = lv_pnr booking_date = sy-datum ).

  SELECT * FROM zbs_t_res INTO CORRESPONDING FIELDS OF TABLE @gt_res WHERE reservationid EQ @lv_pnr.
  SELECT * FROM zbs_t_user INTO CORRESPONDING FIELDS OF TABLE @gt_user WHERE userid EQ @gv_userid.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZBS_SF_FLIGHT'
    IMPORTING
      fm_name            = lv_fname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  ls_out_opt-tddest = 'LP01'.
  ls_control-no_dialog = abap_true.
  ls_control-preview = abap_true.

  CALL FUNCTION lv_fname
    EXPORTING
      control_parameters = ls_control
      output_options     = ls_out_opt
      user_settings      = ''
      gs_sf_data         = gs_sf_data
    IMPORTING
      job_output_info    = ls_job_info
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  PERFORM send_mail.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form send_mail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM send_mail .
  DATA: lv_fname   TYPE  rs38l_fnam,
        ls_out_opt TYPE  ssfcompop,
        ls_control TYPE  ssfctrlop.
  gs_sf_bilet = gs_sf_data.
  READ TABLE gt_user INTO DATA(ls_user) INDEX 1.
  IF sy-subrc EQ 0.
    gs_sf_bilet-birthday = ls_user-userbirthdate.
    gs_sf_bilet-mail = ls_user-usermail.
    gs_sf_bilet-phone = ls_user-userphone.
  ENDIF.
  READ TABLE gt_res INTO DATA(ls_res) INDEX 1.
  IF sy-subrc EQ 0.
    gs_sf_bilet-seatclass = ls_res-seatclass.
    gs_sf_bilet-checkinstatus = ls_res-status.
    SELECT SINGLE * FROM zbs_t_flight INTO @DATA(ls_flight) WHERE flightid EQ @ls_res-flightid.
    IF ls_flight IS NOT INITIAL.
      gs_sf_bilet-airline = ls_flight-airline.
      gs_sf_bilet-arrairportcode = ls_flight-arrivalairport.
      gs_sf_bilet-depairportcode = ls_flight-departureairport.
      gs_sf_bilet-arrdate = ls_flight-arrivaldate.
      gs_sf_bilet-arrtime = ls_flight-arrivaltime.
      SELECT * FROM zbs_t_airport INTO TABLE @DATA(lt_gidis) WHERE airportcode IN ( @gs_sf_bilet-depairportcode, @gs_sf_bilet-arrairportcode ).
      READ TABLE lt_gidis INTO DATA(ls_gidis) WITH KEY airportcode = gs_sf_bilet-depairportcode.
      IF sy-subrc EQ 0.
        gs_sf_bilet-depairport = ls_gidis-airportname.
      ENDIF.
      READ TABLE lt_gidis INTO DATA(ls_varis) WITH KEY airportcode = gs_sf_bilet-arrairportcode.
      IF sy-subrc EQ 0.
        gs_sf_bilet-arrairport = ls_varis-airportname.
      ENDIF.
      SELECT SINGLE * FROM zbs_t_seat INTO @DATA(ls_seat) WHERE flightid EQ @ls_res-flightid AND seatid EQ @ls_res-seatid.
      IF sy-subrc EQ 0.
        gs_sf_bilet-seatclass = ls_seat-seatclass.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZBS_SF_BILET'
    IMPORTING
      fm_name            = lv_fname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  ls_control-no_dialog = abap_true.
  ls_control-getotf = abap_true.
  ls_control-preview = space.

  ls_out_opt-tdnoprev = abap_true.
  ls_out_opt-tddest = 'LP01'.
  ls_out_opt-tdnoprint = abap_true.

  CALL FUNCTION lv_fname
    EXPORTING
      control_parameters = ls_control
      output_options     = ls_out_opt
      user_settings      = ''
      gs_sf_data         = gs_sf_bilet
    IMPORTING
      job_output_info    = gs_job_info
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  PERFORM create_mail.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_mail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_mail .
  DATA: lo_bcs            TYPE REF TO cl_bcs,
        lo_doc_bcs        TYPE REF TO cl_document_bcs,
        lo_recep          TYPE REF TO if_recipient_bcs,
        lo_sapuser_bcs    TYPE REF TO cl_sapuser_bcs,
        lo_cx_bcx         TYPE REF TO cx_bcs,
        lt_binary_content TYPE solix_tab,
        lt_text           TYPE bcsy_text,
        lt_pdf_tab        TYPE TABLE OF tline,
        lt_otf            TYPE TABLE OF itcoo,
        lv_bin_filesize   TYPE so_obj_len,
        lv_sent_to_all    TYPE os_boolean,
        lv_bin_xstr       TYPE xstring,
        lv_string_text    TYPE string,
        lv_email          TYPE adr6-smtp_addr.
  lt_otf = gs_job_info-otfdata.
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = lv_bin_filesize
      bin_file              = lv_bin_xstr
    TABLES
      otf                   = lt_otf
      lines                 = lt_pdf_tab
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = lv_bin_xstr
    TABLES
      binary_tab = lt_binary_content.

  TRY.
      lo_bcs = cl_bcs=>create_persistent( ).
      READ TABLE gt_user INTO DATA(ls_user) INDEX 1.
      IF sy-subrc EQ 0.
        lv_email = ls_user-usermail.
        lv_string_text = |Hi { ls_user-username } { ls_user-userlastname },|.
        CONCATENATE lv_string_text cl_abap_char_utilities=>newline INTO lv_string_text.
        APPEND lv_string_text TO lt_text.
        CLEAR: lv_string_text.

        lv_string_text = |Thank you for booking your flight with us.|.
        CONCATENATE lv_string_text cl_abap_char_utilities=>newline INTO lv_string_text.
        APPEND lv_string_text TO lt_text.
        CLEAR: lv_string_text.

        lv_string_text = |Your trip has been confirmed and flight ticket enclosed.|.
        CONCATENATE lv_string_text cl_abap_char_utilities=>newline INTO lv_string_text.
        APPEND lv_string_text TO lt_text.
        CLEAR: lv_string_text.
      ENDIF.
      lo_doc_bcs = cl_document_bcs=>create_document(
                     i_type          =  'RAW'
                     i_subject       =  'Confirmation'
                     i_text          = lt_text
                   ).
      CALL METHOD lo_doc_bcs->add_attachment
        EXPORTING
          i_attachment_type    = 'PDF'
          i_attachment_subject = 'Confirmation'
          i_attachment_size    = lv_bin_filesize
          i_att_content_hex    = lt_binary_content.

      CALL METHOD lo_bcs->set_document
        EXPORTING
          i_document = lo_doc_bcs.

      lo_recep = cl_cam_address_bcs=>create_internet_address(
                   i_address_string =  lv_email
                 ).

      CALL METHOD lo_bcs->add_recipient
        EXPORTING
          i_recipient = lo_recep.

      CALL METHOD lo_bcs->send
        RECEIVING
          result = lv_sent_to_all.
      IF lv_sent_to_all IS NOT INITIAL.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    CATCH cx_bcs INTO lo_cx_bcx.
      WRITE: 'Exception:', lo_cx_bcx->error_type.
  ENDTRY.
ENDFORM.
