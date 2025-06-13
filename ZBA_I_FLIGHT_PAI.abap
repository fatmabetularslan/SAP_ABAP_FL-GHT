*&---------------------------------------------------------------------*
*& Include          ZBS_I_FLIGHT_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      SET SCREEN 0.
    WHEN '&AIRPORTS'.
      PERFORM get_airports.
    WHEN '&BOOKFLIGH'.
      PERFORM get_selected_rows.
      DESCRIBE TABLE gt_sel_rows LINES DATA(lv_lines).
      DESCRIBE TABLE gt_sel_rows2 LINES DATA(lv_lines2).
      IF lv_lines > 1 OR lv_lines EQ 0.
        MESSAGE i000(zbs_message) WITH 'Select single row' DISPLAY LIKE 'E'.
        RETURN.
      ELSE.
        DATA(gs_selected_row) = VALUE #( gt_sel_rows[ 1 ] OPTIONAL ).
        READ TABLE gt_gidis INTO DATA(ls_gidis) INDEX gs_selected_row-row_id.
        v_arrival = ls_gidis-arrivalairport.
        v_departure = ls_gidis-departureairport.
        v_currency = ls_gidis-currency.
        v_depdate = ls_gidis-departuredate.
        v_deptime = ls_gidis-departuretime.
        v_price = ls_gidis-price.
        v_flightid = ls_gidis-flightid.
        IF r_gdonus EQ 'X'.
          IF lv_lines2 > 1 OR lv_lines2 EQ 0.
            MESSAGE i000(zbs_message) WITH 'Select single row' DISPLAY LIKE 'E'.
            RETURN.
          ELSE.
            DATA(gs_selected_row2) = VALUE #( gt_sel_rows2[ 1 ] OPTIONAL ).
            READ TABLE gt_donus INTO DATA(ls_donus) INDEX gs_selected_row2-row_id.
            v_flightiddonus = ls_donus-flightid.
          ENDIF.
        ENDIF.
      ENDIF.
      CALL SCREEN 0400 STARTING AT 5 10.
    WHEN '&CHECKIN'.
      CALL SCREEN 0500 STARTING AT 5 10.
    WHEN OTHERS.
      RETURN.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      SET SCREEN 0.
    WHEN OTHERS.
      RETURN.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      SET SCREEN 0.
    WHEN '&AIRPORTS'.
      PERFORM get_airports.
    WHEN '&BOOKFLIGH'.
      PERFORM get_selected_rows.
      DESCRIBE TABLE gt_sel_rows LINES DATA(lv_line).
      DESCRIBE TABLE gt_sel_rows2 LINES DATA(lv_line2).
      IF lv_line > 1 OR lv_line EQ 0.
        MESSAGE i000(zbs_message) WITH 'Select single row' DISPLAY LIKE 'E'.
        RETURN.
      ELSE.
        DATA(gs_s_row) = VALUE #( gt_sel_rows[ 1 ] OPTIONAL ).
        READ TABLE gt_gidis INTO DATA(ls_gidis1) INDEX gs_s_row-row_id.
        v_arrival = ls_gidis1-arrivalairport.
        v_departure = ls_gidis1-departureairport.
        v_currency = ls_gidis1-currency.
        v_depdate = ls_gidis1-departuredate.
        v_deptime = ls_gidis1-departuretime.
        v_price = ls_gidis1-price.
        v_flightid = ls_gidis1-flightid.
        IF r_gdonus EQ 'X'.
          IF lv_line2 > 1 OR lv_line2 EQ 0.
            MESSAGE i000(zbs_message) WITH 'Select single row' DISPLAY LIKE 'E'.
            RETURN.
          ELSE.
            DATA(gs_sel_row2) = VALUE #( gt_sel_rows2[ 1 ] OPTIONAL ).
            READ TABLE gt_donus INTO DATA(ls_donus1) INDEX gs_sel_row2-row_id.
            v_flightiddonus = ls_donus1-flightid.
          ENDIF.
        ENDIF.
        CALL SCREEN 0400 STARTING AT 5 10.
      ENDIF.
    WHEN '&CHECKIN'.
      CALL SCREEN 0500 STARTING AT 5 10.

    WHEN OTHERS.
      RETURN.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      SET SCREEN 0.
    WHEN 'BOOKFLI'.
      PERFORM book_flight USING v_flightid 'Departure'.
      IF r_gdonus EQ 'X'.
        PERFORM book_flight USING v_flightiddonus 'Return'.
      ENDIF.
    WHEN OTHERS.
      RETURN.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      SET SCREEN 0.
      gv_open_gr1 = abap_false.
      v_pnr = ''.
      v_surname = ''.
    WHEN 'CONTINUE'.
      SELECT SINGLE * FROM zbs_t_res INTO @DATA(ls_res) WHERE reservationid EQ @v_pnr.
      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM zbs_t_user WHERE userid EQ @ls_res-userid AND upper( userlastname ) EQ @v_surname INTO @DATA(ls_user).
        IF sy-subrc EQ 0.
          SELECT SINGLE seatnumber FROM zbs_t_seat INTO @DATA(lv_seat) WHERE seatid EQ @ls_res-seatid AND flightid EQ @ls_res-flightid.
          IF lv_seat IS NOT INITIAL.
            gv_passenger = |{ ls_user-username } { ls_user-userlastname }|.
            SELECT SINGLE * FROM zbs_t_flight INTO @DATA(ls_fli) WHERE flightid EQ @ls_res-flightid.
            gv_flightcode = ls_fli-flightnum.
            gv_departure = ls_fli-departureairport.
            gv_arrival = ls_fli-arrivalairport.
            gv_departuredate = ls_fli-departuredate.
            gv_deptime = ls_fli-departuretime.
            gv_seat = lv_seat.
            gv_status = ls_res-status.
            gv_open_gr1 = abap_true.
          ENDIF.
        ELSE.
          MESSAGE i000(zbs_message) WITH 'Surnames do not match' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        MESSAGE i000(zbs_message) WITH 'PNR not found' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'BOOKFLIGHT'.
      TRANSLATE gv_status TO UPPER CASE.
      IF gv_status EQ 'BOOKED'.
        SELECT SINGLE * FROM zbs_t_res WHERE reservationid EQ @v_pnr AND upper( status ) EQ 'BOOKED' INTO @DATA(ls_status).
        IF ls_status IS NOT INITIAL.
          ls_status-status = 'Confirmed'.
          MODIFY zbs_t_res FROM ls_status.
          IF sy-subrc EQ 0.
            COMMIT WORK.
            gv_status = ls_status-status.
            gv_userid = ls_status-userid.
            MESSAGE i000(zbs_message) WITH 'Successfully Checked in' DISPLAY LIKE 'S'.
            PERFORM display_ticket.
          ELSE.
            ROLLBACK WORK.
            MESSAGE i000(zbs_message) WITH 'Cannot confirmed' DISPLAY LIKE 'W'.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE i000(zbs_message) WITH 'Already Checked in' DISPLAY LIKE 'W'.
      ENDIF.
    WHEN OTHERS.
      RETURN.
  ENDCASE.
ENDMODULE.
