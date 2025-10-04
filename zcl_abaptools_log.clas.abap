CLASS zcl_abaptools_log DEFINITION
  PUBLIC
  CREATE PRIVATE .

*"* public components of class ZCL_ABAPTOOLS_LOG
*"* do not include other source files here!!!
  PUBLIC SECTION.

    CLASS-DATA message TYPE string .
    DATA log_hndl TYPE balloghndl READ-ONLY .
    CONSTANTS probclass_very_high TYPE bal_s_msg-probclass VALUE '1' ##NO_TEXT.
    CONSTANTS probclass_high TYPE bal_s_msg-probclass VALUE '2' ##NO_TEXT.
    CONSTANTS probclass_medium TYPE bal_s_msg-probclass VALUE '3' ##NO_TEXT.
    CONSTANTS probclass_low TYPE bal_s_msg-probclass VALUE '4' ##NO_TEXT.
    CONSTANTS probclass_none TYPE bal_s_msg-probclass VALUE ' ' ##NO_TEXT.
    CLASS-DATA o_current TYPE REF TO zcl_abaptools_log .
    DATA log_nr TYPE balognr READ-ONLY .

    CLASS-METHODS create
      IMPORTING
        !id_object         TYPE balobj_d OPTIONAL
        !id_subobject      TYPE balsubobj OPTIONAL
        !id_extnumber      TYPE c
      RETURNING
        VALUE(ro_appl_log) TYPE REF TO zcl_abaptools_log .
    CLASS-METHODS discard_all .
    CLASS-METHODS display_all .
    CLASS-METHODS get_log_handles
      RETURNING
        VALUE(rt_log_handles) TYPE bal_t_logh .
    CLASS-METHODS save_all .
    METHODS add
      IMPORTING
        !is_msg TYPE bal_s_msg .
    METHODS add_bapiret1
      IMPORTING
        !is_msg TYPE bapiret1 .
    METHODS add_bapiret2
      IMPORTING
        !is_msg TYPE bapiret2 .
    METHODS add_bapi_coru_return
      IMPORTING
        !is_msg TYPE bapi_coru_return .
    METHODS add_bdcmsgcoll
      IMPORTING
        !is_msg TYPE bdcmsgcoll .
    METHODS add_exception
      IMPORTING
        !exception TYPE REF TO cx_root
        !type      TYPE symsgty DEFAULT 'E' .
    METHODS add_free_text
      IMPORTING
        !id_msg   TYPE string
        !id_msgty TYPE symsgty DEFAULT 'I' .
    METHODS add_free_text_error
      IMPORTING
        !id_msg TYPE string .
    METHODS add_free_text_info
      IMPORTING
        !id_msg TYPE string .
    METHODS add_free_text_success
      IMPORTING
        !id_msg TYPE string .
    METHODS add_free_text_warning
      IMPORTING
        !id_msg TYPE string .
    METHODS add_log
      IMPORTING
        !io_log TYPE REF TO zcl_abaptools_log .
    METHODS add_messages
      IMPORTING
        !is_messages TYPE messages .
    METHODS add_symsg .
    METHODS add_t100_exc
      IMPORTING
        !io_exc TYPE REF TO if_t100_message .
    METHODS constructor
      IMPORTING
        !is_log TYPE bal_s_log .
    METHODS discard .
    METHODS display
      IMPORTING
        !as_popup TYPE xfeld OPTIONAL .
    METHODS get_messages_as_text
      IMPORTING
        !i_filter     TYPE string
      RETURNING
        VALUE(r_msgs) TYPE bal_t_msgr .
    METHODS get_msg_handles
      RETURNING
        VALUE(rt_msg) TYPE bal_t_msgh .
    METHODS has_messages
      IMPORTING
        !i_type       TYPE symsgty OPTIONAL
      RETURNING
        VALUE(rd_msg) TYPE abap_bool .
    METHODS save .
  PROTECTED SECTION.
*"* protected components of class ZCL_PI_MIX_APPL_LOG
*"* do not include other source files here!!!
  PRIVATE SECTION.

*"* private components of class ZCL_ABAPTOOLS_LOG
*"* do not include other source files here!!!
    TYPES
     : gyt_inst    TYPE TABLE OF REF TO zcl_abaptools_log
     .

    CLASS-DATA t_inst TYPE gyt_inst .
    DATA t_msg TYPE bal_t_msgh .
    DATA saveable TYPE xfeld .

    CLASS-METHODS get_probclass
      IMPORTING
        !id_msgty           TYPE symsgty
      RETURNING
        VALUE(rd_probclass) TYPE balprobcl .
ENDCLASS.



CLASS ZCL_ABAPTOOLS_LOG IMPLEMENTATION.


  METHOD add.
    DATA
      : ls_bal_msg  LIKE is_msg
      , ls_handle   TYPE balmsghndl
      .
    ls_bal_msg = is_msg.
    ls_bal_msg-probclass = get_probclass( is_msg-msgty ).
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle   = me->log_hndl
        i_s_msg        = ls_bal_msg
      IMPORTING
        e_s_msg_handle = ls_handle
      EXCEPTIONS
        log_not_found  = 1
        msg_inconsistent = 2
        log_is_full    = 3
        OTHERS         = 4.
    IF sy-subrc = 0.
      INSERT ls_handle INTO TABLE me->t_msg.
    ENDIF.
  ENDMETHOD.


  METHOD add_bapiret1.
    DATA
      : ls_msg    TYPE bal_s_msg
      .
    IF NOT is_msg IS INITIAL.
      ls_msg-msgty = is_msg-type.
      ls_msg-msgid = is_msg-id.
      ls_msg-msgno = is_msg-number.
      ls_msg-msgv1 = is_msg-message_v1.
      ls_msg-msgv2 = is_msg-message_v2.
      ls_msg-msgv3 = is_msg-message_v3.
      ls_msg-msgv4 = is_msg-message_v4.

      me->add( is_msg = ls_msg   ).
    ENDIF.
  ENDMETHOD.


  METHOD add_bapiret2.

    DATA
      : ls_msg    TYPE bal_s_msg
      .
    IF NOT is_msg IS INITIAL.
      ls_msg-msgty = is_msg-type.
      ls_msg-msgid = is_msg-id.
      ls_msg-msgno = is_msg-number.
      ls_msg-msgv1 = is_msg-message_v1.
      ls_msg-msgv2 = is_msg-message_v2.
      ls_msg-msgv3 = is_msg-message_v3.
      ls_msg-msgv4 = is_msg-message_v4.

      me->add( is_msg = ls_msg   ).
    ENDIF.

  ENDMETHOD.


  METHOD add_bapi_coru_return.

    DATA
      : ls_msg    TYPE bal_s_msg
      .
    ls_msg-msgty = is_msg-type.
    ls_msg-msgid = is_msg-id.
    ls_msg-msgno = is_msg-number.
    ls_msg-msgv1 = is_msg-message_v1.
    ls_msg-msgv2 = is_msg-message_v2.
    ls_msg-msgv3 = is_msg-message_v3.
    ls_msg-msgv4 = is_msg-message_v4.

    me->add( is_msg = ls_msg   ).

  ENDMETHOD.


  METHOD add_bdcmsgcoll.

    DATA
           : ls_msg TYPE  bal_s_msg
           .

    ls_msg-msgty = is_msg-msgtyp.
    ls_msg-msgid = is_msg-msgid.
    ls_msg-msgno = is_msg-msgnr.
    ls_msg-msgv1 = is_msg-msgv1.
    ls_msg-msgv2 = is_msg-msgv2.
    ls_msg-msgv3 = is_msg-msgv3.
    ls_msg-msgv4 = is_msg-msgv4.

    me->add( is_msg = ls_msg ).

  ENDMETHOD.


  METHOD add_exception.

    DATA msg TYPE REF TO if_t100_dyn_msg.

    IF exception IS INSTANCE OF if_t100_dyn_msg.
      msg ?= exception.
      IF msg->msgty IS NOT INITIAL.
        MESSAGE ID msg->if_t100_message~t100key-msgid
               TYPE type
             NUMBER msg->if_t100_message~t100key-msgno
               WITH msg->msgv1
                    msg->msgv2
                    msg->msgv3
                    msg->msgv4 INTO DATA(message).
        add_symsg( ).
      ELSE.
        add_free_text( id_msg = exception->get_text( ) id_msgty = type ).
      ENDIF.
    ELSE.
      add_free_text( id_msg = exception->get_text( ) id_msgty = type ).
    ENDIF.



  ENDMETHOD.


  METHOD add_free_text.

    DATA
      : ld_probclass  TYPE balprobcl
      , ls_msg_handle TYPE balmsghndl
      , ld_text       TYPE char200
      .
    ld_text       = id_msg.
    ld_probclass  = get_probclass( id_msgty = id_msgty   ).

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle   = me->log_hndl
        i_msgty        = id_msgty
        i_probclass    = ld_probclass
        i_text         = ld_text
      IMPORTING
        e_s_msg_handle = ls_msg_handle
      EXCEPTIONS
        log_not_found  = 1
        msg_inconsistent = 2
        log_is_full    = 3
        OTHERS         = 4.
    IF sy-subrc = 0.
      APPEND ls_msg_handle TO t_msg.
    ENDIF.

  ENDMETHOD.


  METHOD add_free_text_error.

    add_free_text( id_msg = id_msg  id_msgty = 'E' ).

  ENDMETHOD.


  METHOD add_free_text_info.

    add_free_text( id_msg = id_msg  id_msgty = 'I' ).

  ENDMETHOD.


  METHOD add_free_text_success.

    add_free_text( id_msg = id_msg  id_msgty = 'S' ).

  ENDMETHOD.


  METHOD add_free_text_warning.

    add_free_text( id_msg = id_msg  id_msgty = 'W' ).

  ENDMETHOD.


  METHOD add_log.

    DATA
      : lt_msg    TYPE bal_t_msgh
      , ls_handle TYPE balmsghndl
      , ls_msg    TYPE bal_s_msg
      .
    IF NOT io_log IS BOUND.
      " Kein Log rein -> gleich wieder raus.
      RETURN.
    ENDIF.
    lt_msg = io_log->get_msg_handles( ).

    LOOP AT lt_msg INTO ls_handle.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = ls_handle
        IMPORTING
          e_s_msg        = ls_msg
        EXCEPTIONS
          OTHERS         = 1.

      IF sy-subrc <> 0.
      ELSE.
        me->add( is_msg = ls_msg   ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_messages.
    DATA ls_message TYPE bal_s_msg.
    ls_message-msgty = is_messages-msg_type.
    ls_message-msgid = is_messages-msg_id.
    ls_message-msgno = is_messages-msg_no.
    ls_message-msgv1 = is_messages-msg_v1.
    ls_message-msgv2 = is_messages-msg_v2.
    ls_message-msgv3 = is_messages-msg_v3.
    ls_message-msgv4 = is_messages-msg_v4.
    me->add( is_msg = ls_message ).
  ENDMETHOD.


  METHOD add_symsg.

    DATA
      : ls_msg    TYPE bal_s_msg
      .
    MOVE-CORRESPONDING sy TO ls_msg.

    me->add( is_msg = ls_msg ).

  ENDMETHOD.


  METHOD add_t100_exc.

    DATA
      : ls_msg    TYPE bal_s_msg
      .
    ls_msg-msgty = 'E'.
    ls_msg-msgid = io_exc->t100key-msgid.
    ls_msg-msgno = io_exc->t100key-msgno.
    ls_msg-msgv1 = io_exc->t100key-attr1.
    ls_msg-msgv2 = io_exc->t100key-attr2.
    ls_msg-msgv3 = io_exc->t100key-attr3.
    ls_msg-msgv4 = io_exc->t100key-attr4.

    me->add( is_msg = ls_msg ).

  ENDMETHOD.


  METHOD constructor.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = is_log
      IMPORTING
        e_log_handle = me->log_hndl
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      " Log creation failed - handle the error appropriately
      CLEAR me->log_hndl.
    ENDIF.
    
    IF  is_log-object     IS INITIAL OR
        is_log-subobject  IS INITIAL.
      CLEAR me->saveable.
    ELSE.
      me->saveable = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA
      : ls_log        TYPE        bal_s_log
      .
    ls_log-object = id_object.
    ls_log-subobject  = id_subobject.
    ls_log-extnumber = id_extnumber.
    CREATE OBJECT ro_appl_log
      EXPORTING
        is_log = ls_log.
    APPEND ro_appl_log TO t_inst.

  ENDMETHOD.


  METHOD discard.

    DELETE t_inst WHERE table_line = me.

  ENDMETHOD.


  METHOD discard_all.

    CLEAR t_inst.

  ENDMETHOD.


  METHOD display.

    DATA
      : lt_log_handles    TYPE bal_t_logh
      , ls_profile        TYPE bal_s_prof
      .
    APPEND log_hndl TO lt_log_handles.

    IF as_popup IS NOT INITIAL.
      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = ls_profile.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_profile
        i_t_log_handle      = lt_log_handles
      EXCEPTIONS
        OTHERS              = 0.

  ENDMETHOD.


  METHOD display_all.

    DATA
      : lt_log_handles    TYPE bal_t_logh
      .
    lt_log_handles = get_log_handles( ).

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle = lt_log_handles
      EXCEPTIONS
        OTHERS         = 0.

  ENDMETHOD.


  METHOD get_log_handles.

    DATA
      : lo_log    TYPE REF TO zcl_abaptools_log
      .
    LOOP AT t_inst INTO lo_log.
      INSERT lo_log->log_hndl INTO TABLE rt_log_handles.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_messages_as_text.

    DATA:
      lt_log_handles TYPE bal_t_logh,
      l_log_h        LIKE LINE OF lt_log_handles,
      es_log         TYPE bal_s_log,
      es_statistics  TYPE bal_s_scnt,
      et_msg         TYPE bal_t_msgr,
      et_exc         TYPE bal_t_excr_mass,
      ls_msg         LIKE LINE OF et_msg.


    lt_log_handles = get_log_handles( ).
    LOOP AT lt_log_handles INTO l_log_h.
      CALL FUNCTION 'BAL_LOG_READ'
        EXPORTING
          i_log_handle  = l_log_h    " Anwendungs-Log: Handle eines Protokolls
          i_read_texts  = abap_true
          i_langu       = sy-langu
        IMPORTING
          es_log        = es_log    " Anwendungs-Log: Daten des Protokollkopfes
          es_statistics = es_statistics    " Anwendungs-Log: Statistik: Zähler für Meldungstypen
          et_msg        = et_msg    " Anwendungs-Log: Meldungstabelle
          et_exc        = et_exc    " Anwendungslog: Ausnahmetabelle
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc IS INITIAL.
        LOOP AT et_msg INTO ls_msg WHERE msgty = i_filter.
          INSERT ls_msg INTO TABLE r_msgs.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_msg_handles.
    rt_msg = me->t_msg.
  ENDMETHOD.


  METHOD get_probclass.

    CASE id_msgty.
      WHEN 'X'.
        rd_probclass = probclass_very_high.
      WHEN 'A' OR 'E'.
        rd_probclass = probclass_high.
      WHEN 'W'.
        rd_probclass = probclass_medium.
      WHEN OTHERS.
        rd_probclass = probclass_low.
    ENDCASE.

  ENDMETHOD.


  METHOD has_messages.

    DATA
      : ls_stat_count   TYPE bal_s_scnt
      .
    FIELD-SYMBOLS
      : <l_count>       TYPE i
      .
    CASE i_type.
      WHEN 'E'.
        ASSIGN COMPONENT 'MSG_CNT_E' OF STRUCTURE ls_stat_count
          TO <l_count>.
      WHEN 'A'.
        ASSIGN COMPONENT 'MSG_CNT_A' OF STRUCTURE ls_stat_count
          TO <l_count>.
      WHEN 'W'.
        ASSIGN COMPONENT 'MSG_CNT_W' OF STRUCTURE ls_stat_count
          TO <l_count>.
      WHEN 'I'.
        ASSIGN COMPONENT 'MSG_CNT_I' OF STRUCTURE ls_stat_count
          TO <l_count>.
      WHEN 'S'.
        ASSIGN COMPONENT 'MSG_CNT_S' OF STRUCTURE ls_stat_count
          TO <l_count>.
      WHEN OTHERS.
        ASSIGN COMPONENT 'MSG_CNT_AL' OF STRUCTURE ls_stat_count
          TO <l_count>.
    ENDCASE.

    CALL FUNCTION 'BAL_STATISTICS_LOG_GET'
      EXPORTING
        i_log_handle          = me->log_hndl
      IMPORTING
        e_s_statistics_counts = ls_stat_count
      EXCEPTIONS
        log_not_found         = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      CLEAR rd_msg.
    ELSE.
      IF <l_count> > 0.
        rd_msg = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD save.

    DATA
      : lt_log_handles    TYPE bal_t_logh
      , lt_log_numbers    TYPE bal_t_lgnm
      .
    IF me->saveable = abap_true.
      INSERT me->log_hndl INTO TABLE lt_log_handles.

      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_t_log_handle   = lt_log_handles
        IMPORTING
          e_new_lognumbers = lt_log_numbers
        EXCEPTIONS
          OTHERS           = 0.
      TRY .
          me->log_nr = lt_log_numbers[ 1 ]-lognumber.
        CATCH cx_sy_itab_line_not_found ##no_handler.

      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD save_all.

    DATA
      : lt_log_handles    TYPE bal_t_logh
      .
    lt_log_handles = get_log_handles( ).

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle = lt_log_handles
      EXCEPTIONS
        OTHERS         = 0.

  ENDMETHOD.
ENDCLASS.
